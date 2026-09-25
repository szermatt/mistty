use crate::render;
use alacritty_terminal::{
    Term,
    event::{Event, EventListener},
    grid::{Dimensions, Row},
    index::{Column, Line, Point},
    term::{
        ClipboardType, Config, Osc52,
        cell::{Cell, Flags},
    },
    vte::ansi::{self, Attr, Color, Handler, Processor},
};
use emacs::{Env, IntoLisp, Result, Value};
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

emacs::use_functions! {
    nreverse_func => "nreverse"
    current_kill
    kill_new
}
emacs::use_symbols! {
    pty_write_sym => "pty-write"
    title_sym => "title"
}

/// Size of the scrollback, in lines. There needs to be enough space
/// to keep scrollback between calls to `write_scrollback`, to not lose data.
const SCROLLBACK_SIZE: usize = 100000;

/// Virtual Terminal for MisTTY that keeps its data in memory.
pub struct VTerm {
    inner: Term<EventAccumulator>,
    processor: Processor,
    events: Rc<RefCell<VecDeque<Event>>>,
    start_with_wrapped_line: bool,
    scrollback_enabled: bool,
    render_count: i32,
}

impl VTerm {
    /// Create a new terminal with the given dimensions
    pub fn new(width: usize, height: usize, osc52: Osc52) -> Self {
        let events = Rc::new(RefCell::new(VecDeque::new()));
        let acc = EventAccumulator {
            events: Rc::clone(&events),
        };
        let mut config = Config::default();
        config.scrolling_history = 0; // call enable_scrollback to re-enable
        config.osc52 = osc52;

        let mut inner = Term::new(config, &VTermDimensions::new(width, height, 0), acc);
        let processor = Processor::new();

        // See comment on HandlerProxy
        init_grid(inner.grid_mut());

        Self {
            inner,
            processor,
            events,
            start_with_wrapped_line: false,
            scrollback_enabled: false,
            render_count: 0,
        }
    }

    pub fn inner(&self) -> &Term<EventAccumulator> {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut Term<EventAccumulator> {
        &mut self.inner
    }

    pub fn enable_scrollback(&mut self) {
        if !self.scrollback_enabled {
            self.inner.grid_mut().update_history(SCROLLBACK_SIZE);
            self.scrollback_enabled = true;
        }
    }

    pub fn disable_scrollback(&mut self) {
        if self.scrollback_enabled {
            self.inner_mut().grid_mut().update_history(0);
            self.scrollback_enabled = false;
        }
    }

    pub fn resize(&mut self, width: usize, height: usize) {
        let history_size = self.inner().grid().history_size();
        self.inner_mut()
            .resize(VTermDimensions::new(width, height, history_size));
    }

    /// Clear history, normally after having written scrollback to the
    /// buffer.
    pub fn clear_history(&mut self) {
        let grid = self.inner_mut().grid_mut();
        if grid.topmost_line() == 0 {
            return;
        }
        let wrapped = grid[Line(-1)][grid.last_column()]
            .flags
            .contains(Flags::WRAPLINE);
        grid.clear_history();

        self.start_with_wrapped_line = wrapped;
    }

    /// Check whether the last line cleared by the previous call to
    /// `clear_history` ended within a line that was wrapped.
    pub fn start_with_wrapped_line(&self) -> bool {
        self.start_with_wrapped_line
    }

    /// Return the first line of scrollback, or the top of the screen.
    ///
    /// The top of the screen is always `Line(0)`. If there are lines
    /// currently in the scrollback buffer of the virtual terminal,
    /// these lines have negative number. Lines on the screen have
    /// positive numbers.
    #[inline]
    pub fn topmost_line(&self) -> Line {
        self.inner.grid().topmost_line()
    }

    /// Return the last line available in the virtual terminal, that
    /// corresponds to the bottom of the screen.
    #[inline]
    pub fn bottommost_line(&self) -> Line {
        self.inner.grid().bottommost_line()
    }

    /// Return the current position of the cursor in the terminal.
    #[inline]
    pub fn cursor_point(&self) -> Point {
        self.inner.grid().cursor.point
    }

    /// Return the current position of the cursor in the terminal.
    #[inline]
    pub fn last_column(&self) -> Column {
        self.inner.grid().last_column()
    }

    /// Return the number of characters between two positions on the string.
    ///
    /// start and end must be valid points within the terminal. End
    /// may be just outside the valid range.
    ///
    /// Newlines count as one character, even newlines added for
    /// wrapping count. Empty columns count as one character.
    pub fn count_chars(&self, start: Point, end: Point) -> usize {
        self.apply_cell_counter(start, end, render::cell_char_count)
    }

    /// Return the number of cells between two positions.
    ///
    /// This count ignores clear cells. Each newline count as one.
    pub fn count_cells(&self, start: Point, end: Point) -> usize {
        self.apply_cell_counter(
            start,
            end,
            |c| if render::is_clear(c.flags) { 0 } else { 1 },
        )
    }

    /// Return the number of unwrapped line separating `start` from
    /// `end`.
    ///
    /// This counts the number of newlines not marked as line wrap
    /// between `start` and `end`.
    ///
    /// If `end` < `start` a negative number is returned.}
    pub fn count_unwrapped_lines(&self, start: Line, end: Line) -> usize {
        let grid = self.inner.grid();
        let last_col = grid.last_column();
        let mut count = 0;
        for line in start.0..end.0 {
            if !grid[Line(line)][last_col].flags.contains(Flags::WRAPLINE) {
                count += 1;
            }
        }

        count
    }

    /// Parse terminal data and update internal state
    pub fn process_bytes(&mut self, bytes: &[u8]) {
        self.processor.advance(
            &mut HandlerProxy::new(&mut self.inner, self.scrollback_enabled),
            bytes,
        );
    }

    /// Handle accumulated events using the given `env`.
    ///
    /// The return value is a list of events that should be handled by
    /// the caller in lisp format.
    pub fn handle_events<'a>(&self, env: &'a Env) -> Result<Value<'a>> {
        let mut events = self.events.borrow_mut();
        let mut result = ().into_lisp(env)?;
        if events.is_empty() {
            return Ok(result);
        }

        while let Some(event) = events.pop_front() {
            match event {
                Event::PtyWrite(data) => {
                    result = pty_write(env, result, data)?;
                }
                Event::ColorRequest(index, rgb_to_seq) => {
                    if let Some(named) = render::named_color_for_color_request(index) {
                        if let Some(color) =
                            render::to_emacs_color_rgb(env, Color::Named(named), true)?
                        {
                            result = pty_write(env, result, rgb_to_seq(color))?;
                        }
                    }
                }
                Event::Title(title) => {
                    result = env.cons(env.list((title_sym, title))?, result)?;
                }
                Event::ClipboardStore(clipboard, data) => match clipboard {
                    ClipboardType::Clipboard => {
                        env.call(kill_new, (data,))?;
                    }
                    ClipboardType::Selection => {}
                },
                Event::ClipboardLoad(clipboard, formatter) => match clipboard {
                    ClipboardType::Clipboard => {
                        let data: Option<String> = env
                            .call(current_kill, (0,))
                            .and_then(|v| v.into_rust())
                            .unwrap_or(None);
                        if let Some(data) = data {
                            result = pty_write(env, result, formatter(&data))?;
                        }
                    }
                    ClipboardType::Selection => {}
                },

                _ => {}
            };
        }
        result = env.call(nreverse_func, (result,))?;

        Ok(result)
    }

    /// Return the content of the display as the string within range
    /// [start, end).
    ///
    /// start and end must be valid points within the terminal. End
    /// may be just outside the valid range.
    pub fn display_substring(&self, start: Point, end: Point) -> String {
        self.inner.bounds_to_string(start, end)
    }

    /// Process cells within [start, end) with `counter` and sum it up.
    ///
    /// Newlines count as 1.
    fn apply_cell_counter<F>(&self, start: Point, end: Point, counter: F) -> usize
    where
        F: Fn(&Cell) -> usize,
    {
        if start == end {
            return 0;
        }

        // If end points to the beginning of a line, count the newline just before it.
        let (last_line, add_final_nl) = if end.column.0 == 0 {
            (end.line - 1, true)
        } else {
            (end.line, false)
        };
        let grid = self.inner().grid();
        let last_column = grid.last_column();
        let mut count = 0;
        for line in start.line.0..=last_line.0 {
            let line = Line(line);
            let row = &grid[line];
            let start_col = if line == start.line {
                start.column
            } else {
                count += 1; // last line newline

                Column(0)
            };
            let end_col = if line == end.line {
                end.column
            } else {
                last_column + 1
            };
            count += row[start_col..end_col]
                .iter()
                .map(|c| counter(c))
                .sum::<usize>();
        }
        if add_final_nl {
            count += 1;
        }

        count
    }

    // Compare the render count with a value form Emacs side.
    pub fn check_render_count_tag(&self, tag_value: Value<'_>) -> Result<bool> {
        if !tag_value.is_not_nil() {
            return Ok(false);
        }
        let tag_value: i32 = tag_value.into_rust()?;

        Ok(self.render_count == tag_value)
    }

    // Return a tag that identifies a render operation.
    pub fn inc_render_count(&mut self) -> i32 {
        let mut next = self.render_count + 1;

        // keep value inside minimum integer range supported by Emacs
        if next > 536870911 {
            next = -536870912;
        }

        self.render_count = next;

        next
    }
}

fn pty_write<'a>(env: &'a Env, result: Value<'a>, data: String) -> Result<Value<'a>> {
    env.cons(env.list((pty_write_sym, data))?, result)
}

fn init_grid(grid: &mut alacritty_terminal::Grid<Cell>) {
    grid.cursor.template.flags |= Flags::DIM;
}

/// Simple dimensions for VTerm.
struct VTermDimensions {
    width: usize,
    height: usize,
    history_size: usize,
}

impl VTermDimensions {
    fn new(width: usize, height: usize, history_size: usize) -> Self {
        Self {
            width,
            height,
            history_size,
        }
    }
}

impl Dimensions for VTermDimensions {
    fn total_lines(&self) -> usize {
        self.height + self.history_size
    }

    fn screen_lines(&self) -> usize {
        self.height
    }

    fn columns(&self) -> usize {
        self.width
    }
}

/// Accumulate terminal events and return when needed.
pub struct EventAccumulator {
    events: Rc<RefCell<VecDeque<Event>>>,
}

impl EventListener for EventAccumulator {
    fn send_event(&self, event: Event) {
        self.events.borrow_mut().push_back(event);
    }
}

/// HandlerProxy intercepts calls from vte::ansi to the Term instance.
///
/// ## Flags::DIM Hack
///
/// HandlerProxy intercepts calls to set or clear Flags::DIM, as it is used
/// as signal that a cell has been written to in this code.
///
/// That is, we keep DIM always set in the template so that
/// cells that have been written to have the flag set, whereas
/// cells that have been cleared or skipped over will have this
/// flag cleared. This makes it possible to tell cells to which
/// a space was written from empty cells. HandlerProxy
/// guarantees the DIM flag stays set in the template, even if
/// the application tries to turn it off.
///
/// This does mean that DIM cannot be supported. This would
/// require allocating a separate flag for that.
struct HandlerProxy<'a, T> {
    inner: &'a mut Term<T>,
    scrollback_enabled: bool,
}

impl<'a, T> HandlerProxy<'a, T> {
    fn new(inner: &'a mut Term<T>, scrollback_enabled: bool) -> Self {
        Self {
            inner,
            scrollback_enabled,
        }
    }

    /// Store the scrollback into a vector, so it can later on be recovered.
    fn keep_scrollback(&mut self) -> Vec<Row<Cell>> {
        let history_size = self.inner.grid().history_size();
        let mut history = Vec::with_capacity(history_size);
        if history_size > 0 && self.scrollback_enabled {
            let grid = self.inner.grid_mut();
            for line in grid.topmost_line().0..0 {
                let line = Line(line);
                let row = &mut grid[line];
                let mut copy = Row::new(row.len());
                std::mem::swap(row, &mut copy);
                history.push(copy);
            }
            grid.clear_history();
        }
        history
    }

    /// Put back scrollback saved by `take_scrollback`.
    ///
    /// This assumes an empty screen.
    fn recover_scrollback(&mut self, scrollback: Vec<Row<Cell>>) {
        if scrollback.is_empty() {
            return;
        }
        let grid = self.inner.grid_mut();

        let mut line = Line(0);
        let history_size = scrollback.len();
        for mut row in scrollback {
            std::mem::swap(&mut row, &mut grid[line]);
            line += 1;
        }
        grid.update_history(SCROLLBACK_SIZE);
        // Put written lines into scrollback
        grid.scroll_up(&(Line(0)..grid.bottommost_line()), history_size);
    }
}

impl<'a, T> Handler for HandlerProxy<'a, T>
where
    T: EventListener,
{
    fn terminal_attribute(&mut self, attr: Attr) {
        match attr {
            Attr::Reset => {
                self.inner.terminal_attribute(attr);
                self.inner
                    .grid_mut()
                    .cursor
                    .template
                    .flags
                    .set(Flags::DIM, true);
            }
            Attr::Dim => {}
            Attr::CancelBoldDim => {
                self.inner.terminal_attribute(Attr::CancelBold);
            }
            _ => {
                self.inner.terminal_attribute(attr);
            }
        }
    }

    //=== everything below this point just delegates to inner
    //
    // WARNING: if a new method is added to Handler in a new version
    // of the vte crate, it needs to be delegated here, too.

    fn set_title(&mut self, title: Option<String>) {
        self.inner.set_title(title);
    }

    fn set_cursor_style(&mut self, s: Option<ansi::CursorStyle>) {
        self.inner.set_cursor_style(s);
    }

    fn set_cursor_shape(&mut self, shape: ansi::CursorShape) {
        self.inner.set_cursor_shape(shape);
    }

    fn input(&mut self, c: char) {
        self.inner.input(c);
    }

    fn goto(&mut self, line: i32, col: usize) {
        self.inner.goto(line, col);
    }

    fn goto_line(&mut self, line: i32) {
        self.inner.goto_line(line);
    }

    fn goto_col(&mut self, col: usize) {
        self.inner.goto_col(col);
    }

    fn insert_blank(&mut self, n: usize) {
        self.inner.insert_blank(n);
    }

    fn move_up(&mut self, n: usize) {
        self.inner.move_up(n);
    }

    fn move_down(&mut self, n: usize) {
        self.inner.move_down(n);
    }

    fn identify_terminal(&mut self, intermediate: Option<char>) {
        self.inner.identify_terminal(intermediate);
    }

    fn device_status(&mut self, n: usize) {
        self.inner.device_status(n);
    }

    fn move_forward(&mut self, col: usize) {
        self.inner.move_forward(col);
    }

    fn move_backward(&mut self, col: usize) {
        self.inner.move_backward(col);
    }

    fn move_down_and_cr(&mut self, row: usize) {
        self.inner.move_down_and_cr(row);
    }

    fn move_up_and_cr(&mut self, row: usize) {
        self.inner.move_up_and_cr(row);
    }

    fn put_tab(&mut self, count: u16) {
        self.inner.put_tab(count);
    }

    fn backspace(&mut self) {
        self.inner.backspace();
    }

    fn carriage_return(&mut self) {
        self.inner.carriage_return();
    }

    fn linefeed(&mut self) {
        self.inner.linefeed();
    }

    fn bell(&mut self) {
        self.inner.bell();
    }

    fn substitute(&mut self) {
        self.inner.substitute();
    }

    fn newline(&mut self) {
        self.inner.newline();
    }

    fn set_horizontal_tabstop(&mut self) {
        self.inner.set_horizontal_tabstop();
    }

    fn scroll_up(&mut self, n: usize) {
        self.inner.scroll_up(n);
    }

    fn scroll_down(&mut self, n: usize) {
        self.inner.scroll_down(n);
    }

    fn insert_blank_lines(&mut self, n: usize) {
        self.inner.insert_blank_lines(n);
    }

    fn delete_lines(&mut self, n: usize) {
        self.inner.delete_lines(n);
    }

    fn erase_chars(&mut self, n: usize) {
        self.inner.erase_chars(n);
    }

    fn delete_chars(&mut self, n: usize) {
        self.inner.delete_chars(n);
    }

    fn move_backward_tabs(&mut self, count: u16) {
        self.inner.move_backward_tabs(count);
    }

    fn move_forward_tabs(&mut self, count: u16) {
        self.inner.move_forward_tabs(count);
    }

    fn save_cursor_position(&mut self) {
        self.inner.save_cursor_position();
    }

    fn restore_cursor_position(&mut self) {
        self.inner.restore_cursor_position();
    }

    fn clear_line(&mut self, mode: ansi::LineClearMode) {
        self.inner.clear_line(mode);
    }

    fn clear_screen(&mut self, mode: ansi::ClearMode) {
        match mode {
            ansi::ClearMode::Saved => {
                // Refuse to clear the scrollback as this messes up the
                // buffer. This is handled elisp-side with
                // mistty-allow-clearing-scrollback.
            }
            _ => {
                self.inner.clear_screen(mode);
            }
        }
    }

    fn clear_tabs(&mut self, mode: ansi::TabulationClearMode) {
        self.inner.clear_tabs(mode);
    }

    fn set_tabs(&mut self, interval: u16) {
        self.inner.set_tabs(interval);
    }

    fn reset_state(&mut self) {
        // The scrollback should resist a reset for MisTTY. A reset of
        // the scrollback, if desired, can be done elisp-side with
        // mistty-allow-clearing-scrollback.
        self.inner.clear_screen(ansi::ClearMode::All);
        let scrollback = self.keep_scrollback();

        self.inner.reset_state();
        init_grid(self.inner.grid_mut());
        self.recover_scrollback(scrollback);
    }

    fn reverse_index(&mut self) {
        self.inner.reverse_index();
    }

    fn set_mode(&mut self, mode: ansi::Mode) {
        self.inner.set_mode(mode);
    }

    fn unset_mode(&mut self, mode: ansi::Mode) {
        self.inner.unset_mode(mode);
    }

    fn report_mode(&mut self, mode: ansi::Mode) {
        self.inner.report_mode(mode);
    }

    fn set_private_mode(&mut self, mode: ansi::PrivateMode) {
        self.inner.set_private_mode(mode);
    }

    fn unset_private_mode(&mut self, mode: ansi::PrivateMode) {
        self.inner.unset_private_mode(mode);
    }

    fn report_private_mode(&mut self, mode: ansi::PrivateMode) {
        self.inner.report_private_mode(mode);
    }

    fn set_scrolling_region(&mut self, top: usize, bottom: Option<usize>) {
        self.inner.set_scrolling_region(top, bottom);
    }

    fn set_keypad_application_mode(&mut self) {
        self.inner.set_keypad_application_mode();
    }

    fn unset_keypad_application_mode(&mut self) {
        self.inner.unset_keypad_application_mode();
    }

    fn set_active_charset(&mut self, index: ansi::CharsetIndex) {
        self.inner.set_active_charset(index);
    }

    fn configure_charset(&mut self, index: ansi::CharsetIndex, charset: ansi::StandardCharset) {
        self.inner.configure_charset(index, charset);
    }

    fn set_color(&mut self, index: usize, color: ansi::Rgb) {
        self.inner.set_color(index, color);
    }

    fn dynamic_color_sequence(&mut self, prefix: String, index: usize, terminator: &str) {
        self.inner.dynamic_color_sequence(prefix, index, terminator);
    }

    fn reset_color(&mut self, index: usize) {
        self.inner.reset_color(index);
    }

    fn clipboard_store(&mut self, clipboard: u8, base64: &[u8]) {
        self.inner.clipboard_store(clipboard, base64);
    }

    fn clipboard_load(&mut self, clipboard: u8, terminator: &str) {
        self.inner.clipboard_load(clipboard, terminator);
    }

    fn decaln(&mut self) {
        self.inner.decaln();
    }

    fn push_title(&mut self) {
        self.inner.push_title();
    }

    fn pop_title(&mut self) {
        self.inner.pop_title();
    }

    fn text_area_size_pixels(&mut self) {
        self.inner.text_area_size_pixels();
    }

    fn text_area_size_chars(&mut self) {
        self.inner.text_area_size_chars();
    }

    fn set_hyperlink(&mut self, hyperlink: Option<ansi::Hyperlink>) {
        self.inner.set_hyperlink(hyperlink);
    }

    fn set_mouse_cursor_icon(&mut self, icon: ansi::cursor_icon::CursorIcon) {
        self.inner.set_mouse_cursor_icon(icon);
    }

    fn report_keyboard_mode(&mut self) {
        self.inner.report_keyboard_mode();
    }

    fn push_keyboard_mode(&mut self, mode: ansi::KeyboardModes) {
        self.inner.push_keyboard_mode(mode);
    }

    fn pop_keyboard_modes(&mut self, to_pop: u16) {
        self.inner.pop_keyboard_modes(to_pop);
    }

    fn set_keyboard_mode(
        &mut self,
        mode: ansi::KeyboardModes,
        behavior: ansi::KeyboardModesApplyBehavior,
    ) {
        self.inner.set_keyboard_mode(mode, behavior);
    }

    fn set_modify_other_keys(&mut self, mode: ansi::ModifyOtherKeys) {
        self.inner.set_modify_other_keys(mode);
    }

    fn report_modify_other_keys(&mut self) {
        self.inner.report_modify_other_keys();
    }

    fn set_scp(&mut self, char_path: ansi::ScpCharPath, update_mode: ansi::ScpUpdateMode) {
        self.inner.set_scp(char_path, update_mode);
    }
}
