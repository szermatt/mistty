//! Define [VTerm], a wrapper type for
// [alacritty_terminal::term::Term] and
// [alacritty_terminal::term::Grid].

use crate::gridext::{CellExt, GridExt, RowExt};
use crate::render;
use alacritty_terminal::vte::ansi::{KeyboardModes, KeyboardModesApplyBehavior, ModifyOtherKeys};
use alacritty_terminal::{
    Grid, Term,
    event::{Event, EventListener},
    grid::{Dimensions, Row},
    index::{Column, Line, Point},
    term::{
        ClipboardType, Config, Osc52, TermDamage, TermMode,
        cell::{Cell, Flags},
    },
    vte::ansi::{self, Attr, Color, Handler, NamedPrivateMode, PrivateMode, Processor},
};
use emacs::{Env, Result, Value};
use std::{
    cell::RefCell,
    collections::{BTreeSet, VecDeque},
    rc::Rc,
};

emacs::use_functions! {
    nreverse_func => "nreverse"
    current_kill
    kill_new
}
emacs::use_symbols! {
    pty_write_sym => "pty-write"
    title_sym => "title"
    kkp_sym => "kkp"
}

/// Size of the scrollback, in lines. There needs to be enough space
/// to keep scrollback between calls to `write_scrollback`, to not lose data.
const SCROLLBACK_SIZE: usize = 100000;

/// Virtual Terminal for MisTTY that keeps its data in memory.
pub struct VTerm {
    inner: Term<EventAccumulator>,
    processor: Processor,
    events: Rc<RefCell<VecDeque<Event>>>,
    continues_wrapped_line: bool,
    scrollback_enabled: bool,
    render_count: i32,

    /// Rows of scrollback, taken from the grid. These rows
    /// come before the ones in the current grid, if any.
    scrollback: Vec<Row<Cell>>,

    /// Damage made by functions at the VTerm level that directly
    /// modify the grid. Such changes don't register as part of
    /// [Term::damage].
    ///
    /// Always call [VTerm::damaged_lines] and [VTerm::reset_damage]
    /// instead of the [Term] equivalent.
    extra_damage: BTreeSet<Line>,
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
        config.kitty_keyboard = true;

        let mut inner = Term::new(config, &VTermDimensions::new(width, height, 0), acc);
        let processor = Processor::new();

        // See comment on HandlerProxy
        init_grid(inner.grid_mut());

        Self {
            inner,
            processor,
            events,
            continues_wrapped_line: false,
            scrollback_enabled: false,
            render_count: 0,
            scrollback: vec![],
            extra_damage: BTreeSet::new(),
        }
    }

    /// Current terminal mode.
    pub fn mode(&self) -> &TermMode {
        self.inner.mode()
    }

    /// Read-only access to the terminal grid.
    ///
    /// WARNING: history is incomplete in the grid. To access
    /// scrollback/history, do not access rows below 0 directly but
    /// instead go through functions such as [VTerm::scrollback] that
    /// include both the scrollback in [VTerm] as the scrollback in
    /// the grid.
    pub fn grid(&self) -> &Grid<Cell> {
        self.inner.grid()
    }

    /// Return a modifiable reference to the terminal grid.
    ///
    /// If you modify terminal content, remember to register the
    /// modified lines as extra damage.
    fn grid_mut(&mut self) -> &mut Grid<Cell> {
        self.inner.grid_mut()
    }

    /// Enable scrollback.
    pub fn enable_scrollback(&mut self) {
        if !self.scrollback_enabled {
            self.inner.grid_mut().update_history(SCROLLBACK_SIZE);
            self.scrollback_enabled = true;
        }
    }

    /// Disable scrollback.
    pub fn disable_scrollback(&mut self) {
        if self.scrollback_enabled {
            self.inner.grid_mut().update_history(0);
            self.scrollback.clear();
            self.scrollback_enabled = false;
        }
    }

    /// Change terminal size.
    pub fn resize(&mut self, width: usize, height: usize) {
        let history_size = if self.scrollback_enabled {
            SCROLLBACK_SIZE
        } else {
            0
        };
        self.inner
            .resize(VTermDimensions::new(width, height, history_size));
    }

    /// Clear history, normally after having written scrollback to the
    /// buffer.
    ///
    /// This clears both the scrollback stored in [VTerm] as the
    /// history stored in the grid.
    pub fn clear_scrollback(&mut self) {
        let wrapped = self.scrollback_ends_with_wrapline();
        self.scrollback.clear();
        self.inner.grid_mut().clear_history();

        self.continues_wrapped_line = wrapped;
    }

    /// Check whether the last line cleared by the previous call to
    /// [VTerm::clear_scrollback] ended within a line that was
    /// wrapped, the first line scrollback or, if the scrollback is
    /// empty, on the terminal, continues an existing line.
    pub fn continue_wrapped_line(&self) -> bool {
        self.continues_wrapped_line
    }

    /// Check whether existing scrollback data ends in an incomplete line.
    ///
    /// This takes into account the rows from [Self::scrollback] then
    /// the rows from the grid, before line 0.
    pub fn scrollback_ends_with_wrapline(&self) -> bool {
        self.last_scrollback_row()
            .map(|row| row.is_wrapped())
            .unwrap_or(false)
    }

    /// Store grid history into [VTerm]'s scrollback buffer.
    ///
    /// Call this before making a change that would otherwise lose or
    /// hide grid history, such as reset or swapping to the alt
    /// buffer.
    #[cfg(test)]
    fn move_history(&mut self) {
        move_history(self.inner.grid_mut(), &mut self.scrollback);
    }

    /// Number of terminal lines in the scrollback.
    pub fn scrollback_row_count(&self) -> usize {
        self.scrollback.len() + self.inner.grid().history_size()
    }

    /// Return all scrollback rows.
    ///
    /// This returns the rows from [Self::scrollback] then the rows
    /// from the grid, before line 0. There are
    /// [VTerm::scrollback_row_count] rows.
    pub fn scrollback_rows(&self) -> impl Iterator<Item = &Row<Cell>> {
        self.scrollback
            .iter()
            .chain(self.inner.grid().history_iter().map(|(_, row)| row))
    }

    /// Return the last scrollback line, if any.
    ///
    /// This takes into account the rows from [Self::scrollback] then
    /// the rows from the grid, before line 0.
    fn last_scrollback_row(&self) -> Option<&Row<Cell>> {
        if self.inner.grid().history_size() > 0 {
            Some(&self.inner.grid()[Line(-1)])
        } else {
            self.scrollback.last()
        }
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

    /// Parse terminal data and update internal state.
    pub fn process_bytes(&mut self, bytes: &[u8]) {
        self.processor.advance(
            &mut HandlerProxy::new(&mut self.inner, &mut self.scrollback),
            bytes,
        );
    }

    /// Handle accumulated events using the given `env`.
    ///
    /// The return value is a vector of lisp-formatted events that
    /// should be handled by the caller in lisp format.
    pub fn handle_events<'a>(&self, env: &'a Env) -> Result<Vec<Value<'a>>> {
        let mut events = self.events.borrow_mut();
        let mut lisp_events = vec![];
        if events.is_empty() {
            return Ok(lisp_events);
        }

        while let Some(event) = events.pop_front() {
            match event {
                Event::PtyWrite(data) => {
                    lisp_events.push(pty_write(env, data)?);
                }
                Event::ColorRequest(index, rgb_to_seq) => {
                    if let Some(named) = render::named_color_for_color_request(index) {
                        if let Some(color) =
                            render::to_emacs_color_rgb(env, Color::Named(named), true)?
                        {
                            lisp_events.push(pty_write(env, rgb_to_seq(color))?);
                        }
                    }
                }
                Event::Title(title) => {
                    lisp_events.push(env.list((title_sym, title))?);
                }
                Event::ResetTitle => {
                    lisp_events.push(env.list((title_sym, ""))?);
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
                            lisp_events.push(pty_write(env, formatter(&data))?);
                        }
                    }
                    ClipboardType::Selection => {}
                },
                Event::MouseCursorDirty | Event::CursorBlinkingChange => {}
                Event::TextAreaSizeRequest(_) => {}
                Event::Wakeup | Event::Bell | Event::Exit | Event::ChildExit(_) => {}
            };
        }

        Ok(lisp_events)
    }

    pub fn mode_changes<'a>(&self, env: &'a Env, before: &TermMode) -> Result<Vec<Value<'a>>> {
        let mut reports = vec![];

        let kkp_on = self.mode().intersects(TermMode::KITTY_KEYBOARD_PROTOCOL);
        if kkp_on != before.intersects(TermMode::KITTY_KEYBOARD_PROTOCOL) {
            reports.push(env.list((kkp_sym, kkp_on))?);
        }

        Ok(reports)
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

    /// Clear the given line from the given char to end of line.
    pub fn clear_to_eol(&mut self, line: Line, beg_chars: usize) {
        let mut chars = 0;
        for cell in &mut self.grid_mut()[line] {
            if chars >= beg_chars && cell.c == ' ' {
                cell.flags.set(Flags::DIM, false);
            }
            chars += cell.char_count();
        }
        self.extra_damage.insert(line);
    }

    /// Cleanup after a shell's PROMPT-SP hack.
    pub fn cleanup_prompt_sp(&mut self, line: Line) {
        if line == Line(0) {
            return;
        }

        let grid = self.grid_mut();
        let last_column = grid.last_column();
        let prev_line: Line = line - 1;
        let prev_row = &mut grid[prev_line];
        let mut prev_damaged = false;
        if prev_row.is_wrapped() {
            prev_row[last_column].flags.remove(Flags::WRAPLINE);
            blank_trailing(prev_row);
            prev_damaged = true;
        }
        blank_trailing(&mut grid[line]);
        if prev_damaged {
            self.extra_damage.insert(prev_line);
        }
        self.extra_damage.insert(line);
    }

    /// Return the set of lines to refresh, `None` to refresh the whole screen.
    ///
    /// If there are no changes, return an empty vector.
    ///
    /// This returns the terminal lines modified since last call to [VTerm::reset_damage].
    pub fn damaged_lines(&mut self) -> Option<Vec<Line>> {
        if let TermDamage::Partial(iter) = self.inner.damage() {
            let mut lines: Vec<Line> = iter.map(|d| Line(d.line as i32)).collect();
            lines.extend(self.extra_damage.iter());
            lines.sort_unstable();
            lines.dedup();
            // damage is sorted by line, one damage per line.

            Some(lines)
        } else {
            None
        }
    }

    /// Reset damage, so the next call to [VTerm::damaged_lines]
    /// returns an empty vector.
    pub fn reset_damage(&mut self) {
        self.extra_damage.clear();
        self.inner.reset_damage();
    }
}

fn blank_trailing(row: &mut Row<Cell>) {
    for col in (0..row.len()).rev() {
        let col = Column(col);
        let cell = &mut row[col];
        if cell.c != ' ' {
            break;
        }
        cell.flags.set(Flags::DIM, false);
    }
}

/// Move the history from the current grid into the given scrollback
/// vector.
fn move_history(grid: &mut Grid<Cell>, history: &mut Vec<Row<Cell>>) {
    for line in grid.topmost_line().0..0 {
        let line = Line(line);
        let row = &mut grid[line];
        let mut copy = Row::new(row.len());
        std::mem::swap(row, &mut copy);
        history.push(copy);
    }
    grid.clear_history();
}

/// Generate `(pty-write <value>)`
fn pty_write<'a>(env: &'a Env, data: String) -> Result<Value<'a>> {
    env.list((pty_write_sym, data))
}

/// Set flags on a newly-created or reset grid.
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
    scrollback: &'a mut Vec<Row<Cell>>,
}

impl<'a, T> HandlerProxy<'a, T> {
    fn new(inner: &'a mut Term<T>, scrollback: &'a mut Vec<Row<Cell>>) -> Self {
        Self { inner, scrollback }
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
        move_history(self.inner.grid_mut(), self.scrollback);

        self.inner.reset_state();
        init_grid(self.inner.grid_mut());
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

    fn set_private_mode(&mut self, mode: PrivateMode) {
        match mode {
            PrivateMode::Named(NamedPrivateMode::SwapScreenAndSetRestoreCursor) => {
                if !self.inner.mode().contains(TermMode::ALT_SCREEN) {
                    move_history(self.inner.grid_mut(), self.scrollback);
                }
                self.inner.set_private_mode(mode);
            }
            _ => {
                self.inner.set_private_mode(mode);
            }
        }
    }

    fn unset_private_mode(&mut self, mode: PrivateMode) {
        self.inner.unset_private_mode(mode);
    }

    fn report_private_mode(&mut self, mode: PrivateMode) {
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

    fn set_keyboard_mode(&mut self, mode: KeyboardModes, behavior: KeyboardModesApplyBehavior) {
        // Kitty Keyboard Protocol progressive enhancements are not supported by MisTTY;
        // only DISAMBIGUATE_ESC_CODES.
        //
        // Reporting events and reporting alternative key cannot be supported, as
        // the information just isn't available in Emacs.
        //
        // Reporting all keys is unsupported, because without event types,
        // it's just not worth the complexity.
        self.inner.set_keyboard_mode(
            if mode.is_empty() {
                KeyboardModes::empty()
            } else {
                KeyboardModes::DISAMBIGUATE_ESC_CODES
            },
            behavior,
        );
    }

    fn push_keyboard_mode(&mut self, mode: KeyboardModes) {
        // We only ever "push" the single supported mode. This keeps
        // the stack functional, even though this isn't doing
        // anything useful.
        self.inner.push_keyboard_mode(if mode.is_empty() {
            KeyboardModes::empty()
        } else {
            KeyboardModes::DISAMBIGUATE_ESC_CODES
        });
    }

    fn pop_keyboard_modes(&mut self, to_pop: u16) {
        self.inner.pop_keyboard_modes(to_pop);
    }

    fn set_modify_other_keys(&mut self, _mode: ModifyOtherKeys) {
        // unsupported
        self.inner.set_modify_other_keys(ModifyOtherKeys::Reset);
    }

    fn report_modify_other_keys(&mut self) {
        self.inner.report_modify_other_keys();
    }

    fn set_scp(&mut self, char_path: ansi::ScpCharPath, update_mode: ansi::ScpUpdateMode) {
        self.inner.set_scp(char_path, update_mode);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scrollback_empty() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        assert_eq!(0, term.scrollback_row_count());
        assert_eq!(0, term.scrollback_rows().count());
        assert!(term.last_scrollback_row().is_none());
    }

    #[test]
    fn disabled_scrollback() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.disable_scrollback();

        // fill and scroll terminal
        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        // scrollback remains empty
        assert_eq!(0, term.scrollback_row_count());
        assert_eq!(0, term.scrollback_rows().count());
        assert!(term.last_scrollback_row().is_none());
    }

    #[test]
    fn scrollback_from_grid() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        // fill and scroll terminal
        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        // the first three lines are no included into history
        assert_eq!(3, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(term.last_scrollback_row().expect("last_row")),
            "line 3"
        );
    }

    #[test]
    fn scrollback_after_move() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        // fill and scroll terminal, putting 3 rows into history
        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        term.move_history();

        // grid history is empty
        assert_eq!(0, term.grid().history_size());

        // the three lines are still available in VTerm.scrollback
        assert_eq!(3, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(term.last_scrollback_row().expect("last_row")),
            "line 3"
        );
    }

    #[test]
    fn scrollback_merged() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        // fill and scroll terminal, putting 3 rows into history
        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        term.move_history();

        term.process_bytes(b"line 6\r\nline 7\r\n");
        assert_eq!(2, term.grid().history_size());

        // 3 rows are in VTerm.scrollback, 2 in grid history
        assert_eq!(5, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3", "line 4", "line 5"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(term.last_scrollback_row().expect("last_row")),
            "line 5"
        );
    }

    #[test]
    fn scrollback_append() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        term.move_history();

        term.process_bytes(b"line 6\r\nline 7\r\n");
        assert_eq!(2, term.grid().history_size());

        term.move_history();

        // Everything is now in VTerm::scrollback; make sure it was
        // handled properly.

        assert_eq!(5, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3", "line 4", "line 5"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(term.last_scrollback_row().expect("last_row")),
            "line 5"
        );
    }

    #[test]
    fn clear_scrollback() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        term.move_history();

        term.process_bytes(b"line 6\r\nline 7\r\n");
        assert_eq!(2, term.grid().history_size());

        // this should clear both the scrollback in VTerm::scrollback and the grid history
        term.clear_scrollback();

        assert_eq!(0, term.grid().history_size());
        assert_eq!(0, term.scrollback_row_count());
    }

    #[test]
    fn ignore_clear_scrollback() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(3, term.grid().history_size());

        handler(&mut term).clear_screen(ansi::ClearMode::Saved);
        assert_eq!(3, term.grid().history_size());
    }

    #[test]
    fn reset_fills_scrollback() {
        let mut term = VTerm::new(10, 6, Osc52::Disabled);
        term.enable_scrollback();

        term.process_bytes(b"line 1\r\nline 2\r\nline 3");

        // scrollback is initially empty
        assert_eq!(0, term.scrollback_row_count());
        assert_eq!(0, term.scrollback_rows().count());

        // reset clears the screen (among other things) and store
        // the screen content into scrollback.
        handler(&mut term).reset_state();

        // the three screen lines are now in scrollback
        assert_eq!(3, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(term.last_scrollback_row().expect("last_row")),
            "line 3"
        );

        // the screen is clear
        assert_eq!(0, term.grid().history_size());
        for line in 0..=term.bottommost_line().0 {
            assert!(term.grid()[Line(line)].is_clear());
        }
    }

    #[test]
    fn save_scrollback_before_switching_to_alt_buf() {
        let mut term = VTerm::new(10, 3, Osc52::Disabled);
        term.enable_scrollback();

        term.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(3, term.scrollback_row_count());

        handler(&mut term).set_private_mode(PrivateMode::Named(
            NamedPrivateMode::SwapScreenAndSetRestoreCursor,
        ));

        // scrollback is still available, even after the swap
        assert_eq!(3, term.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            term.scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
    }

    /// --- test utilities

    fn handler<'a>(term: &'a mut VTerm) -> HandlerProxy<'a, EventAccumulator> {
        HandlerProxy::new(&mut term.inner, &mut term.scrollback)
    }

    /// Return a string representation of the row.
    ///
    /// This doesn't support hidden chars or multi-column characters;
    /// it's just good enough for testing.
    fn row_to_string(row: &Row<Cell>) -> String {
        (0..row.len())
            .map(|col| row[Column(col)].c)
            .collect::<String>()
            .trim_end()
            .to_string()
    }
}
