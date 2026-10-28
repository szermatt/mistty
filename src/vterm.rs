//! Define [VTerm], a wrapper type for
// [alacritty_terminal::term::Term] and
// [alacritty_terminal::term::Grid].

use crate::damage::GridDamage;
use crate::gridext::{CellExt, GridExt, RowExt};
use crate::prompt::{PromptChange, PromptDetector};
use crate::render;
use crate::types::Scrolline;
use alacritty_terminal::vte::ansi::{KeyboardModes, KeyboardModesApplyBehavior, ModifyOtherKeys};
use alacritty_terminal::{
    Grid, Term,
    event::{Event, EventListener},
    grid::{Dimensions, Row},
    index::{Line, Point},
    term::{
        ClipboardType, Config, Osc52, TermMode,
        cell::{Cell, Flags},
    },
    vte::ansi::{self, Attr, Color, Handler, NamedPrivateMode, PrivateMode, Processor},
};
use emacs::{Env, IntoLisp, Result, Value};
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

emacs::use_functions! {
    nreverse_func => "nreverse"
    current_kill
    kill_new
}
emacs::use_symbols! {
    bracketed_paste_sym => "bracketed_paste"
    title_sym => "title"
    kkp_sym => "kkp"
    mouse_sym => "mouse"
    click_sym => "click"
    drag_sym => "drag"
    motion_sym => "motion"
    nil_sym => "nil"
    cwd_sym => "cwd"
    fullscreen_sym => "fullscreen"
}

/// Size of the scrollback, in lines. There needs to be enough space
/// to keep scrollback between calls to `write_scrollback`, to not lose data.
const SCROLLBACK_SIZE: usize = 100000;

pub(crate) struct VTermProcessor {
    processor: Processor,
    prompt_detector: PromptDetector,
}

impl VTermProcessor {
    pub(crate) fn new() -> Self {
        Self {
            processor: Processor::new(),
            prompt_detector: PromptDetector::new(),
        }
    }

    /// Collect all prompts detected in previous calls to [process_bytes].
    pub(crate) fn collect_prompt_changes(&mut self) -> Vec<PromptChange> {
        self.prompt_detector.collect()
    }

    /// Update the terminal and processor state from the given teminal
    /// sequence.
    pub(crate) fn process_bytes(&mut self, vterm: &mut VTerm, bytes: &[u8]) {
        let cols = vterm.columns();
        let mut start = 0;
        loop {
            match self.prompt_detector.detect_prompt_sp(cols, &bytes[start..]) {
                Some(breakpoint) => {
                    self.process_bytes_internal(vterm, &bytes[start..breakpoint]);
                    self.prompt_detector.handle_prompt_sp(vterm);
                    start = breakpoint;
                    continue;
                }
                None => {
                    self.process_bytes_internal(vterm, &bytes[start..]);
                    break;
                }
            }
        }
    }

    fn process_bytes_internal(&mut self, vterm: &mut VTerm, bytes: &[u8]) {
        if bytes.len() == 0 {
            return;
        }
        self.processor.advance(
            &mut HandlerProxy::new(vterm, &mut self.prompt_detector),
            bytes,
        );
    }
}

/// Changes reported by
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum VTermChange {
    /// Kitty Keyboard protocol enabled or disabled.
    KittyKeyboard(bool),

    /// Bracketed paste enabled or disabled.
    BracketedPaste(bool),

    /// Fullscreen (alt screen) mode enabled or disabled.
    Fullscreen(bool),

    /// Toggle mouse support
    Mouse(MouseMode),

    /// Set or clear window title.
    Title(Option<String>),

    /// Set or clear working directory.
    ///
    /// The working directory is set as a file: URL
    Cwd(Option<String>),
}

/// Mouse mode reported to the lisp side
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum MouseMode {
    Off,
    Click,
    Drag,
    Motion,
}

impl<'e> IntoLisp<'e> for VTermChange {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        match self {
            VTermChange::KittyKeyboard(on) => env.list((kkp_sym, on)),
            VTermChange::BracketedPaste(on) => env.list((bracketed_paste_sym, on)),
            VTermChange::Fullscreen(on) => env.list((fullscreen_sym, on)),
            VTermChange::Mouse(MouseMode::Motion) => env.list((mouse_sym, motion_sym)),
            VTermChange::Mouse(MouseMode::Drag) => env.list((mouse_sym, drag_sym)),
            VTermChange::Mouse(MouseMode::Click) => env.list((mouse_sym, click_sym)),
            VTermChange::Mouse(MouseMode::Off) => env.list((mouse_sym, nil_sym)),
            VTermChange::Title(None) => env.list((title_sym, nil_sym)),
            VTermChange::Title(Some(title)) => env.list((title_sym, title)),
            VTermChange::Cwd(None) => env.list((cwd_sym, nil_sym)),
            VTermChange::Cwd(Some(url)) => env.list((cwd_sym, url)),
        }
    }
}

/// Virtual Terminal for MisTTY that keeps its data in memory.
pub(crate) struct VTerm {
    inner: Term<EventAccumulator>,
    events: Rc<RefCell<VecDeque<Event>>>,
    continues_wrapped_line: bool,
    scrollback_enabled: bool,
    render_count: i32,

    /// Scrolline that corresponds to the first terminal line
    ///
    /// This is the scrolline of the first line of scrollback, or, if
    /// it is empty, grid history or, if it is empty the terminal.
    ///
    /// This is incremented by `clear_scrollback`.
    base_scrolline: Scrolline,

    /// Rows of scrollback, taken from the grid. These rows
    /// come before the ones in the current grid, if any.
    scrollback: Vec<Row<Cell>>,

    /// Damage made by functions at the VTerm level that directly
    /// modify the grid. Such changes don't register as part of
    /// [Term::damage].
    ///
    /// Always call [VTerm::damaged_lines] and [VTerm::_damage]
    /// instead of the [Term] equivalent.
    extra_damage: GridDamage,

    /// State kept for mouse_encode
    last_cell: Option<(i64, i64)>,

    /// Hostname and path of the working directory, as reported by OSC 7.
    ///
    /// None means that the working directory is unknown or has been
    /// cleared. The string is supposed to be a percent-encoded file
    /// URL; this isn't checked.
    cwd_url: Option<String>,

    /// If true, cwd_url should be reported to the lisp side. This is
    /// set whenever cwd_url is modified.
    cwd_modified: bool,

    /// Terminal mode at the time of the last call to [VTerm::collect_mode_changes].
    last_collected_mode: TermMode,

    /// Title change to be collected. The empty string represents a
    /// cleared title.
    title: Option<String>,
}

impl VTerm {
    /// Create a new terminal with the given dimensions
    pub(crate) fn new(width: usize, height: usize, osc52: Osc52) -> Self {
        let events = Rc::new(RefCell::new(VecDeque::new()));
        let acc = EventAccumulator {
            events: Rc::clone(&events),
        };
        let config = Config {
            scrolling_history: 0, // call enable_scrollback to re-enable
            osc52,
            kitty_keyboard: true,
            ..Default::default()
        };

        let mut inner = Term::new(config, &VTermDimensions::new(width, height, 0), acc);

        // See comment on HandlerProxy
        init_grid(inner.grid_mut());
        let last_collected_mode = *inner.mode();

        Self {
            inner,
            events,
            continues_wrapped_line: false,
            scrollback_enabled: false,
            base_scrolline: Scrolline(0),
            render_count: 0,
            scrollback: vec![],
            extra_damage: GridDamage::new(),
            last_cell: None,
            cwd_url: None,
            cwd_modified: false,
            last_collected_mode,
            title: None,
        }
    }

    /// Current terminal mode.
    #[inline]
    pub(crate) fn mode(&self) -> &TermMode {
        self.inner.mode()
    }

    pub(crate) fn set_private_mode(&mut self, mode: PrivateMode) {
        self.inner.set_private_mode(mode);
    }

    pub(crate) fn unset_private_mode(&mut self, mode: PrivateMode) {
        self.inner.unset_private_mode(mode);
    }

    /// Read-only access to the terminal grid.
    ///
    /// WARNING: history is incomplete in the grid. To access
    /// scrollback/history, do not access rows below 0 directly but
    /// instead go through functions such as [VTerm::scrollback] that
    /// include both the scrollback in [VTerm] as the scrollback in
    /// the grid.
    #[inline]
    pub(crate) fn grid(&self) -> &Grid<Cell> {
        self.inner.grid()
    }

    /// Terminal column count.
    #[inline]
    pub(crate) fn columns(&self) -> usize {
        self.inner.columns()
    }

    /// Terminal line count.
    #[inline]
    pub(crate) fn screen_lines(&self) -> usize {
        self.inner.screen_lines()
    }

    /// Return a modifiable reference to the terminal grid.
    /// modified lines as extra damage.
    #[inline]
    pub(crate) fn grid_mut(&mut self) -> &mut Grid<Cell> {
        self.inner.grid_mut()
    }

    /// Enable scrollback.
    pub(crate) fn enable_scrollback(&mut self) {
        if !self.scrollback_enabled {
            self.inner.grid_mut().update_history(SCROLLBACK_SIZE);
            self.scrollback_enabled = true;
        }
    }

    /// Disable scrollback.
    pub(crate) fn disable_scrollback(&mut self) {
        if self.scrollback_enabled {
            self.inner.grid_mut().update_history(0);
            self.scrollback.clear();
            self.scrollback_enabled = false;
        }
    }

    /// Scrolline that correspond to the first line in the terminal,
    /// including history.
    pub(crate) fn base_scrolline(&self) -> Scrolline {
        self.base_scrolline
    }

    /// Scrolline at the top of the screen, `Line(0)`.
    pub(crate) fn screen_top_scrolline(&self) -> Scrolline {
        self.base_scrolline + self.scrollback_rows().filter(|r| !r.is_wrapped()).count()
    }

    /// Scrolline that correspond to the line the cursor is on.
    pub(crate) fn scrolline_at_cursor(&self) -> Scrolline {
        self.screen_top_scrolline()
            + self
                .grid()
                .count_scrollines(Line(0), self.grid().cursor.point.line)
    }

    /// Return the line that currently corresponds to the given
    /// scrolline.
    ///
    /// The returned line may be part of scrollback, with a negative
    /// index, or part of the screen with an index >= 0.
    ///
    /// If there's no corresponding line on the terminal or in the
    /// history, return None.
    pub(crate) fn terminal_line_for_scrolline(&self, scrolline: Scrolline) -> Option<Line> {
        if scrolline < self.base_scrolline {
            return None;
        }
        let mut current = self.base_scrolline();
        for (line, row) in self.all_rows() {
            if current == scrolline {
                return Some(line);
            }
            if !row.is_wrapped() {
                current += 1
            }
        }

        None
    }

    /// Change terminal size.
    pub(crate) fn resize(&mut self, width: usize, height: usize) {
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
    pub(crate) fn clear_scrollback(&mut self) {
        let wrapped = match self.scrollback_ends_with_wrapline() {
            None => {
                // Scrollback is already empty
                return;
            }
            Some(v) => v,
        };
        let new_base = self.screen_top_scrolline();

        self.scrollback.clear();
        self.inner.grid_mut().clear_history();

        self.continues_wrapped_line = wrapped;
        self.base_scrolline = new_base;
    }

    /// Check whether the last line cleared by the previous call to
    /// [VTerm::clear_scrollback] ended within a line that was
    /// wrapped, the first line scrollback or, if the scrollback is
    /// empty, on the terminal, continues an existing line.
    pub(crate) fn continue_wrapped_line(&self) -> bool {
        self.continues_wrapped_line
    }

    /// Check whether existing scrollback data ends in an incomplete line.
    ///
    /// This takes into account the rows from [Self::scrollback] then
    /// the rows from the grid, before line 0.
    pub(crate) fn scrollback_ends_with_wrapline(&self) -> Option<bool> {
        self.last_scrollback_row().map(|row| row.is_wrapped())
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
    pub(crate) fn scrollback_row_count(&self) -> usize {
        self.scrollback.len() + self.inner.grid().history_size()
    }

    /// Return all scrollback rows.
    ///
    /// This returns the rows from [Self::scrollback] then the rows
    /// from the grid, before line 0. There are
    /// [VTerm::scrollback_row_count] rows.
    pub(crate) fn scrollback_rows(&self) -> impl Iterator<Item = &Row<Cell>> {
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

    /// Return all rows, from scrollback to the end of the screen.
    ///
    /// Note that negative lines may reference lines in scrollback
    /// which might not exist in the grid. Positive lines always
    /// reference existing screen lines.
    fn all_rows(&self) -> impl Iterator<Item = (Line, &Row<Cell>)> {
        let start_line = Line(-(self.scrollback_row_count() as i32));

        self.scrollback_rows()
            .enumerate()
            .map(move |(i, r)| (start_line + (i as i32), r))
            .chain(self.grid().screen_iter())
    }

    /// Return the last line available in the virtual terminal, that
    /// corresponds to the bottom of the screen.
    #[inline]
    pub(crate) fn bottommost_line(&self) -> Line {
        self.inner.grid().bottommost_line()
    }

    /// Return the current position of the cursor in the terminal.
    #[inline]
    pub(crate) fn cursor_point(&self) -> Point {
        self.inner.grid().cursor.point
    }

    /// Process accumulated events and prepare data to send to the PTY.
    ///
    /// Returns response data to be sent to the PTY.
    ///
    /// Some other events, like window Title events are stored for
    /// later and returned as part of [VTerm::collect_events].
    ///
    /// This should be called right after calling 'process_bytes' to
    /// avoid delaying responses.
    pub(crate) fn prepare_responses<'a>(&mut self, env: &'a Env) -> Result<Vec<String>> {
        let mut pty_writes = vec![];
        let mut events = self.events.borrow_mut();
        while let Some(event) = events.pop_front() {
            match event {
                Event::PtyWrite(data) => {
                    pty_writes.push(data);
                }
                Event::ColorRequest(index, rgb_to_seq) => {
                    if let Some(named) = render::named_color_for_color_request(index)
                        && let Some(color) =
                            render::to_emacs_color_rgb(env, Color::Named(named), true)?
                    {
                        pty_writes.push(rgb_to_seq(color));
                    }
                }
                Event::Title(title) => {
                    self.title = Some(title);
                }
                Event::ResetTitle => {
                    self.title = Some(String::new());
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
                            pty_writes.push(formatter(&data));
                        }
                    }
                    ClipboardType::Selection => {}
                },
                Event::MouseCursorDirty | Event::CursorBlinkingChange => {}
                Event::TextAreaSizeRequest(_) => {}
                Event::Wakeup | Event::Bell | Event::Exit | Event::ChildExit(_) => {}
            };
        }

        Ok(pty_writes)
    }

    /// Collect events to send back to the lisp side.
    pub(crate) fn collect_changes<'e>(&mut self) -> Result<Vec<VTermChange>> {
        let mut changes = self.collect_mode_changes();
        if let Some(title) = self.title.take() {
            changes.push(VTermChange::Title(if title.is_empty() {
                None
            } else {
                Some(title)
            }));
        }
        if self.cwd_modified {
            self.cwd_modified = false;
            changes.push(VTermChange::Cwd(self.cwd_url.clone()));
        }

        Ok(changes)
    }

    /// Report changes to [TermMode] since the last call to [VTerm::collect_mode_changes].
    fn collect_mode_changes(&mut self) -> Vec<VTermChange> {
        let mut reports = vec![];
        let mode = *self.mode();
        let before = self.last_collected_mode;
        self.last_collected_mode = mode;

        let kkp_on = mode.intersects(TermMode::KITTY_KEYBOARD_PROTOCOL);
        if kkp_on != before.intersects(TermMode::KITTY_KEYBOARD_PROTOCOL) {
            reports.push(VTermChange::KittyKeyboard(kkp_on));
        }

        let bp_on = mode.intersects(TermMode::BRACKETED_PASTE);
        if bp_on != before.intersects(TermMode::BRACKETED_PASTE) {
            reports.push(VTermChange::BracketedPaste(bp_on));
        }

        let fs_on = mode.intersects(TermMode::ALT_SCREEN);
        if fs_on != before.intersects(TermMode::ALT_SCREEN) {
            reports.push(VTermChange::Fullscreen(fs_on));
        }

        let mouse_flags =
            TermMode::MOUSE_REPORT_CLICK | TermMode::MOUSE_DRAG | TermMode::MOUSE_MOTION;
        let mouse = mode.intersection(mouse_flags);
        if mouse != before.intersection(mouse_flags) {
            reports.push(VTermChange::Mouse(
                if mouse.intersects(TermMode::MOUSE_MOTION) {
                    MouseMode::Motion
                } else if mouse.intersects(TermMode::MOUSE_DRAG) {
                    MouseMode::Drag
                } else if mouse.intersects(TermMode::MOUSE_REPORT_CLICK) {
                    MouseMode::Click
                } else {
                    MouseMode::Off
                },
            ));
        }

        reports
    }

    /// Compare the render count with a value form Emacs side.
    pub(crate) fn check_render_count_tag(&self, tag_value: Value<'_>) -> Result<bool> {
        if !tag_value.is_not_nil() {
            return Ok(false);
        }
        let tag_value: i32 = tag_value.into_rust()?;

        Ok(self.render_count == tag_value)
    }

    /// Return a tag that identifies a render operation.
    pub(crate) fn inc_render_count(&mut self) -> i32 {
        let mut next = self.render_count + 1;

        // keep value inside minimum integer range supported by Emacs
        if next > 536870911 {
            next = -536870912;
        }

        self.render_count = next;

        next
    }

    /// Clear the given line from the given char to end of line.
    pub(crate) fn clear_to_eol(&mut self, line: Line, beg_chars: usize) {
        let mut chars = 0;
        for cell in &mut self.grid_mut()[line] {
            if chars >= beg_chars && cell.c == ' ' {
                cell.flags.set(Flags::DIM, false);
            }
            chars += cell.char_count();
        }
        self.extra_damage.add(line);
    }

    /// Return the set of lines to refresh, `None` to refresh the whole screen.
    ///
    /// If there are no changes, return an empty vector.
    ///
    /// This returns the terminal lines modified since last call to [VTerm::reset_damage].
    pub(crate) fn damaged_lines(&mut self) -> Option<Vec<Line>> {
        self.extra_damage.damaged_lines(self.inner.damage())
    }

    /// Reset damage, so the next call to [VTerm::damaged_lines]
    /// returns an empty vector.
    pub(crate) fn reset_damage(&mut self) {
        self.extra_damage.reset();
        self.inner.reset_damage();
    }

    pub(crate) fn add_damage(&mut self, line: Line) {
        self.extra_damage.add(line);
    }

    /// Return state for mouse_encode
    pub(crate) fn last_cell(&mut self) -> &mut Option<(i64, i64)> {
        &mut self.last_cell
    }

    /// Set the cwd to report to the lisp side
    fn set_cwd(&mut self, cwd: Option<&str>) {
        match cwd {
            None => {
                // clear
                if self.cwd_url.is_some() {
                    self.cwd_url = None;
                    self.cwd_modified = true;
                }
            }
            Some(url) => {
                let url = Some(url.to_string());
                if url != self.cwd_url {
                    self.cwd_url = url;
                    self.cwd_modified = true;
                }
            }
        }
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
pub(crate) struct EventAccumulator {
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
struct HandlerProxy<'a> {
    vterm: &'a mut VTerm,
    prompt_detector: &'a mut PromptDetector,
}

impl<'a> HandlerProxy<'a> {
    fn new(vterm: &'a mut VTerm, prompt_detector: &'a mut PromptDetector) -> Self {
        Self {
            vterm,
            prompt_detector,
        }
    }
}

impl<'a> Handler for HandlerProxy<'a> {
    fn terminal_attribute(&mut self, attr: Attr) {
        match attr {
            Attr::Reset => {
                self.vterm.inner.terminal_attribute(attr);
                self.vterm
                    .grid_mut()
                    .cursor
                    .template
                    .flags
                    .set(Flags::DIM, true);
            }
            Attr::Dim => {}
            Attr::CancelBoldDim => {
                self.vterm.inner.terminal_attribute(Attr::CancelBold);
            }
            _ => {
                self.vterm.inner.terminal_attribute(attr);
            }
        }
    }

    //=== everything below this point just delegates to inner
    //
    // WARNING: if a new method is added to Handler in a new version
    // of the vte crate, it needs to be delegated here, too.

    fn set_title(&mut self, title: Option<String>) {
        self.vterm.inner.set_title(title);
    }

    fn set_cursor_style(&mut self, s: Option<ansi::CursorStyle>) {
        self.vterm.inner.set_cursor_style(s);
    }

    fn set_cursor_shape(&mut self, shape: ansi::CursorShape) {
        self.vterm.inner.set_cursor_shape(shape);
    }

    fn input(&mut self, c: char) {
        self.vterm.inner.input(c);
    }

    fn goto(&mut self, line: i32, col: usize) {
        self.vterm.inner.goto(line, col);
    }

    fn goto_line(&mut self, line: i32) {
        self.vterm.inner.goto_line(line);
    }

    fn goto_col(&mut self, col: usize) {
        self.vterm.inner.goto_col(col);
    }

    fn insert_blank(&mut self, n: usize) {
        self.vterm.inner.insert_blank(n);
    }

    fn move_up(&mut self, n: usize) {
        self.vterm.inner.move_up(n);
    }

    fn move_down(&mut self, n: usize) {
        self.vterm.inner.move_down(n);
    }

    fn identify_terminal(&mut self, intermediate: Option<char>) {
        self.vterm.inner.identify_terminal(intermediate);
    }

    fn device_status(&mut self, n: usize) {
        self.vterm.inner.device_status(n);
    }

    fn move_forward(&mut self, col: usize) {
        self.vterm.inner.move_forward(col);
    }

    fn move_backward(&mut self, col: usize) {
        self.vterm.inner.move_backward(col);
    }

    fn move_down_and_cr(&mut self, row: usize) {
        self.vterm.inner.move_down_and_cr(row);
    }

    fn move_up_and_cr(&mut self, row: usize) {
        self.vterm.inner.move_up_and_cr(row);
    }

    fn put_tab(&mut self, count: u16) {
        self.vterm.inner.put_tab(count);
    }

    fn backspace(&mut self) {
        self.vterm.inner.backspace();
    }

    fn carriage_return(&mut self) {
        self.vterm.inner.carriage_return();
    }

    fn linefeed(&mut self) {
        self.vterm.inner.linefeed();
    }

    fn bell(&mut self) {
        self.vterm.inner.bell();
    }

    fn substitute(&mut self) {
        self.vterm.inner.substitute();
    }

    fn newline(&mut self) {
        self.vterm.inner.newline();
    }

    fn set_horizontal_tabstop(&mut self) {
        self.vterm.inner.set_horizontal_tabstop();
    }

    fn scroll_up(&mut self, n: usize) {
        self.vterm.inner.scroll_up(n);
    }

    fn scroll_down(&mut self, n: usize) {
        self.vterm.inner.scroll_down(n);
    }

    fn insert_blank_lines(&mut self, n: usize) {
        self.vterm.inner.insert_blank_lines(n);
    }

    fn delete_lines(&mut self, n: usize) {
        self.vterm.inner.delete_lines(n);
    }

    fn erase_chars(&mut self, n: usize) {
        self.vterm.inner.erase_chars(n);
    }

    fn delete_chars(&mut self, n: usize) {
        self.vterm.inner.delete_chars(n);
    }

    fn move_backward_tabs(&mut self, count: u16) {
        self.vterm.inner.move_backward_tabs(count);
    }

    fn move_forward_tabs(&mut self, count: u16) {
        self.vterm.inner.move_forward_tabs(count);
    }

    fn save_cursor_position(&mut self) {
        self.vterm.inner.save_cursor_position();
    }

    fn restore_cursor_position(&mut self) {
        self.vterm.inner.restore_cursor_position();
    }

    fn clear_line(&mut self, mode: ansi::LineClearMode) {
        self.vterm.inner.clear_line(mode);
    }

    fn clear_screen(&mut self, mode: ansi::ClearMode) {
        match mode {
            ansi::ClearMode::Saved => {
                // Refuse to clear the scrollback as this messes up the
                // buffer. This is handled elisp-side with
                // mistty-allow-clearing-scrollback.
            }
            _ => {
                self.vterm.inner.clear_screen(mode);
            }
        }
    }

    fn clear_tabs(&mut self, mode: ansi::TabulationClearMode) {
        self.vterm.inner.clear_tabs(mode);
    }

    fn set_tabs(&mut self, interval: u16) {
        self.vterm.inner.set_tabs(interval);
    }

    fn reset_state(&mut self) {
        // The scrollback should resist a reset for MisTTY. A reset of
        // the scrollback, if desired, can be done elisp-side with
        // mistty-allow-clearing-scrollback.
        self.vterm.inner.clear_screen(ansi::ClearMode::All);
        move_history(self.vterm.inner.grid_mut(), &mut self.vterm.scrollback);

        self.vterm.inner.reset_state();
        init_grid(self.vterm.inner.grid_mut());
    }

    fn reverse_index(&mut self) {
        self.vterm.inner.reverse_index();
    }

    fn set_mode(&mut self, mode: ansi::Mode) {
        self.vterm.inner.set_mode(mode);
    }

    fn unset_mode(&mut self, mode: ansi::Mode) {
        self.vterm.inner.unset_mode(mode);
    }

    fn report_mode(&mut self, mode: ansi::Mode) {
        self.vterm.inner.report_mode(mode);
    }

    fn set_private_mode(&mut self, mode: PrivateMode) {
        match mode {
            PrivateMode::Named(NamedPrivateMode::SwapScreenAndSetRestoreCursor) => {
                if !self.vterm.inner.mode().contains(TermMode::ALT_SCREEN) {
                    move_history(self.vterm.inner.grid_mut(), &mut self.vterm.scrollback);
                }
            }
            PrivateMode::Named(NamedPrivateMode::BracketedPaste) => {
                self.prompt_detector.set_bracketed_paste(self.vterm, true);
            }
            _ => {}
        }
        self.vterm.inner.set_private_mode(mode);
    }

    fn unset_private_mode(&mut self, mode: PrivateMode) {
        match mode {
            PrivateMode::Named(NamedPrivateMode::BracketedPaste) => {
                self.prompt_detector.set_bracketed_paste(self.vterm, false);
            }
            _ => {}
        }

        self.vterm.inner.unset_private_mode(mode);
    }

    fn report_private_mode(&mut self, mode: PrivateMode) {
        self.vterm.inner.report_private_mode(mode);
    }

    fn set_scrolling_region(&mut self, top: usize, bottom: Option<usize>) {
        self.vterm.inner.set_scrolling_region(top, bottom);
    }

    fn set_keypad_application_mode(&mut self) {
        self.vterm.inner.set_keypad_application_mode();
    }

    fn unset_keypad_application_mode(&mut self) {
        self.vterm.inner.unset_keypad_application_mode();
    }

    fn set_active_charset(&mut self, index: ansi::CharsetIndex) {
        self.vterm.inner.set_active_charset(index);
    }

    fn configure_charset(&mut self, index: ansi::CharsetIndex, charset: ansi::StandardCharset) {
        self.vterm.inner.configure_charset(index, charset);
    }

    fn set_color(&mut self, index: usize, color: ansi::Rgb) {
        self.vterm.inner.set_color(index, color);
    }

    fn dynamic_color_sequence(&mut self, prefix: String, index: usize, terminator: &str) {
        self.vterm
            .inner
            .dynamic_color_sequence(prefix, index, terminator);
    }

    fn reset_color(&mut self, index: usize) {
        self.vterm.inner.reset_color(index);
    }

    fn clipboard_store(&mut self, clipboard: u8, base64: &[u8]) {
        self.vterm.inner.clipboard_store(clipboard, base64);
    }

    fn clipboard_load(&mut self, clipboard: u8, terminator: &str) {
        self.vterm.inner.clipboard_load(clipboard, terminator);
    }

    fn decaln(&mut self) {
        self.vterm.inner.decaln();
    }

    fn push_title(&mut self) {
        self.vterm.inner.push_title();
    }

    fn pop_title(&mut self) {
        self.vterm.inner.pop_title();
    }

    fn text_area_size_pixels(&mut self) {
        self.vterm.inner.text_area_size_pixels();
    }

    fn text_area_size_chars(&mut self) {
        self.vterm.inner.text_area_size_chars();
    }

    fn set_hyperlink(&mut self, hyperlink: Option<ansi::Hyperlink>) {
        self.vterm.inner.set_hyperlink(hyperlink);
    }

    fn set_mouse_cursor_icon(&mut self, icon: ansi::cursor_icon::CursorIcon) {
        self.vterm.inner.set_mouse_cursor_icon(icon);
    }

    fn report_keyboard_mode(&mut self) {
        self.vterm.inner.report_keyboard_mode();
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
        self.vterm.inner.set_keyboard_mode(
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
        self.vterm.inner.push_keyboard_mode(if mode.is_empty() {
            KeyboardModes::empty()
        } else {
            KeyboardModes::DISAMBIGUATE_ESC_CODES
        });
    }

    fn pop_keyboard_modes(&mut self, to_pop: u16) {
        self.vterm.inner.pop_keyboard_modes(to_pop);
    }

    fn set_modify_other_keys(&mut self, _mode: ModifyOtherKeys) {
        // unsupported
        self.vterm
            .inner
            .set_modify_other_keys(ModifyOtherKeys::Reset);
    }

    fn report_modify_other_keys(&mut self) {
        self.vterm.inner.report_modify_other_keys();
    }

    fn set_scp(&mut self, char_path: ansi::ScpCharPath, update_mode: ansi::ScpUpdateMode) {
        self.vterm.inner.set_scp(char_path, update_mode);
    }

    /// OSC 133 A
    fn prompt_start(&mut self, args: ansi::KeyValueArgs<'_>) {
        self.prompt_detector
            .osc133_prompt_start(self.vterm, args.get("click_events") == Some("1"));
    }

    /// OSC 133 B
    fn command_start(&mut self) {
        self.prompt_detector.osc133_command_start(self.vterm);
    }

    /// OSC 133 C
    fn command_executed(&mut self, __args: ansi::KeyValueArgs<'_>) {
        // TODO: forward cmdline_url
        self.prompt_detector.osc133_command_end(self.vterm);
    }

    /// OSC 133 D
    fn command_finished(&mut self, _exit_status: Option<u8>) {
        self.prompt_detector.osc133_command_end(self.vterm);
    }

    /// OSC 7
    fn set_cwd(&mut self, url: Option<&str>) {
        self.vterm.set_cwd(url);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::row_to_string;

    struct Fixture {
        vterm: VTerm,
        processor: VTermProcessor,
    }

    impl Fixture {
        fn setup() -> Self {
            Self {
                vterm: VTerm::new(10, 3, Osc52::Disabled),
                processor: VTermProcessor::new(),
            }
        }

        fn handler(&mut self) -> HandlerProxy<'_> {
            HandlerProxy::new(&mut self.vterm, &mut self.processor.prompt_detector)
        }

        fn process_bytes(&mut self, bytes: &[u8]) {
            self.processor.process_bytes(&mut self.vterm, bytes);
        }
    }

    #[test]
    fn scrollback_empty() {
        let mut fixture = Fixture::setup();
        let vterm = &mut fixture.vterm;

        vterm.enable_scrollback();

        assert_eq!(0, vterm.scrollback_row_count());
        assert_eq!(0, vterm.scrollback_rows().count());
        assert!(vterm.last_scrollback_row().is_none());
    }

    #[test]
    fn disabled_scrollback() {
        let mut fixture = Fixture::setup();

        fixture.vterm.disable_scrollback();

        // fill and scroll terminal
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        // scrollback remains empty
        let vterm = &fixture.vterm;
        assert_eq!(0, vterm.scrollback_row_count());
        assert_eq!(0, vterm.scrollback_rows().count());
        assert!(vterm.last_scrollback_row().is_none());
    }

    #[test]
    fn scrollback_from_grid() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        // fill and scroll terminal
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        // the first three lines are no included into history
        let vterm = &fixture.vterm;
        assert_eq!(3, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(vterm.last_scrollback_row().expect("last_row")),
            "line 3"
        );
    }

    #[test]
    fn scrollback_after_move() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        // fill and scroll terminal, putting 3 rows into history
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &mut fixture.vterm;
        vterm.move_history();

        // grid history is empty
        assert_eq!(0, vterm.grid().history_size());

        // the three lines are still available in VTerm.scrollback
        assert_eq!(3, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(vterm.last_scrollback_row().expect("last_row")),
            "line 3"
        );
    }

    #[test]
    fn scrollback_merged() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        // fill and scroll terminal, putting 3 rows into history
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        let vterm = &fixture.vterm;
        assert_eq!(2, vterm.grid().history_size());

        // 3 rows are in VTerm.scrollback, 2 in grid history
        assert_eq!(5, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3", "line 4", "line 5"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(vterm.last_scrollback_row().expect("last_row")),
            "line 5"
        );
    }

    #[test]
    fn scrollback_append() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        let vterm = &mut fixture.vterm;

        assert_eq!(2, vterm.grid().history_size());
        vterm.move_history();

        // Everything is now in VTerm::scrollback; make sure it was
        // handled properly.

        assert_eq!(5, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3", "line 4", "line 5"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(vterm.last_scrollback_row().expect("last_row")),
            "line 5"
        );
    }

    #[test]
    fn clear_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        let vterm = &mut fixture.vterm;
        assert_eq!(2, vterm.grid().history_size());

        // this should clear both the scrollback in VTerm::scrollback and the grid history
        vterm.clear_scrollback();

        assert_eq!(0, vterm.grid().history_size());
        assert_eq!(0, vterm.scrollback_row_count());
    }

    #[test]
    fn ignore_clear_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(3, fixture.vterm.grid().history_size());

        fixture.handler().clear_screen(ansi::ClearMode::Saved);
        assert_eq!(3, fixture.vterm.grid().history_size());
    }

    #[test]
    fn reset_fills_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3");

        // scrollback is initially empty
        assert_eq!(0, fixture.vterm.scrollback_row_count());
        assert_eq!(0, fixture.vterm.scrollback_rows().count());

        // reset clears the screen (among other things) and store
        // the screen content into scrollback.
        fixture.handler().reset_state();

        // the three screen lines are now in scrollback
        let vterm = &fixture.vterm;
        assert_eq!(3, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        assert_eq!(
            row_to_string(vterm.last_scrollback_row().expect("last_row")),
            "line 3"
        );

        // the screen is clear
        assert_eq!(0, vterm.grid().history_size());
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }
    }

    #[test]
    fn save_scrollback_before_switching_to_alt_buf() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(3, fixture.vterm.scrollback_row_count());

        fixture.handler().set_private_mode(PrivateMode::Named(
            NamedPrivateMode::SwapScreenAndSetRestoreCursor,
        ));

        // scrollback is still available, even after the swap
        let vterm = &fixture.vterm;
        assert_eq!(3, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
    }

    #[test]
    fn screen_top_scrolline_empty() {
        let vterm = VTerm::new(10, 3, Osc52::Disabled);
        assert_eq!(Scrolline(0), vterm.screen_top_scrolline());
    }

    #[test]
    fn screen_top_scrolline_with_history() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        // 3 scrollines/5 terminal lines
        fixture.process_bytes(b"line 1 is long\r\nline 2\r\nline 3 is long\r\n");
        // writing 3 terminal lines puts everything above into history
        fixture.process_bytes(b"line 4\r\nline 5\r\n");
        assert_eq!(Scrolline(3), fixture.vterm.screen_top_scrolline());
        assert_eq!(5, fixture.vterm.scrollback_row_count());

        // 2 more scrollines scroll into history
        fixture.process_bytes(b"line 6 is long\r\nline 7");
        let vterm = &fixture.vterm;
        assert_eq!(Scrolline(5), vterm.screen_top_scrolline());
        assert_eq!(7, vterm.scrollback_row_count());
    }

    #[test]
    fn screen_top_scrolline_after_clear_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(Scrolline(3), fixture.vterm.screen_top_scrolline());

        fixture.vterm.clear_scrollback();
        assert_eq!(Scrolline(3), fixture.vterm.base_scrolline());
        assert_eq!(Scrolline(3), fixture.vterm.screen_top_scrolline());

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        assert_eq!(Scrolline(5), fixture.vterm.screen_top_scrolline());
    }

    #[test]
    fn terminal_line_for_scrolline_screen() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"line 1\r\nline 2\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(1)),
            vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(2)),
            vterm.terminal_line_for_scrolline(Scrolline(2))
        );

        // We're past the end of the screen
        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(3)));
    }

    #[test]
    fn terminal_line_for_scrolline_within_history() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(Scrolline(3), vterm.screen_top_scrolline());
        assert_eq!(
            Some(Line(-3)),
            vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(-2)),
            vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(-1)),
            vterm.terminal_line_for_scrolline(Scrolline(2))
        );
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(3))
        );
        assert_eq!(
            Some(Line(1)),
            vterm.terminal_line_for_scrolline(Scrolline(4))
        );
        assert_eq!(
            Some(Line(2)),
            vterm.terminal_line_for_scrolline(Scrolline(5))
        );
        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(6)));
    }

    #[test]
    fn terminal_line_for_scrolline_within_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        let vterm = &fixture.vterm;
        assert_eq!(Scrolline(5), vterm.screen_top_scrolline());

        assert_eq!(
            Some(Line(-5)),
            vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(-4)),
            vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(-3)),
            vterm.terminal_line_for_scrolline(Scrolline(2))
        );
        assert_eq!(
            Some(Line(-2)),
            vterm.terminal_line_for_scrolline(Scrolline(3))
        );
        assert_eq!(
            Some(Line(-1)),
            vterm.terminal_line_for_scrolline(Scrolline(4))
        );
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(5))
        );
        assert_eq!(
            Some(Line(1)),
            vterm.terminal_line_for_scrolline(Scrolline(6))
        );
        assert_eq!(
            Some(Line(2)),
            vterm.terminal_line_for_scrolline(Scrolline(7))
        );
        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(8)));
    }

    #[test]
    fn terminal_line_for_scrolline_out_of_bounds() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        let vterm = &mut fixture.vterm;
        vterm.clear_scrollback();

        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(0)));
        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(1)));
        assert_eq!(None, vterm.terminal_line_for_scrolline(Scrolline(2)));
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(3))
        );
    }

    #[test]
    fn scrolline_with_wrapped_lines() {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 4);
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"A long line....\r\nline 2\r\n");

        let vterm = &mut fixture.vterm;
        assert_eq!(Scrolline(0), vterm.screen_top_scrolline());
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(2)),
            vterm.terminal_line_for_scrolline(Scrolline(1))
        );

        // Put screen content into scrollback, make sure it still works.
        vterm.inner.clear_screen(ansi::ClearMode::All);
        assert_eq!(Scrolline(2), vterm.screen_top_scrolline());
        assert_eq!(
            Some(Line(-3)),
            vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(-1)),
            vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(0)),
            vterm.terminal_line_for_scrolline(Scrolline(2))
        );
    }

    #[test]
    fn collect_mode_changes_kkp() {
        let mut fixture = Fixture::setup();
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[>1u");
        assert_eq!(
            vec![VTermChange::KittyKeyboard(true)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[>8u");
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[<u");
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[<u");
        assert_eq!(
            vec![VTermChange::KittyKeyboard(false)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );
    }

    #[test]
    fn collect_mode_changes_bracketed_paste() {
        let mut fixture = Fixture::setup();

        fixture.process_bytes(b"\x1b[?2004h");
        assert_eq!(
            vec![VTermChange::BracketedPaste(true)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?2004l");
        assert_eq!(
            vec![VTermChange::BracketedPaste(false)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );
    }

    #[test]
    fn collect_mode_changes_fullscreen() {
        let mut fixture = Fixture::setup();

        fixture.process_bytes(b"\x1b[?1049h");
        assert_eq!(
            vec![VTermChange::Fullscreen(true)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?1049l");
        assert_eq!(
            vec![VTermChange::Fullscreen(false)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );
    }

    #[test]
    fn collect_mode_changes_mouse() {
        let mut fixture = Fixture::setup();

        fixture.process_bytes(b"\x1b[?1000h");
        assert_eq!(
            vec![VTermChange::Mouse(MouseMode::Click)],
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?1002h");
        assert_eq!(
            vec![VTermChange::Mouse(MouseMode::Drag)],
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?1003h");
        assert_eq!(
            vec![VTermChange::Mouse(MouseMode::Motion)],
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?1003l");
        assert_eq!(
            vec![VTermChange::Mouse(MouseMode::Off)],
            fixture.vterm.collect_mode_changes()
        );
    }

    #[test]
    fn collect_mode_changes_across_multiple_process_bytes() {
        let mut fixture = Fixture::setup();

        fixture.process_bytes(b"\x1b[?1000h");
        fixture.process_bytes(b"\x1b[?1002h");
        fixture.process_bytes(b"\x1b[>1u");

        assert_eq!(
            vec![
                VTermChange::KittyKeyboard(true),
                VTermChange::Mouse(MouseMode::Drag),
            ],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );
    }
}
