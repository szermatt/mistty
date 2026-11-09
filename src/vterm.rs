//! Define [VTerm], a wrapper type for
// [alacritty_terminal::term::Term] and
// [alacritty_terminal::term::Grid].

use crate::damage::GridDamage;
use crate::gridext::{CellExt, GridExt, RowExt};
use crate::prompt::{PromptChange, PromptDetector};
use crate::render;
use crate::types::Scrolline;
use alacritty_terminal::vte::ansi::{
    ClearMode, KeyboardModes, KeyboardModesApplyBehavior, ModifyOtherKeys,
};
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
use either::Either;
use emacs::{Env, IntoLisp, Result, Value};
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

emacs::use_functions! {
    nreverse_func => "nreverse"
    current_kill
    kill_new
    plist_member
    cadr
}
emacs::use_symbols! {
    bracketed_paste_sym => "bracketed-paste"
    title_sym => "title"
    kkp_sym => "kkp"
    mouse_sym => "mouse"
    click_sym => "click"
    drag_sym => "drag"
    motion_sym => "motion"
    nil_sym => "nil"
    cwd_sym => "cwd"
    cursor_sym => "cursor"
    fullscreen_sym => "fullscreen"
    cleared_screen_sym => "cleared-screen"
    cleared_scrollback_sym => "cleared-scrollback"
    modified_sym => "modified"
    reset_sym => "reset"
    colon_allow_clearing_scrollback => ":allow-clearing-scrollback"
    colon_osc52 => ":osc52"
    only_copy
    only_paste
    copy_paste
}

/// Size of the scrollback, in lines. There needs to be enough space
/// to keep scrollback between calls to `write_scrollback`, to not lose data.
const SCROLLBACK_SIZE: usize = 100000;

/// Tag identifying a render operation, stored in an Emacs text
/// property to check whether it's safe to render only the damaged
/// portions of the terminal.
#[derive(Copy, Clone, Debug, Default)]
pub(crate) struct RenderIndex {
    count: usize,
}

impl RenderIndex {
    /// Check whether another tag matches the current one
    pub(crate) fn matches(&self, other: &Self) -> bool {
        self.count == other.count
    }

    /// Increment the tag count.
    fn increment(&mut self) {
        self.count = self.count.wrapping_add(1);
    }
}

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

    /// Start a regexp prompt and make it current, return true if a
    /// prompt was created.
    ///
    /// The change will *not* be reported in the next call to
    /// [collect_prompt_change].
    pub(crate) fn start_regexp_prompt(&mut self, vterm: &mut VTerm, start: Scrolline) -> bool {
        self.prompt_detector.start_regexp_prompt(vterm, start)
    }

    /// End a regexp prompt.
    ///
    /// The change will *not* be reported in the next call to
    /// [collect_prompt_change].
    pub(crate) fn end_regexp_prompt(&mut self, vterm: &mut VTerm) {
        self.prompt_detector.end_regexp_prompt(vterm)
    }

    /// Update the terminal and processor state from the given teminal
    /// sequence.
    pub(crate) fn process_bytes(&mut self, vterm: &mut VTerm, bytes: &[u8]) {
        let cols = vterm.columns();
        let mut start = 0;
        loop {
            match self
                .prompt_detector
                .detect_prompt_sp(cols, &bytes[start..])
                .map(|idx| start + idx)
            {
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

    /// Screen has been cleared
    ClearedScreen,

    /// Scrollback has been cleared
    ClearedScrollback,

    /// Terminal has been reset. This is generally accompanied by
    /// changes for clearing state, such as keyboard mode, bracketed
    /// paste or clearing the scrollback.
    Reset,

    /// Show or hide the cursor.
    ShowCursor(bool),

    /// Report that a scrolline above the target scrolline has been
    /// unexpectedly modified.
    ///
    /// The target scrolline is set with
    /// [VTerm::track_modifications_above].
    Modified(Scrolline),
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
            VTermChange::ClearedScreen => env.cons(cleared_screen_sym, ()),
            VTermChange::ClearedScrollback => env.cons(cleared_scrollback_sym, ()),
            VTermChange::Reset => env.cons(reset_sym, ()),
            VTermChange::ShowCursor(on) => env.list((cursor_sym, on)),
            VTermChange::Modified(scrolline) => env.list((modified_sym, scrolline)),
        }
    }
}

#[derive(Default, Clone, Debug)]
pub(crate) struct VTermOptions {
    pub(crate) osc52: Osc52,
    pub(crate) allow_clearing_scrollback: bool,
}

impl VTermOptions {
    /// Update options from an property list.
    ///
    /// Properties:
    ///  - :osc52 nil|'only-copy|'only-paste|'copy-paste
    ///  - :allow-clearing-scrollback bool
    ///
    /// Unknown properties are ignored.
    pub(crate) fn apply_plist(&mut self, plist: Value<'_>) -> Result<()> {
        let env = plist.env;

        let member = env.call(plist_member, (plist, colon_osc52))?;
        if member.is_not_nil() {
            let osc52 = env.call(cadr, (member,))?;
            if !osc52.is_not_nil() {
                self.osc52 = Osc52::Disabled;
            } else if osc52 == *only_copy {
                self.osc52 = Osc52::OnlyCopy;
            } else if osc52 == *only_paste {
                self.osc52 = Osc52::OnlyPaste;
            } else if osc52 == *copy_paste {
                self.osc52 = Osc52::CopyPaste;
            } else {
                let _ = env.call("mistty-log", ("Unknown value for :osc52: '%s'", osc52));
            };
        }

        let member = env.call(plist_member, (plist, colon_allow_clearing_scrollback))?;
        if member.is_not_nil() {
            self.allow_clearing_scrollback = env.call(cadr, (member,))?.is_not_nil();
        }

        Ok(())
    }
}

/// Virtual Terminal for MisTTY that keeps its data in memory.
pub(crate) struct VTerm {
    inner: Term<EventAccumulator>,
    events: Rc<RefCell<VecDeque<Event>>>,

    /// Current option values.
    options: VTermOptions,

    scrollback_enabled: bool,
    render_index: RenderIndex,

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

    /// Changes to return to the next call to [VTerm::collect_changes].
    changes: Vec<VTermChange>,

    /// Terminal mode at the time of the last call to [VTerm::collect_changes].
    last_collected_mode: TermMode,

    /// Track terminal modifications above a given scrolline.
    modification_tracker: Option<ModificationTracker>,
}

/// Track changes above a given scrolline
struct ModificationTracker {
    target: Scrolline,

    /// Hashes of scrollines above the target.
    ///
    /// The hash is computed using fnv1a_32.
    ///
    /// - index 0 is the hash of scrolline target-1
    /// - index 1 is the hash of scrolline target-2
    ///
    /// and so one. These hashes are captured when the target is set,
    /// based on the scrolline currently visible on the terminal.
    ///
    /// Note that the hashes might reach scrolline below the current
    /// scrolline at screen top, depending on when it was created.
    /// These should be ignored.
    hashes: Vec<u32>,
}

impl VTerm {
    /// Create a new terminal with the given dimensions
    pub(crate) fn new(width: usize, height: usize, options: VTermOptions) -> Self {
        let events = Rc::new(RefCell::new(VecDeque::new()));
        let acc = EventAccumulator {
            events: Rc::clone(&events),
        };
        let config = build_config(&options, false);
        let dims = VTermDimensions::new(width, height, config.scrolling_history);
        let mut inner = Term::new(config, &dims, acc);

        // See comment on HandlerProxy
        init_grid(inner.grid_mut());

        Self {
            inner,
            events,
            base_scrolline: Scrolline(0),
            scrollback_enabled: false,
            render_index: RenderIndex::default(),
            scrollback: vec![],
            extra_damage: GridDamage::new(),
            last_cell: None,
            last_collected_mode: TermMode::default(),
            changes: vec![],
            options,
            modification_tracker: None,
        }
    }

    /// Current terminal mode.
    #[inline]
    pub(crate) fn mode(&self) -> &TermMode {
        self.inner.mode()
    }

    /// Return the current options
    #[inline]
    pub(crate) fn options(&self) -> &VTermOptions {
        &self.options
    }

    /// Update the current options.
    pub(crate) fn set_options(&mut self, options: VTermOptions) {
        if options.osc52 != self.options.osc52 {
            let config = build_config(&options, self.scrollback_enabled);
            self.inner.set_options(config);
        }
        self.options = options;
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
    pub(crate) fn scrolline_at_screen_top(&self) -> Scrolline {
        self.base_scrolline + self.scrollback_rows().filter(|r| !r.is_wrapped()).count()
    }

    /// Scrolline that correspond to the line the cursor is on.
    pub(crate) fn scrolline_at_cursor(&self) -> Scrolline {
        self.scrolline_at_screen_top()
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
        let new_base = self.scrolline_at_screen_top();

        self.scrollback.clear();
        self.inner.grid_mut().clear_history();

        self.base_scrolline = new_base;
    }

    /// Clear scrollback, but keep the last `line_count` lines.
    pub(crate) fn retain_scrollback_lines(&mut self, line_count: usize) {
        let total = self.scrollback_row_count();
        if line_count >= total {
            return;
        }

        if line_count == 0 {
            self.clear_scrollback();
            return;
        }

        let drop_count = total - line_count;
        let mut removed_scrollines = 0;

        let drop_from_scrollback = drop_count.min(self.scrollback.len());
        if drop_from_scrollback > 0 {
            removed_scrollines += self.scrollback[0..drop_from_scrollback]
                .iter()
                .filter(|r| !r.is_wrapped())
                .count();
            self.scrollback.drain(0..drop_from_scrollback);
        }

        // Drop from the grid. It's worth it to keep scrollback lines
        // in the grid as much as possible, and just call
        // move_history() at the beginning of this function, as
        // alacritty will then re-flow these lines when the terminal
        // is resized.
        let drop_from_grid = drop_count - drop_from_scrollback;
        if drop_from_grid > 0 {
            let grid = self.inner.grid_mut();
            let grid_history_size = grid.history_size();
            let keep_in_grid = grid_history_size - drop_from_grid;
            for (_, row) in grid.ranged_iter(
                Line(-(grid_history_size as i32)),
                Line(-(keep_in_grid as i32)),
            ) {
                if !row.is_wrapped() {
                    removed_scrollines += 1;
                }
            }
            grid.update_history(keep_in_grid);
            grid.update_history(SCROLLBACK_SIZE);
        }

        self.base_scrolline += removed_scrollines;
    }

    /// Store grid history into [VTerm]'s scrollback buffer.
    ///
    /// Call this before making a change that would otherwise lose or
    /// hide grid history, such as reset or swapping to the alt
    /// buffer.
    fn move_history(&mut self) {
        let grid: &mut Grid<Cell> = self.inner.grid_mut();
        for line in grid.topmost_line().0..0 {
            let line = Line(line);
            let row = &mut grid[line];
            let mut copy = Row::new(row.len());
            std::mem::swap(row, &mut copy);
            self.scrollback.push(copy);
        }
        grid.clear_history();
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
    pub(crate) fn scrollback_rows(&self) -> impl DoubleEndedIterator<Item = &Row<Cell>> {
        self.scrollback
            .iter()
            .chain(self.inner.grid().history_iter().map(|(_, row)| row))
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

    /// Return a subset of the rows.
    ///
    /// This includes rows in scrollback as well as rows on the screen. Rows in scrollback are indexed
    /// as [Line] with negative values, even when they're not on the grid anymore.
    ///
    /// Panics if give invalid or out-of-bounds `start` and `end`.
    pub(crate) fn row_range(
        &self,
        start: Line,
        end: Line,
    ) -> impl Iterator<Item = (Line, &Row<Cell>)> {
        assert!(start <= end, "Invalid row range {start} > {end}");
        if start.0 >= 0 {
            Either::Left(self.grid().ranged_iter(start, end))
        } else {
            let min_start = Line(-(self.scrollback_row_count() as i32));
            assert!(start >= min_start, "Invalid row range {start} too low");
            let skip_count = (start.0 - min_start.0) as usize;
            let take_count = (end.0 - start.0) as usize;
            Either::Right(self.all_rows().skip(skip_count).take(take_count))
        }
    }

    /// Return lines within the given scrolline ranges.
    ///
    /// Panics if given invalid or out-of-bounds `start` and `end`.
    pub(crate) fn scrolline_range(
        &self,
        start: Scrolline,
        end: Scrolline,
    ) -> impl Iterator<Item = (Line, &Row<Cell>)> {
        assert!(start <= end, "Invalid scrolline range {start} > {end}");
        assert!(
            start >= self.base_scrolline,
            "Invalid scrolline {start}, before base."
        );

        let mut cur = self.base_scrolline();
        self.all_rows()
            .map(move |(l, r)| {
                let sl = cur;
                if !r.is_wrapped() {
                    cur += 1;
                }

                (sl, l, r)
            })
            .skip_while(move |(sl, _, _)| *sl < start)
            .take_while(move |(sl, _, _)| *sl < end)
            .map(|(_, l, r)| (l, r))
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
    /// later and returned as part of [VTerm::collect_changes].
    ///
    /// This should be called right after calling 'process_bytes' to
    /// avoid delaying responses.
    pub(crate) fn prepare_responses<'a>(&mut self, env: &'a Env) -> Result<Vec<String>> {
        let mut pty_writes = vec![];
        let mut changes = vec![];
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
                    changes.push(VTermChange::Title(Some(title)));
                }
                Event::ResetTitle => {
                    changes.push(VTermChange::Title(None));
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

        self.changes.append(&mut changes);

        Ok(pty_writes)
    }

    /// Tell VTerm to report modifications above the given `target` scrolline.
    ///
    /// Tracking modification only applies to the main buffer.
    pub(crate) fn track_modifications_above(&mut self, target: Scrolline) {
        let screen_top = self.scrolline_at_screen_top();
        if target <= screen_top {
            self.modification_tracker = None;
            return;
        }

        let alt_buf = self.mode().intersects(TermMode::ALT_SCREEN);
        if alt_buf {
            self.inner.swap_alt();
        }

        self.modification_tracker = Some(match self.modification_tracker.take() {
            // create new
            None => ModificationTracker {
                target,
                hashes: self.hash_lines(screen_top, target),
            },
            // restrict existing. It is important to keep existing
            // hashes and not recompute them as the current scrolline
            // data might have already changed since then.
            Some(mut t) if t.target > target => {
                t.hashes.drain(0..(t.target - target));
                t.target = target;

                t
            }
            // expand existing.
            Some(mut t) if t.target < target && t.target > screen_top => {
                let mut new_hashes = self.hash_lines(t.target, target);
                t.hashes.truncate(t.target - screen_top);
                new_hashes.append(&mut t.hashes);
                std::mem::swap(&mut t.hashes, &mut new_hashes);
                t.target = target;

                t
            }
            // replace existing
            Some(t) if t.target < target => ModificationTracker {
                target,
                hashes: self.hash_lines(screen_top, target),
            },
            // tracker.target = target, keep it
            Some(t) => t,
        });

        if alt_buf {
            self.inner.swap_alt();
        }
        assert_eq!(alt_buf, self.mode().intersects(TermMode::ALT_SCREEN));
    }

    /// Collect events to send back to the lisp side.
    pub(crate) fn collect_changes<'e>(&mut self) -> Result<Vec<VTermChange>> {
        let mut changes = self.collect_mode_changes();
        changes.append(&mut self.changes);

        if let Some(modified) = self.detect_modifications() {
            changes.push(VTermChange::Modified(modified));
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

        let cursor_on = mode.intersects(TermMode::SHOW_CURSOR);
        if cursor_on != before.intersects(TermMode::SHOW_CURSOR) {
            reports.push(VTermChange::ShowCursor(cursor_on));
        }

        reports
    }

    /// Return the tag that identifies the last render operation.
    pub(crate) fn current_render_index(&self) -> RenderIndex {
        self.render_index
    }

    /// Increment and return a tag that identifies a render operation.
    pub(crate) fn next_render_index(&mut self) -> RenderIndex {
        self.render_index.increment();

        self.render_index
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

    /// Record a change to report to the next call to [VTerm::collect_changes].
    fn record(&mut self, change: VTermChange) {
        self.changes.push(change)
    }

    /// Hashes of scrollines betwen start (inclusive) and end
    /// (exclusive).
    ///
    /// The hash is computed using fnv1a_32.
    ///
    /// The hashes are returned in the reverse order, from end to start:
    ///
    /// - index 0 is the hash of scrolline end-1
    /// - index 1 is the hash of scrolline end-2
    /// - ...
    /// - index N is the hash of scrolline end
    fn hash_lines(&self, start: Scrolline, end: Scrolline) -> Vec<u32> {
        assert!(!self.mode().intersects(TermMode::ALT_SCREEN));

        let mut hashes = vec![];
        let mut hasher = Fnv1aHasher::new();
        let mut buf = String::new();
        for (_, row) in self.scrolline_range(start, end) {
            for (_, cell) in row.iter() {
                cell.append_content(&mut buf);
                hasher.add(&buf);
                buf.clear();
            }
            if !row.is_wrapped() {
                hashes.push(hasher.take());
            }
        }
        hashes.reverse();

        hashes
    }

    /// If tracking modification, check the current buffer and report the earliest
    /// tracked scrolline that was modified.
    fn detect_modifications(&mut self) -> Option<Scrolline> {
        if !self.mode().intersects(TermMode::ALT_SCREEN)
            && let Some(tracker) = &self.modification_tracker
        {
            let screen_top = self.scrolline_at_screen_top();
            if tracker.target > screen_top {
                let hashes = self.hash_lines(screen_top, tracker.target);
                if let Some(index) = std::iter::zip(&tracker.hashes, hashes)
                    .enumerate()
                    .filter(|(_, (h1, h2))| **h1 != *h2)
                    .last()
                    .map(|(index, _)| index)
                {
                    let modified = tracker.target - index - 1;

                    self.track_modifications_above(modified);
                    return Some(modified);
                }
            }
        }

        None
    }
}

/// Build [Config] for [Term].
fn build_config(options: &VTermOptions, scrollback_enabled: bool) -> Config {
    let config = Config {
        scrolling_history: if scrollback_enabled {
            SCROLLBACK_SIZE
        } else {
            0
        },
        osc52: options.osc52,
        kitty_keyboard: true,
        ..Default::default()
    };
    config
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

    fn clear_screen(&mut self, mode: ClearMode) {
        match mode {
            ClearMode::Saved => {
                if self.vterm.options.allow_clearing_scrollback
                    && !self.vterm.mode().intersects(TermMode::ALT_SCREEN)
                {
                    self.vterm.clear_scrollback();
                    self.vterm.record(VTermChange::ClearedScrollback);
                }
            }
            ClearMode::All => {
                if self.vterm.mode().intersects(TermMode::ALT_SCREEN) {
                    self.vterm.inner.clear_screen(ClearMode::All);
                    return;
                }
                // If there is an active prompt, discard everything in
                // or below the prompt
                // TODO: Handle prompts starting in scrollback (terminal_line < Line(0)).
                if let Some(prompt) = self.prompt_detector.current_prompt()
                    && let Some(terminal_line) =
                        self.vterm.terminal_line_for_scrolline(prompt.start)
                {
                    self.vterm.grid_mut().reset_region(terminal_line..)
                }
                // Avoid calling ClearMode::All (clear_viewport) on an
                // empty screen, as alacritty_terminal scrolls 1 blank
                // line into history when history_size() == 0.
                if self.vterm.grid().screen_iter().any(|(_, r)| !r.is_clear()) {
                    self.vterm.inner.clear_screen(ClearMode::All);
                }
                self.vterm.record(VTermChange::ClearedScreen);
            }
            ClearMode::Below | ClearMode::Above => {
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
        self.vterm.inner.clear_screen(ClearMode::All);
        self.vterm.move_history();
        if self.vterm.options.allow_clearing_scrollback {
            // clear_screen and move_history put lines into
            // scrollback. Even if the scrollback is later on cleared,
            // it is important to go through these steps to update the
            // scrolline correctly, so it matches with the elisp side.
            self.vterm.clear_scrollback();
            self.vterm.record(VTermChange::ClearedScrollback);
        }
        self.prompt_detector
            .reset(self.vterm.scrolline_at_screen_top());
        self.vterm.inner.reset_state();
        init_grid(self.vterm.inner.grid_mut());
        self.vterm.record(VTermChange::Reset);
        self.vterm.modification_tracker = None;
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
                    self.vterm.move_history();
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
        self.vterm.record(VTermChange::Cwd(url.map(str::to_string)));
    }
}

/// Compute FNV-1a hash for a series of strings.
struct Fnv1aHasher {
    hash: u32,
}

const FNV1_BASE: u32 = 0x811c9dc5;

impl Fnv1aHasher {
    fn new() -> Self {
        Fnv1aHasher { hash: FNV1_BASE }
    }

    /// Add the given string to the hash.
    fn add(&mut self, data: &str) {
        let mut hash = self.hash;
        for byte in data.as_bytes() {
            hash ^= *byte as u32;
            hash = hash.wrapping_mul(0x01000193);
        }
        self.hash = hash;
    }

    /// Return the current hash value and reset the hasher.
    fn take(&mut self) -> u32 {
        let val = self.hash;
        self.hash = FNV1_BASE;

        val
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
                vterm: VTerm::new(10, 3, VTermOptions::default()),
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
    fn retain_scrollback_lines_zero() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.process_bytes(b"line 6\r\nline 7\r\n");

        let vterm = &mut fixture.vterm;
        assert_eq!(5, vterm.scrollback_row_count());
        vterm.retain_scrollback_lines(0);

        assert_eq!(0, vterm.scrollback_row_count());
        assert_eq!(Scrolline(5), vterm.base_scrolline());
    }

    #[test]
    fn retain_scrollback_lines_subset() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.process_bytes(b"line 6\r\nline 7\r\n");

        let vterm = &mut fixture.vterm;
        assert_eq!(5, vterm.scrollback_row_count());

        // Retain the last 2 lines ("line 4", "line 5")
        vterm.retain_scrollback_lines(2);

        assert_eq!(2, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 4", "line 5"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        // Drained 3 non-wrapped lines, so base_scrolline is now 3
        assert_eq!(Scrolline(3), vterm.base_scrolline());
        assert_eq!(
            vec![
                (Line(-2), "line 4".to_string()),
                (Line(-1), "line 5".to_string()),
            ],
            vterm
                .row_range(Line(-2), Line(0))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn retain_scrollback_lines_with_wrapped() {
        let mut fixture = Fixture::setup(); // 10x3
        fixture.vterm.enable_scrollback();

        // The large line is split
        fixture.process_bytes(b"123456789012345\r\nline 2");
        assert_eq!(0, fixture.vterm.scrollback_row_count());
        assert_eq!(
            Vec::<String>::new(),
            fixture
                .vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec!["1234567890", "12345", "line 2"],
            fixture
                .vterm
                .grid()
                .screen_iter()
                .map(|(_, r)| row_to_string(r))
                .collect::<Vec<_>>()
        );
        assert!(fixture.vterm.grid()[Line(0)].is_wrapped());
        assert!(!fixture.vterm.grid()[Line(1)].is_wrapped());

        // The first part of the large line goes into scrollback.
        fixture.process_bytes(b"\r\nline 3");
        assert_eq!(1, fixture.vterm.scrollback_row_count());
        assert_eq!(
            vec!["1234567890"],
            fixture
                .vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<_>>()
        );
        assert!(fixture.vterm.scrollback_rows().next().unwrap().is_wrapped());
        assert_eq!(
            vec!["12345", "line 2", "line 3"],
            fixture
                .vterm
                .grid()
                .screen_iter()
                .map(|(_, r)| row_to_string(r))
                .collect::<Vec<_>>()
        );
        assert!(!fixture.vterm.grid()[Line(0)].is_wrapped());

        // Both parts of the large line are now in scrallbock
        fixture.process_bytes(b"\r\nline 4");
        assert_eq!(2, fixture.vterm.scrollback_row_count());
        assert_eq!(
            vec!["1234567890", "12345"],
            fixture
                .vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<_>>()
        );
        let scrollback: Vec<_> = fixture.vterm.scrollback_rows().collect();
        assert!(scrollback[0].is_wrapped());
        assert!(!scrollback[1].is_wrapped());
        assert_eq!(
            vec!["line 2", "line 3", "line 4"],
            fixture
                .vterm
                .grid()
                .screen_iter()
                .map(|(_, r)| row_to_string(r))
                .collect::<Vec<_>>()
        );

        // Keep only one line of scrollbock
        fixture.vterm.retain_scrollback_lines(1);
        assert_eq!(1, fixture.vterm.scrollback_row_count());
        assert_eq!(
            vec!["12345"],
            fixture
                .vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<_>>()
        );
        assert!(!fixture.vterm.scrollback_rows().next().unwrap().is_wrapped());
        assert_eq!(
            vec!["line 2", "line 3", "line 4"],
            fixture
                .vterm
                .grid()
                .screen_iter()
                .map(|(_, r)| row_to_string(r))
                .collect::<Vec<_>>()
        );
        assert_eq!(Scrolline(0), fixture.vterm.base_scrolline());

        // Clear scrollback
        fixture.vterm.retain_scrollback_lines(0);
        assert_eq!(0, fixture.vterm.scrollback_row_count());
        assert_eq!(
            Vec::<String>::new(),
            fixture
                .vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec!["line 2", "line 3", "line 4"],
            fixture
                .vterm
                .grid()
                .screen_iter()
                .map(|(_, r)| row_to_string(r))
                .collect::<Vec<_>>()
        );
        assert_eq!(Scrolline(1), fixture.vterm.base_scrolline());
    }

    #[test]
    fn test_update_history_shrinks_oldest() {
        let mut fixture = Fixture::setup(); // 10x3
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(3, fixture.vterm.grid().history_size());
        assert_eq!("line 1", row_to_string(&fixture.vterm.grid()[Line(-3)]));
        assert_eq!("line 2", row_to_string(&fixture.vterm.grid()[Line(-2)]));
        assert_eq!("line 3", row_to_string(&fixture.vterm.grid()[Line(-1)]));

        fixture.vterm.inner.grid_mut().update_history(1);
        fixture
            .vterm
            .inner
            .grid_mut()
            .update_history(SCROLLBACK_SIZE);

        assert_eq!(1, fixture.vterm.grid().history_size());
        assert_eq!("line 3", row_to_string(&fixture.vterm.grid()[Line(-1)]));
    }

    #[test]
    fn test_reflow_wrapped_in_history_on_resize() {
        let mut fixture = Fixture::setup(); // 10x3
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"123456789012345\r\nline 2\r\nline 3");
        assert_eq!(1, fixture.vterm.grid().history_size());
        assert_eq!("1234567890", row_to_string(&fixture.vterm.grid()[Line(-1)]));

        fixture.vterm.resize(20, 3);
        assert_eq!(0, fixture.vterm.grid().history_size());
        assert_eq!(
            "123456789012345",
            row_to_string(&fixture.vterm.grid()[Line(0)])
        );
        assert_eq!("line 2", row_to_string(&fixture.vterm.grid()[Line(1)]));
        assert_eq!("line 3", row_to_string(&fixture.vterm.grid()[Line(2)]));
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
    fn clear_screen_without_prompt() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3");
        fixture.process_bytes(b"\x1b[2J");

        let vterm = &mut fixture.vterm;
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }
        assert_eq!(
            vec![VTermChange::ClearedScreen],
            vterm.collect_changes().unwrap()
        );
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
    }

    #[test]
    fn clear_screen_empty() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"\x1b[2J");

        let vterm = &mut fixture.vterm;
        assert_eq!(0, vterm.scrollback_row_count());
        assert_eq!(
            vec![VTermChange::ClearedScreen],
            vterm.collect_changes().unwrap()
        );
    }

    #[test]
    fn clear_screen_with_prompt_at_top() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"\x1b]133;A\x07prompt\r\n$ \x1b]133;B\x07cmd");
        fixture.process_bytes(b"\x1b[2J");

        let vterm = &mut fixture.vterm;
        assert_eq!(0, vterm.scrollback_row_count());
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }
        assert_eq!(
            vec![VTermChange::ClearedScreen],
            vterm.collect_changes().unwrap()
        );
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
    }

    #[test]
    fn clear_screen_with_prompt() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\n\x1b]133;A\x07prompt\r\n$ \x1b]133;B\x07cmd");
        fixture.process_bytes(b"\x1b[2J");

        let vterm = &mut fixture.vterm;
        assert_eq!(
            vec!["line 1"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }
        assert_eq!(
            vec![VTermChange::ClearedScreen],
            vterm.collect_changes().unwrap()
        );
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
    }

    #[test]
    fn clear_screen_with_ended_prompt() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"\x1b]133;A\x07$ \x1b]133;B\x07clear\r\n\x1b]133;C\x07");
        fixture.process_bytes(b"\x1b[2J");

        let vterm = &mut fixture.vterm;
        assert_eq!(
            vec!["$ clear"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }
        assert_eq!(
            vec![VTermChange::ClearedScreen],
            vterm.collect_changes().unwrap()
        );
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
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
        let vterm = &mut fixture.vterm;
        assert_eq!(3, vterm.scrollback_row_count());
        assert_eq!(
            vec!["line 1", "line 2", "line 3"],
            vterm
                .scrollback_rows()
                .map(row_to_string)
                .collect::<Vec<String>>()
        );

        // the screen is clear
        assert_eq!(0, vterm.grid().history_size());
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }

        assert_eq!(vec![VTermChange::Reset], vterm.collect_changes().unwrap());
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
    }

    #[test]
    fn reset_clears_scrollback_when_allowed() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.vterm.set_options(VTermOptions {
            allow_clearing_scrollback: true,
            ..Default::default()
        });

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3");
        fixture.process_bytes(b"\x1bc");

        let vterm = &mut fixture.vterm;
        assert_eq!(0, vterm.scrollback_row_count());
        assert_eq!(0, vterm.grid().history_size());
        assert_eq!(Scrolline(3), vterm.scrolline_at_screen_top());
        for line in 0..=vterm.bottommost_line().0 {
            assert!(vterm.grid()[Line(line)].is_clear());
        }

        assert_eq!(
            vec![VTermChange::ClearedScrollback, VTermChange::Reset],
            vterm.collect_changes().unwrap()
        );
        assert_eq!(Vec::<VTermChange>::new(), vterm.collect_changes().unwrap());
    }

    #[test]
    fn reset_resets_modes() {
        let mut fixture = Fixture::setup();

        fixture.process_bytes(b"\x1b[?1049h\x1b[?2004h\x1b[>1u\x1b[?1000h\x1b[?25l");
        assert_eq!(
            vec![
                VTermChange::KittyKeyboard(true),
                VTermChange::BracketedPaste(true),
                VTermChange::Fullscreen(true),
                VTermChange::Mouse(MouseMode::Click),
                VTermChange::ShowCursor(false),
            ],
            fixture.vterm.collect_changes().unwrap()
        );

        fixture.process_bytes(b"\x1bc");
        assert_eq!(
            vec![
                VTermChange::KittyKeyboard(false),
                VTermChange::BracketedPaste(false),
                VTermChange::Fullscreen(false),
                VTermChange::Mouse(MouseMode::Off),
                VTermChange::ShowCursor(true),
                VTermChange::Reset,
            ],
            fixture.vterm.collect_changes().unwrap()
        );
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
        let vterm = VTerm::new(10, 3, VTermOptions::default());
        assert_eq!(Scrolline(0), vterm.scrolline_at_screen_top());
    }

    #[test]
    fn screen_top_scrolline_with_history() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        // 3 scrollines/5 terminal lines
        fixture.process_bytes(b"line 1 is long\r\nline 2\r\nline 3 is long\r\n");
        // writing 3 terminal lines puts everything above into history
        fixture.process_bytes(b"line 4\r\nline 5\r\n");
        assert_eq!(Scrolline(3), fixture.vterm.scrolline_at_screen_top());
        assert_eq!(5, fixture.vterm.scrollback_row_count());

        // 2 more scrollines scroll into history
        fixture.process_bytes(b"line 6 is long\r\nline 7");
        let vterm = &fixture.vterm;
        assert_eq!(Scrolline(5), vterm.scrolline_at_screen_top());
        assert_eq!(7, vterm.scrollback_row_count());
    }

    #[test]
    fn screen_top_scrolline_after_clear_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        assert_eq!(Scrolline(3), fixture.vterm.scrolline_at_screen_top());

        fixture.vterm.clear_scrollback();
        assert_eq!(Scrolline(3), fixture.vterm.base_scrolline());
        assert_eq!(Scrolline(3), fixture.vterm.scrolline_at_screen_top());

        fixture.process_bytes(b"line 6\r\nline 7\r\n");
        assert_eq!(Scrolline(5), fixture.vterm.scrolline_at_screen_top());
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
        assert_eq!(Scrolline(3), vterm.scrolline_at_screen_top());
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
        assert_eq!(Scrolline(5), vterm.scrolline_at_screen_top());

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
        assert_eq!(Scrolline(0), vterm.scrolline_at_screen_top());
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
        assert_eq!(Scrolline(2), vterm.scrolline_at_screen_top());
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
    fn row_range_screen() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3");

        let vterm = &fixture.vterm;
        assert_eq!(
            vec![
                (Line(0), "line 1".to_string()),
                (Line(1), "line 2".to_string()),
            ],
            vterm
                .row_range(Line(0), Line(2))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![(Line(1), "line 2".to_string())],
            vterm
                .row_range(Line(1), Line(2))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn row_range_scrollback() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();
        fixture.process_bytes(b"line 6\r\nline 7\r\n");

        let vterm = &fixture.vterm;
        // Scrollback has 5 lines: -5 to -1.
        // Lines -5..-3 are in VTerm.scrollback, -2..-1 in grid history.
        assert_eq!(
            vec![
                (Line(-5), "line 1".to_string()),
                (Line(-4), "line 2".to_string()),
            ],
            vterm
                .row_range(Line(-5), Line(-3))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![
                (Line(-3), "line 3".to_string()),
                (Line(-2), "line 4".to_string()),
                (Line(-1), "line 5".to_string()),
            ],
            vterm
                .row_range(Line(-3), Line(0))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn row_range_spanning_scrollback_and_screen() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();
        fixture.process_bytes(b"line 6\r\nline 7\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(
            vec![
                (Line(-2), "line 4".to_string()),
                (Line(-1), "line 5".to_string()),
                (Line(0), "line 6".to_string()),
                (Line(1), "line 7".to_string()),
            ],
            vterm
                .row_range(Line(-2), Line(2))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn row_range_empty() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(0, vterm.row_range(Line(1), Line(1)).count());
        assert_eq!(0, vterm.row_range(Line(-2), Line(-2)).count());
    }

    #[test]
    #[should_panic]
    fn row_range_inverted() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        vterm.row_range(Line(2), Line(-1)).count();
    }

    #[test]
    #[should_panic]
    fn row_range_out_of_bounds_upper() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        vterm.row_range(Line(0), Line(100)).count();
    }

    #[test]
    #[should_panic]
    fn row_range_out_of_bounds_lower() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        vterm.row_range(Line(-100), Line(1)).count();
    }

    #[test]
    fn scrolline_range() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.move_history();
        fixture.process_bytes(b"line 6\r\nline 7\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(
            vec![
                (Line(-2), "line 4".to_string()),
                (Line(-1), "line 5".to_string()),
                (Line(0), "line 6".to_string()),
                (Line(1), "line 7".to_string()),
            ],
            vterm
                .scrolline_range(Scrolline(3), Scrolline(7))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn scrolline_range_with_wrapped_lines() {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 4);
        fixture.vterm.enable_scrollback();

        fixture.process_bytes(b"A long line....\r\nline 2\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(
            vec![
                (Line(0), "A long lin".to_string()),
                (Line(1), "e....".to_string()),
            ],
            vterm
                .scrolline_range(Scrolline(0), Scrolline(1))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![(Line(2), "line 2".to_string())],
            vterm
                .scrolline_range(Scrolline(1), Scrolline(2))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![
                (Line(0), "A long lin".to_string()),
                (Line(1), "e....".to_string()),
                (Line(2), "line 2".to_string()),
            ],
            vterm
                .scrolline_range(Scrolline(0), Scrolline(2))
                .map(|(l, r)| (l, row_to_string(r)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn scrolline_range_empty() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        assert_eq!(0, vterm.scrolline_range(Scrolline(1), Scrolline(1)).count());
        assert_eq!(0, vterm.scrolline_range(Scrolline(3), Scrolline(3)).count());
    }

    #[test]
    fn scrolline_range_end_beyond_available() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"line 1\r\nline 2\r\n");

        // return available rows up to the end
        let vterm = &fixture.vterm;
        let rows = vterm
            .scrolline_range(Scrolline(0), Scrolline(100))
            .map(|(l, r)| (l, row_to_string(r)))
            .collect::<Vec<_>>();
        assert_eq!(
            vec![
                (Line(0), "line 1".to_string()),
                (Line(1), "line 2".to_string()),
                (Line(2), "".to_string()),
            ],
            rows
        );
    }

    #[test]
    #[should_panic]
    fn scrolline_range_inverted() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");

        let vterm = &fixture.vterm;
        vterm.scrolline_range(Scrolline(3), Scrolline(1)).count();
    }

    #[test]
    #[should_panic]
    fn scrolline_range_before_base() {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5\r\n");
        fixture.vterm.clear_scrollback();

        let vterm = &fixture.vterm;
        assert_eq!(Scrolline(3), vterm.base_scrolline());
        vterm.scrolline_range(Scrolline(0), Scrolline(4)).count();
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
    fn collect_mode_changes_show_cursor() {
        let mut fixture = Fixture::setup();

        // Cursor is shown by default; enabling it again produces no change.
        fixture.process_bytes(b"\x1b[?25h");
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?25l");
        assert_eq!(
            vec![VTermChange::ShowCursor(false)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
            fixture.vterm.collect_mode_changes()
        );

        fixture.process_bytes(b"\x1b[?25h");
        assert_eq!(
            vec![VTermChange::ShowCursor(true)],
            fixture.vterm.collect_mode_changes()
        );
        assert_eq!(
            Vec::<VTermChange>::new(),
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

    #[test]
    fn process_bytes_multiple_prompt_sp() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes("a⏎         \r$ b⏎         \r$ ".as_bytes());

        assert_eq!("a⏎", row_to_string(&fixture.vterm.grid()[Line(0)]));
        assert_eq!("$ b⏎", row_to_string(&fixture.vterm.grid()[Line(1)]));
        assert_eq!("$ ", row_to_string(&fixture.vterm.grid()[Line(2)]));
    }

    #[test]
    fn track_modifications_above() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 4);
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4");

        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.vterm.track_modifications_above(Scrolline(2));
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[2;1HX"); // change line 1
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );
        // target is now scrolline 1

        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[1;1HY"); // change line 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );
        // tracking is stopped

        assert!(fixture.vterm.collect_changes()?.is_empty());
        fixture.process_bytes(b"\x1b[1;2HZ");
        assert!(fixture.vterm.collect_changes()?.is_empty());

        Ok(())
    }

    #[test]
    fn track_modifications_above_wrapped_line() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 5);
        fixture.process_bytes(b"0123456789wrapped\r\nshort 1\r\nshort 2");

        assert_eq!(
            Some(Line(0)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(2)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(3)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(2))
        );

        fixture.vterm.track_modifications_above(Scrolline(2));
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[2;1HX"); // row 1, scrolline 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );

        fixture.vterm.track_modifications_above(Scrolline(2));
        fixture.process_bytes(b"\x1b[1;1HY"); // row 0, scrolline 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_above_at_or_below_target_ignored() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 4);
        fixture.process_bytes(b"line 1\r\nline 2\r\nline 3\r\nline 4");

        fixture.vterm.track_modifications_above(Scrolline(1));

        // line 1 and 2 are modified. They're ignored.
        fixture.process_bytes(b"\x1b[2;1HX\x1b[3;1HY");
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[1;1HZ"); // line 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_above_earliest_modification_reported() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 5);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4");

        fixture.vterm.track_modifications_above(Scrolline(4));

        // Line 3 and 1 are modified, but only 1, the earliest, is reported.
        fixture.process_bytes(b"\x1b[4;1HA\x1b[2;1HB");

        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_restricted_multiple_times() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 6);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5");

        fixture.vterm.track_modifications_above(Scrolline(5));
        fixture.vterm.track_modifications_above(Scrolline(4));
        fixture.vterm.track_modifications_above(Scrolline(2));

        fixture.process_bytes(b"\x1b[3;1HX"); // line 2, ignored
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[2;1HY"); // line 1, detected
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_restriction_preserves_earlier_modifications() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 6);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5");

        fixture.vterm.track_modifications_above(Scrolline(5));

        fixture.process_bytes(b"\x1b[2;1HX"); // line 1

        fixture.vterm.track_modifications_above(Scrolline(4));
        fixture.vterm.track_modifications_above(Scrolline(3));

        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_expanded_multiple_times() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 6);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5");

        fixture.vterm.track_modifications_above(Scrolline(2));
        fixture.vterm.track_modifications_above(Scrolline(3));
        fixture.vterm.track_modifications_above(Scrolline(5));

        fixture.process_bytes(b"\x1b[4;1HX"); // line 3

        assert_eq!(
            vec![VTermChange::Modified(Scrolline(3))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_expansion_preserves_earlier_modifications() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 6);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5");

        fixture.vterm.track_modifications_above(Scrolline(2));
        fixture.process_bytes(b"\x1b[2;1HA"); // line 1

        fixture.vterm.track_modifications_above(Scrolline(4));
        fixture.process_bytes(b"\x1b[4;1HB"); // line 3

        fixture.vterm.track_modifications_above(Scrolline(5));
        fixture.process_bytes(b"\x1b[3;1HC"); // line 2

        // 1 is the earliest change, it is the one reported.
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_oscillates() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 6);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3\r\nline 4\r\nline 5");

        fixture.vterm.track_modifications_above(Scrolline(2));
        fixture.vterm.track_modifications_above(Scrolline(5));
        fixture.vterm.track_modifications_above(Scrolline(3));
        fixture.vterm.track_modifications_above(Scrolline(4));
        fixture.vterm.track_modifications_above(Scrolline(3));

        fixture.process_bytes(b"\x1b[4;1HX"); // line 3, igored
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[3;1HY"); // line 2
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(2))],
            fixture.vterm.collect_changes()?
        );

        fixture.process_bytes(b"\x1b[1;1HZ"); // line 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_changes_with_wrapped_lines() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 7);
        fixture.process_bytes(b"0123456789wrap0\r\n0123456789wrap1\r\nline 2\r\nline 3");
        assert_eq!(
            Some(Line(0)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(0))
        );
        assert_eq!(
            Some(Line(2)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(1))
        );
        assert_eq!(
            Some(Line(4)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(2))
        );
        assert_eq!(
            Some(Line(5)),
            fixture.vterm.terminal_line_for_scrolline(Scrolline(3))
        );

        fixture.vterm.track_modifications_above(Scrolline(1));
        fixture.vterm.track_modifications_above(Scrolline(3));
        fixture.vterm.track_modifications_above(Scrolline(2));

        fixture.process_bytes(b"\x1b[4;1HX"); // row 3, scrolline 1
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        fixture.process_bytes(b"\x1b[2;1HY"); // row 1, scrolline 0
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(0))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_changes_across_scrolling() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.enable_scrollback();
        fixture.vterm.resize(10, 3);

        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2");
        assert_eq!(Scrolline(0), fixture.vterm.scrolline_at_screen_top());

        fixture.vterm.track_modifications_above(Scrolline(2));

        // scroll screen
        fixture.process_bytes(b"\r\nline 3\r\nline 4\r\nline 5");
        let screen_top = fixture.vterm.scrolline_at_screen_top();
        assert_eq!(Scrolline(3), screen_top);

        fixture.vterm.track_modifications_above(Scrolline(6));

        fixture.process_bytes(b"\x1b[2;1HX"); // row 1, scrolline 4
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(4))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }

    #[test]
    fn track_modifications_target_changes_with_alt_screen() -> Result<()> {
        let mut fixture = Fixture::setup();
        fixture.vterm.resize(10, 4);
        fixture.process_bytes(b"line 0\r\nline 1\r\nline 2\r\nline 3");

        fixture.vterm.track_modifications_above(Scrolline(2));

        fixture.process_bytes(b"\x1b[?1049h"); // alt_screen on

        // clear mode changes
        fixture.vterm.collect_changes()?;

        fixture.vterm.track_modifications_above(Scrolline(3));

        // ignore modifications in alt screen
        fixture.process_bytes(b"\x1b[2;1HX");
        assert!(fixture.vterm.collect_changes()?.is_empty());

        fixture.process_bytes(b"\x1b[?1049l"); // alt_screen off
        let changes = fixture.vterm.collect_changes()?;
        assert!(changes.contains(&VTermChange::Fullscreen(false)));

        fixture.process_bytes(b"\x1b[2;1HY"); // line 1
        assert_eq!(
            vec![VTermChange::Modified(Scrolline(1))],
            fixture.vterm.collect_changes()?
        );

        Ok(())
    }
}
