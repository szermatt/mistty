use crate::{
    gridext::{CellExt, RowExt},
    types::Scrolline,
    vterm::VTerm,
};
use alacritty_terminal::{
    Grid,
    grid::{Dimensions, Row},
    index::{Column, Line, Point},
    term::{
        TermMode,
        cell::{Cell, Flags},
    },
    vte::ansi::{NamedPrivateMode, PrivateMode},
};
use emacs::{IntoLisp, Result, Value};

emacs::use_functions! {
    append_fn => "append"
}

emacs::use_symbols! {
    osc133_sym => "osc133"
    start_prompt_sym => "start-prompt"
    end_prompt_sym => "end-prompt"
    update_prompt_sym => "update-prompt"
    prompt_sp_sym => "prompt_sp"
    bracketed_paste_sym => "bracketed-paste"
    regexp_sym => "regexp"
    colon_end_sym => ":end"
    colon_text_sym => ":text"
    colon_user_input_start_sym => ":user-input-start"
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum PromptChange {
    Start(PromptSource, Scrolline, Option<Scrolline>, PromptOptions),
    End(PromptSource, Scrolline),
    Update(PromptSource, PromptOptions),
}

#[derive(PartialEq, Eq, Debug, Default)]
pub(crate) struct PromptOptions {
    pub(crate) text: Option<String>,
    pub(crate) user_input_start: Option<(Scrolline, Column)>,
}

impl<'e> IntoLisp<'e> for PromptOptions {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        (&self).into_lisp(env)
    }
}
impl<'e> IntoLisp<'e> for &PromptOptions {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        let nil = ().into_lisp(env)?;

        env.call(
            append_fn,
            (
                if let Some(val) = &self.text {
                    env.list((colon_text_sym, val))?
                } else {
                    nil
                },
                if let Some((scrolline, col)) = &self.user_input_start {
                    env.list((colon_user_input_start_sym, env.cons(scrolline, col.0)?))?
                } else {
                    nil
                },
            ),
        )
    }
}

impl PromptChange {
    /// Return the start scrolline
    fn start(&self) -> Option<Scrolline> {
        match self {
            PromptChange::Start(_, start, _, _) => Some(*start),
            PromptChange::End(_, _) => None,
            PromptChange::Update(_, opts) => opts.user_input_start.map(|(s, _)| s),
        }
    }
}

impl<'e> IntoLisp<'e> for PromptChange {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        match self {
            PromptChange::Start(source, start, end, opts) => {
                let nil = ().into_lisp(env)?;
                env.call(
                    append_fn,
                    (
                        env.list((start_prompt_sym, source, start))?,
                        if let Some(end) = end {
                            env.list((colon_end_sym, end))?
                        } else {
                            nil
                        },
                        opts,
                    ),
                )
            }
            PromptChange::End(source, end) => env.list((end_prompt_sym, source, end)),
            PromptChange::Update(source, opts) => {
                env.call(append_fn, (env.list((update_prompt_sym, source))?, opts))
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum PromptSource {
    PromptSp,
    BracketedPaste,
    Regexp,
    Osc133,
}

impl<'e> IntoLisp<'e> for PromptSource {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        match self {
            PromptSource::PromptSp => Ok(prompt_sp_sym.bind(env)),
            PromptSource::BracketedPaste => Ok(bracketed_paste_sym.bind(env)),
            PromptSource::Regexp => Ok(regexp_sym.bind(env)),
            PromptSource::Osc133 => Ok(osc133_sym.bind(env)),
        }
    }
}

pub(crate) struct PromptDetector {
    /// Keep track of the length of the current space sequences.
    ///
    /// We're looking for the sequence: screen_columns * SPACE + CR
    /// sent by shells for the PROMPT_SP hack.
    seen_spaces: usize,

    last_prompt_scrolline: Option<Scrolline>,

    prompts: Vec<PromptChange>,
}

impl PromptDetector {
    pub(crate) fn new() -> Self {
        PromptDetector {
            seen_spaces: 0,
            last_prompt_scrolline: None,
            prompts: vec![],
        }
    }

    /// Return the prompts that were detected since last collected.
    pub(crate) fn collect(&mut self) -> Vec<PromptChange> {
        std::mem::take(&mut self.prompts)
    }

    /// Add a new prompt and update `last_prompt_scrolline`.
    fn push(&mut self, prompt: PromptChange) {
        self.last_prompt_scrolline = prompt.start();
        self.prompts.push(prompt);
    }

    /// Detect the PROMPT_SP hack and return the position at which
    /// handle_prompt_sp should be called.
    pub(crate) fn detect_prompt_sp(
        &mut self,
        mode: TermMode,
        screen_columns: usize,
        bytes: &[u8],
    ) -> Option<usize> {
        if mode.contains(TermMode::ALT_SCREEN) {
            self.seen_spaces = 0;
            return None;
        }

        for (index, c) in bytes.iter().enumerate() {
            match *c {
                b' ' => {
                    self.seen_spaces += 1;
                }
                b'\r' => {
                    if self.seen_spaces >= (screen_columns - 1) {
                        self.seen_spaces = 0;

                        return Some(index);
                    }
                }
                _ => {
                    self.seen_spaces = 0;
                }
            }
        }

        None
    }

    /// This is called just before a CR is received in the PROMPT_SP hack.
    ///
    /// This function cleans up the terminal after a trick used to detect output
    /// that doesn't end in a newline is called prompt sp. That trick consists
    /// of outputing an optional end-of-line marker, then columns-1 spaces and a
    /// CR. If we end up still on the same line, the output ended with a NL and
    /// the whole line, and the marker, is then overwritten. If we end up on
    /// another line due to line wrap, the previous line and the marker stay in
    /// the previous line.
    ///
    /// This results in a continuation line that shouldn't be continued and a
    /// large number of newlines, both of which will look bad when they enter
    /// scrollback.
    ///
    /// To work around it, this call transform the fake newline into a real one
    /// and marks the spaces at the end of the previous line as blank.
    ///
    /// The cursor should should be at the position where the CR is
    /// called in the prompt-sp sequence.
    pub(crate) fn handle_prompt_sp(&mut self, vterm: &mut VTerm) {
        cleanup_prompt_sp(vterm);

        let scrolline = vterm.scrolline_at_cursor();
        self.last_prompt_scrolline = Some(scrolline);
        self.push(PromptChange::Start(
            PromptSource::PromptSp,
            scrolline,
            None,
            PromptOptions::default(),
        ));
    }

    /// React to bracketed paste being set or unset.
    pub(crate) fn set_bracketed_paste(&mut self, vterm: &mut VTerm, value: bool) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }

        if value && !vterm.mode().intersects(TermMode::BRACKETED_PASTE) {
            let scrolline = vterm.scrolline_at_cursor();
            self.last_prompt_scrolline = Some(scrolline);
            self.prompts.push(PromptChange::Start(
                PromptSource::BracketedPaste,
                scrolline,
                None,
                PromptOptions::default(),
            ));
            return;
        }
        if !value && vterm.mode().intersects(TermMode::BRACKETED_PASTE) {
            let scrolline = if vterm.grid().cursor.point.column.0 == 0 {
                vterm.scrolline_at_cursor()
            } else {
                vterm.scrolline_at_cursor() + 1
            };
            self.push(PromptChange::End(PromptSource::BracketedPaste, scrolline));
        }
    }

    /// Detect a prompt matching `mistty--prompt-regexp` at the cursor.
    pub(crate) fn detect_regexp_prompt(&mut self, vterm: &VTerm) {
        if vterm
            .mode()
            .intersects(TermMode::ALT_SCREEN | TermMode::BRACKETED_PASTE)
        {
            return;
        }

        let grid = vterm.grid();
        let cursor = grid.cursor.point;

        // reject continuation lines
        if cursor.line > Line(0) {
            let prevrow: &Row<Cell> = &grid[cursor.line - 1];
            if prevrow.is_wrapped() {
                return;
            }
        }
        // before cursor must looks like the end of a prompt
        if !may_be_prompt_end(&grid[cursor.line], cursor.column) {
            return;
        }

        // ignores scrollines already considered
        let cursor_scrolline = vterm.scrolline_at_cursor();
        if self
            .last_prompt_scrolline
            .map(|s| s >= cursor_scrolline)
            .unwrap_or(false)
        {
            return;
        }
        // there must be nothing after cursor
        if !all_clear_after(grid, cursor) {
            return;
        }

        // Register a regexp prompt
        let mut text = String::new();
        for (_, cell) in grid[cursor.line].ranged_iter(Column(0), cursor.column) {
            cell.append_content(&mut text);
        }
        self.push(PromptChange::Start(
            PromptSource::Regexp,
            cursor_scrolline,
            Some(cursor_scrolline + 1),
            PromptOptions {
                text: Some(text),
                ..PromptOptions::default()
            },
        ));
    }

    pub(crate) fn osc133_prompt_start(&mut self, vterm: &mut VTerm, click_events: bool) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }
        self.push(PromptChange::Start(
            PromptSource::Osc133,
            vterm.scrolline_at_cursor(),
            None,
            PromptOptions::default(),
        ));
        if click_events {
            vterm.set_private_mode(PrivateMode::Named(NamedPrivateMode::ReportMouseClicks));
            vterm.set_private_mode(PrivateMode::Named(NamedPrivateMode::SgrMouse));
        } else {
            vterm.unset_private_mode(PrivateMode::Named(NamedPrivateMode::ReportMouseClicks));
        }
    }

    pub(crate) fn osc133_command_start(&mut self, vterm: &VTerm) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }
        self.push(PromptChange::Update(
            PromptSource::Osc133,
            PromptOptions {
                user_input_start: Some((
                    vterm.scrolline_at_cursor(),
                    vterm.grid().cursor.point.column,
                )),
                ..PromptOptions::default()
            },
        ));
    }

    pub(crate) fn osc133_command_end(&mut self, vterm: &mut VTerm) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }
        self.push(PromptChange::End(
            PromptSource::Osc133,
            vterm.scrolline_at_cursor(),
        ));
        vterm.unset_private_mode(PrivateMode::Named(NamedPrivateMode::ReportMouseClicks));
    }
}

/// Cleanup after a shell's PROMPT-SP hack.
///
/// This change marks the spaces issued by the hack as blank, so as not to
/// confuse the display.
pub(crate) fn cleanup_prompt_sp(vterm: &mut VTerm) {
    let grid = vterm.grid_mut();
    let cursor_line = grid.cursor.point.line;
    if cursor_line == Line(0) {
        return;
    }

    let cursor_col = grid.cursor.point.column;
    let last_column = grid.last_column();
    let mut damaged_prev_line = false;
    let prev_line: Line = cursor_line - 1;
    if cursor_col != last_column {
        let prev_row = &mut grid[prev_line];
        if prev_row.is_wrapped() {
            prev_row[last_column].flags.remove(Flags::WRAPLINE);
            blank_trailing(prev_row);
            damaged_prev_line = true;
        }
    }

    let row = &mut grid[cursor_line];
    blank_trailing(row);

    vterm.add_damage(cursor_line);
    if damaged_prev_line {
        vterm.add_damage(prev_line);
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

/// Check whether col may be at prompt end.
///
/// A prompt end looks like "$ " or "> " or any non-alphanumeric
/// character followed by a space, followed by the cursor.
pub(crate) fn may_be_prompt_end(row: &Row<Cell>, col: Column) -> bool {
    if col < 2 {
        return false;
    }
    let prev1 = &row[col - 1];
    if prev1.is_clear() || prev1.content_string() != " " {
        return false;
    }
    let prev2 = &row[col - 2];
    if prev2.c.is_alphanumeric() || prev2.c.is_control() || prev2.c.is_whitespace() {
        return false;
    }

    true
}

/// Check whether anything is non-clear after the given point.
fn all_clear_after(grid: &Grid<Cell>, point: Point) -> bool {
    for cell in grid.iter_from(point) {
        if !cell.is_clear() {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing;
    use crate::vterm::{VTerm, VTermProcessor};
    use alacritty_terminal::term::Osc52;

    struct Fixture {
        vterm: VTerm,
        processor: VTermProcessor,
    }

    impl Fixture {
        fn setup() -> anyhow::Result<Self> {
            let vterm = VTerm::new(20, 10, Osc52::Disabled);
            let processor = VTermProcessor::new();
            Ok(Self { vterm, processor })
        }

        fn process_bytes(&mut self, bytes: &[u8]) {
            self.processor.process_bytes(&mut self.vterm, bytes);
        }

        fn row_to_string(&self, line: Line) -> String {
            testing::row_to_string(&self.vterm.grid()[line])
        }
    }

    #[test]
    fn detect_prompt_sp_full_line() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes("text\r\n⏎                   \r$ ".as_bytes());

        assert_eq!("$ ", fixture.row_to_string(Line(1)));
        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::PromptSp,
                Scrolline(1),
                None,
                PromptOptions::default()
            )],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_prompt_sp_split_line() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes("text⏎                   \r$ ".as_bytes());

        assert_eq!("text⏎", fixture.row_to_string(Line(0)));
        assert_eq!("$ ", fixture.row_to_string(Line(1)));
        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::PromptSp,
                Scrolline(1),
                None,
                PromptOptions::default()
            )],
            fixture.processor.collect_prompt_changes()
        );

        Ok(())
    }

    #[test]
    fn detect_multiple_prompts() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes("text\r\n⏎                   \r$ ".as_bytes());
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes("⏎                   \r$ ".as_bytes());

        assert_eq!("text", fixture.row_to_string(Line(0)));
        assert_eq!("$ echo ok", fixture.row_to_string(Line(1)));
        assert_eq!("ok", fixture.row_to_string(Line(2)));
        assert_eq!("$ ", fixture.row_to_string(Line(3)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::PromptSp,
                    Scrolline(1),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::Start(
                    PromptSource::PromptSp,
                    Scrolline(3),
                    None,
                    PromptOptions::default()
                )
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_bracketed_paste() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b[?2004h$ ");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b[?2004l");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes(b"\x1b[?2004h$ ");

        assert_eq!("$ echo ok", fixture.row_to_string(Line(0)));
        assert_eq!("ok", fixture.row_to_string(Line(1)));
        assert_eq!("$ ", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::BracketedPaste,
                    Scrolline(0),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::End(PromptSource::BracketedPaste, Scrolline(1)),
                PromptChange::Start(
                    PromptSource::BracketedPaste,
                    Scrolline(2),
                    None,
                    PromptOptions::default()
                ),
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_bracketed_paste_end_before_nl() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b[?2004h$ ");
        fixture.process_bytes(b"echo ok");
        // bracketed pasted is ended before the final newline of the
        // prompt is sent.
        fixture.process_bytes(b"\x1b[?2004l\r\n");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes(b"\x1b[?2004h$ ");

        assert_eq!("$ echo ok", fixture.row_to_string(Line(0)));
        assert_eq!("ok", fixture.row_to_string(Line(1)));
        assert_eq!("$ ", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::BracketedPaste,
                    Scrolline(0),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::End(PromptSource::BracketedPaste, Scrolline(1)),
                PromptChange::Start(
                    PromptSource::BracketedPaste,
                    Scrolline(2),
                    None,
                    PromptOptions::default()
                ),
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_bracketed_paste_multiline() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b[?2004hmyprompt\r\n> ");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b[?2004l");
        fixture.process_bytes(b"ok\r\n");

        assert_eq!("myprompt", fixture.row_to_string(Line(0)));
        assert_eq!("> echo ok", fixture.row_to_string(Line(1)));
        assert_eq!("ok", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::BracketedPaste,
                    Scrolline(0),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::End(PromptSource::BracketedPaste, Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_simple() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"$ ");

        assert_eq!("$ ", fixture.row_to_string(Line(0)));
        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::Regexp,
                Scrolline(0),
                Some(Scrolline(1)),
                PromptOptions {
                    text: Some("$ ".to_string()),
                    ..PromptOptions::default()
                }
            )],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_custom_text() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"say something>> ");

        assert_eq!("say something>> ", fixture.row_to_string(Line(0)));
        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::Regexp,
                Scrolline(0),
                Some(Scrolline(1)),
                PromptOptions {
                    text: Some("say something>> ".to_string()),
                    ..PromptOptions::default()
                }
            )],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_multiline_output() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"line 1\r\nline 2\r\n> ");

        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::Regexp,
                Scrolline(2),
                Some(Scrolline(3)),
                PromptOptions {
                    text: Some("> ".to_string()),
                    ..PromptOptions::default()
                }
            )],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_ignores_non_prompt() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"hello world\r\n");

        assert_eq!(
            Vec::<PromptChange>::new(),
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_ignores_text_after_cursor() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        // Cursor moves back 3 characters over 'foo'
        fixture.process_bytes(b"$ foo\x1b[3D");

        let cursor = fixture.vterm.grid().cursor.point;
        assert!(may_be_prompt_end(
            &fixture.vterm.grid()[cursor.line],
            cursor.column
        ));
        assert_eq!(
            Vec::<PromptChange>::new(),
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_regexp_prompt_only_once_per_line() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"$ ");
        assert_eq!(
            vec![PromptChange::Start(
                PromptSource::Regexp,
                Scrolline(0),
                Some(Scrolline(1)),
                PromptOptions {
                    text: Some("$ ".to_string()),
                    ..PromptOptions::default()
                }
            )],
            fixture.processor.collect_prompt_changes()
        );

        // Typing more on the same line should not detect again
        fixture.process_bytes(b"echo");
        assert_eq!(
            Vec::<PromptChange>::new(),
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_osc133() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b]133;A\x07$ \x1b]133;B\x07");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b]133;C\x07");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes(b"\x1b]133;D;0\x07");
        fixture.process_bytes(b"\x1b]133;A\x1b\\$ \x1b]133;B\x1b\\");

        assert_eq!("$ echo ok", fixture.row_to_string(Line(0)));
        assert_eq!("ok", fixture.row_to_string(Line(1)));
        assert_eq!("$ ", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::Osc133,
                    Scrolline(0),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::Update(
                    PromptSource::Osc133,
                    PromptOptions {
                        user_input_start: Some((Scrolline(0), Column(2))),
                        ..PromptOptions::default()
                    }
                ),
                PromptChange::End(PromptSource::Osc133, Scrolline(1)),
                PromptChange::End(PromptSource::Osc133, Scrolline(2)),
                PromptChange::Start(
                    PromptSource::Osc133,
                    Scrolline(2),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::Update(
                    PromptSource::Osc133,
                    PromptOptions {
                        user_input_start: Some((Scrolline(2), Column(2))),
                        ..PromptOptions::default()
                    }
                ),
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_osc133_multiline() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b]133;A\x07myprompt\r\n> \x1b]133;B\x07");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b]133;C\x07");

        assert_eq!("myprompt", fixture.row_to_string(Line(0)));
        assert_eq!("> echo ok", fixture.row_to_string(Line(1)));

        assert_eq!(
            vec![
                PromptChange::Start(
                    PromptSource::Osc133,
                    Scrolline(0),
                    None,
                    PromptOptions::default()
                ),
                PromptChange::Update(
                    PromptSource::Osc133,
                    PromptOptions {
                        user_input_start: Some((Scrolline(1), Column(2))),
                        ..PromptOptions::default()
                    }
                ),
                PromptChange::End(PromptSource::Osc133, Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
        Ok(())
    }

    #[test]
    fn detect_osc133_click_events() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));

        fixture.process_bytes(b"\x1b]133;A;click_events=1\x07$ \x1b]133;B\x07");
        assert!(fixture
            .vterm
            .mode()
            .contains(TermMode::MOUSE_REPORT_CLICK | TermMode::SGR_MOUSE));

        fixture.process_bytes(b"echo ok\r\n\x1b]133;C\x07");
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));

        fixture.process_bytes(b"\x1b]133;A;click_events=1\x07$ ");
        assert!(fixture
            .vterm
            .mode()
            .contains(TermMode::MOUSE_REPORT_CLICK | TermMode::SGR_MOUSE));

        fixture.process_bytes(b"\x1b]133;A\x07$ ");
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));

        Ok(())
    }

    #[test]
    fn detect_osc133_ignores_alt_screen() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture.process_bytes(b"\x1b[?1049h");
        assert!(fixture.vterm.mode().contains(TermMode::ALT_SCREEN));

        fixture.process_bytes(b"\x1b]133;A;click_events=1\x07$ \x1b]133;B\x07echo ok\r\n\x1b]133;C\x07");
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));
        assert_eq!(
            Vec::<PromptChange>::new(),
            fixture.processor.collect_prompt_changes()
        );

        Ok(())
    }
}
