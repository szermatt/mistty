use crate::{gridext::RowExt, types::Scrolline, vterm::VTerm};
use alacritty_terminal::{
    grid::{Dimensions, Row},
    index::{Column, Line},
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
    bracketed_paste_sym => "bracketed-paste"
    colon_end_sym => ":end"
    colon_user_input_start_sym => ":user-input-start"
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum PromptChange {
    /// Report a prompt start. If the current prompt isn't ended yet,
    /// this replaces the current prompt.
    Start(PromptSource, Scrolline),

    /// End the current prompt, identified by its id.
    End(Scrolline),

    /// Update the current prompt, identified by its id.
    Update(PromptOptions),
}

#[derive(PartialEq, Eq, Debug, Default)]
pub(crate) struct PromptOptions {
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
            (if let Some((scrolline, col)) = &self.user_input_start {
                env.list((colon_user_input_start_sym, env.cons(scrolline, col.0)?))?
            } else {
                nil
            },),
        )
    }
}

impl<'e> IntoLisp<'e> for PromptChange {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        match self {
            PromptChange::Start(source, start) => env.list((start_prompt_sym, source, start)),
            PromptChange::End(end) => env.list((end_prompt_sym, end)),
            PromptChange::Update(opts) => env.cons(update_prompt_sym, opts),
        }
    }
}

/// Prompt source, ordered from less to most trustworthy.
#[derive(Copy, Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub(crate) enum PromptSource {
    BracketedPaste,
    Osc133,
}

impl<'e> IntoLisp<'e> for PromptSource {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        match self {
            PromptSource::BracketedPaste => Ok(bracketed_paste_sym.bind(env)),
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

    /// Scrolline at which the last PROMPT_SP hack was seen. This is
    /// the line below the one PROMPT_SP sign would be seen.
    last_prompt_sp: Scrolline,

    /// Detected prompts, from the most ancient to the most recent.
    ///
    /// Add new prompts using PromptDetector::push.
    prompts: Vec<PromptChange>,

    /// Current prompt
    current: Option<Prompt>,
}

/// Internal struct used to keep track of the current prompt.
struct Prompt {
    source: PromptSource,
    start: Scrolline,
}

impl PromptDetector {
    pub(crate) fn new() -> Self {
        PromptDetector {
            seen_spaces: 0,
            prompts: vec![],
            last_prompt_sp: Scrolline(0),
            current: None,
        }
    }

    /// Return the prompts that were detected since last collected.
    pub(crate) fn collect(&mut self) -> Vec<PromptChange> {
        std::mem::take(&mut self.prompts)
    }

    /// Add a new prompt and update `last_prompt_scrolline`.
    fn push(&mut self, prompt: PromptChange) {
        self.prompts.push(prompt);
    }

    /// Detect the PROMPT_SP hack and return the position at which
    /// handle_prompt_sp should be called.
    pub(crate) fn detect_prompt_sp(
        &mut self,
        screen_columns: usize,
        bytes: &[u8],
    ) -> Option<usize> {
        // This logic must be executed even when mode is ALT_SCREEN,
        // because we don't know when in the bytes array it's going to
        // be switched. The right point to check that the mode is in
        // handle_prompt_sp.
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
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }

        cleanup_prompt_sp(vterm);

        self.last_prompt_sp = vterm.scrolline_at_cursor();
    }

    /// React to bracketed paste being set or unset.
    pub(crate) fn set_bracketed_paste(&mut self, vterm: &mut VTerm, value: bool) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }

        if value && !vterm.mode().intersects(TermMode::BRACKETED_PASTE) {
            let mut start = vterm.scrolline_at_cursor();

            // Handle the ZSH case, where BRACKETED_PASTE is turned on
            // where user input is expected and not at the beginning of
            // the prompt, like Bash or Fish.
            if self.last_prompt_sp < start
                && (start - self.last_prompt_sp) <= 2
                && vterm.grid().cursor.point.column.0 > 0
            {
                start = self.last_prompt_sp;
            }

            self.report_prompt(PromptSource::BracketedPaste, start);
            return;
        }
        if !value && vterm.mode().intersects(TermMode::BRACKETED_PASTE) {
            let scrolline = if vterm.grid().cursor.point.column.0 == 0 {
                vterm.scrolline_at_cursor()
            } else {
                vterm.scrolline_at_cursor() + 1
            };
            self.end_prompt(PromptSource::BracketedPaste, scrolline);
        }
    }

    pub(crate) fn osc133_prompt_start(&mut self, vterm: &mut VTerm, click_events: bool) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }
        self.report_prompt(PromptSource::Osc133, vterm.scrolline_at_cursor());
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
        if let Some(_) = &self.current {
            self.push(PromptChange::Update(PromptOptions {
                user_input_start: Some((
                    vterm.scrolline_at_cursor(),
                    vterm.grid().cursor.point.column,
                )),
                ..PromptOptions::default()
            }));
        }
    }

    pub(crate) fn osc133_command_end(&mut self, vterm: &mut VTerm) {
        if vterm.mode().contains(TermMode::ALT_SCREEN) {
            return;
        }
        self.end_prompt(PromptSource::Osc133, vterm.scrolline_at_cursor());
        vterm.unset_private_mode(PrivateMode::Named(NamedPrivateMode::ReportMouseClicks));
    }

    fn report_prompt(&mut self, source: PromptSource, start: Scrolline) {
        match &mut self.current {
            Some(current) if current.source < source => {
                // Overwrite
                current.source = source;
                current.start = start;
                self.push(PromptChange::Start(source, start));
            }
            Some(current) if current.source == source && start > current.start => {
                // Replace
                self.push(PromptChange::End(start));

                self.current = Some(Prompt { source, start });
                self.push(PromptChange::Start(source, start));
            }
            None => {
                // Create new
                self.current = Some(Prompt { source, start });
                self.push(PromptChange::Start(source, start));
            }
            _ => {} // Ignore
        }
    }

    fn end_prompt(&mut self, source: PromptSource, end: Scrolline) {
        if let Some(_) = self.current.take_if(|p| p.source == source) {
            self.push(PromptChange::End(end))
        }
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
        fn setup() -> Self {
            let vterm = VTerm::new(20, 10, Osc52::Disabled);
            let processor = VTermProcessor::new();
            Self { vterm, processor }
        }

        fn process_bytes(&mut self, bytes: &[u8]) {
            self.processor.process_bytes(&mut self.vterm, bytes);
        }

        fn row_to_string(&self, line: Line) -> String {
            testing::row_to_string(&self.vterm.grid()[line])
        }
    }

    #[test]
    fn fix_prompt_sp_full_line() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes("text\r\n⏎                   \r$ ".as_bytes());

        assert_eq!("$ ", fixture.row_to_string(Line(1)));
    }

    #[test]
    fn fix_prompt_sp_split_line() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes("text⏎                   \r$ ".as_bytes());

        assert_eq!("text⏎", fixture.row_to_string(Line(0)));
        assert_eq!("$ ", fixture.row_to_string(Line(1)));
    }

    #[test]
    fn detect_bracketed_paste() {
        let mut fixture = Fixture::setup();
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
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::End(Scrolline(1)),
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_bracketed_paste_after_prompt_sp() {
        // This is what happens with zsh: first a prompt_sp, then the
        // prompt, and then, just before user input is expected,
        // bracketed paste is turned on. There is of course no
        // prompt_sp before the first prompt.
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"$ \x1b[?2004h");
        fixture.process_bytes(b"echo ok");
        fixture.process_bytes(b"\x1b[?2004l\r\n");
        fixture.process_bytes(b"ok\r\n#                   \r$ ");
        fixture.process_bytes(b"\x1b[?2004h");

        assert_eq!("$ echo ok", fixture.row_to_string(Line(0)));
        assert_eq!("ok", fixture.row_to_string(Line(1)));
        assert_eq!("$ ", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::End(Scrolline(1)),
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_bracketed_paste_after_prompt_sp_multiline() {
        // This is what happens with zsh: first a prompt_sp, then the
        // prompt, and then, just before user input is expected,
        // bracketed paste is turned on. There is of course no
        // prompt_sp before the first prompt.
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"prompt\r\n$ \x1b[?2004h");
        fixture.process_bytes(b"echo ok");
        fixture.process_bytes(b"\x1b[?2004l\r\n");
        fixture.process_bytes(b"ok\r\n#                   \rprompt\r\n$ ");
        fixture.process_bytes(b"\x1b[?2004h");

        assert_eq!("prompt", fixture.row_to_string(Line(0)));
        assert_eq!("$ echo ok", fixture.row_to_string(Line(1)));
        assert_eq!("ok", fixture.row_to_string(Line(2)));
        assert_eq!("prompt", fixture.row_to_string(Line(3)));
        assert_eq!("$ ", fixture.row_to_string(Line(4)));

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::End(Scrolline(2)),
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(3)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_bracketed_paste_end_before_nl() {
        let mut fixture = Fixture::setup();
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
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::End(Scrolline(1)),
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_bracketed_paste_multiline() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"\x1b[?2004hmyprompt\r\n> ");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b[?2004l");
        fixture.process_bytes(b"ok\r\n");

        assert_eq!("myprompt", fixture.row_to_string(Line(0)));
        assert_eq!("> echo ok", fixture.row_to_string(Line(1)));
        assert_eq!("ok", fixture.row_to_string(Line(2)));

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::End(Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133() {
        let mut fixture = Fixture::setup();
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
                PromptChange::Start(PromptSource::Osc133, Scrolline(0)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(0), Column(2))),
                    ..PromptOptions::default()
                }),
                PromptChange::End(Scrolline(1)),
                PromptChange::Start(PromptSource::Osc133, Scrolline(2)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(2), Column(2))),
                    ..PromptOptions::default()
                }),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133_multiline() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"\x1b]133;A\x07myprompt\r\n> \x1b]133;B\x07");
        fixture.process_bytes(b"echo ok\r\n");
        fixture.process_bytes(b"\x1b]133;C\x07");

        assert_eq!("myprompt", fixture.row_to_string(Line(0)));
        assert_eq!("> echo ok", fixture.row_to_string(Line(1)));

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::Osc133, Scrolline(0)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(1), Column(2))),
                    ..PromptOptions::default()
                }),
                PromptChange::End(Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133_replace_unended_prompt() {
        let mut fixture = Fixture::setup();
        // First prompt starts on line 0, never ended with C or D (e.g. Ctrl-C)
        fixture.process_bytes(b"\x1b]133;A\x07$ \x1b]133;B\x07^C\r\n");
        // Duplicate A on the same line is ignored, A on a new line replaces prompt 1
        fixture.process_bytes(b"\x1b]133;A\x07\x1b]133;A\x07$ \x1b]133;B\x07");

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::Osc133, Scrolline(0)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(0), Column(2))),
                    ..PromptOptions::default()
                }),
                PromptChange::End(Scrolline(1)),
                PromptChange::Start(PromptSource::Osc133, Scrolline(1)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(1), Column(2))),
                    ..PromptOptions::default()
                }),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133_created_before_bracketed_paste() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"\x1b]133;A\x07myprompt\r\n$ \x1b]133;B\x07\x1b[?2004h");
        fixture.process_bytes(b"echo ok\x1b[?2004l\r\n");
        fixture.process_bytes(b"\x1b]133;C\x07");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes(b"\x1b]133;D;0\x07");

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::Osc133, Scrolline(0)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(1), Column(2))),
                    ..PromptOptions::default()
                }),
                PromptChange::End(Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133_overwrites_bracketed_paste() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"\x1b[?2004hmyprompt\r\n\x1b]133;A\x07$ \x1b]133;B\x07");
        fixture.process_bytes(b"echo ok\x1b[?2004l\r\n");
        fixture.process_bytes(b"\x1b]133;C\x07");
        fixture.process_bytes(b"ok\r\n");
        fixture.process_bytes(b"\x1b]133;D;0\x07");

        assert_eq!(
            vec![
                PromptChange::Start(PromptSource::BracketedPaste, Scrolline(0)),
                PromptChange::Start(PromptSource::Osc133, Scrolline(1)),
                PromptChange::Update(PromptOptions {
                    user_input_start: Some((Scrolline(1), Column(2))),
                    ..PromptOptions::default()
                }),
                PromptChange::End(Scrolline(2)),
            ],
            fixture.processor.collect_prompt_changes()
        );
    }

    #[test]
    fn detect_osc133_click_events() {
        let mut fixture = Fixture::setup();
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));

        fixture.process_bytes(b"\x1b]133;A;click_events=1\x07$ \x1b]133;B\x07");
        assert!(
            fixture
                .vterm
                .mode()
                .contains(TermMode::MOUSE_REPORT_CLICK | TermMode::SGR_MOUSE)
        );

        fixture.process_bytes(b"echo ok\r\n\x1b]133;C\x07");
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));

        fixture.process_bytes(b"\x1b]133;A;click_events=1\x07$ ");
        assert!(
            fixture
                .vterm
                .mode()
                .contains(TermMode::MOUSE_REPORT_CLICK | TermMode::SGR_MOUSE)
        );

        fixture.process_bytes(b"\x1b]133;A\x07$ ");
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));
    }

    #[test]
    fn detect_osc133_ignores_alt_screen() {
        let mut fixture = Fixture::setup();
        fixture.process_bytes(b"\x1b[?1049h");
        assert!(fixture.vterm.mode().contains(TermMode::ALT_SCREEN));

        fixture.process_bytes(
            b"\x1b]133;A;click_events=1\x07$ \x1b]133;B\x07echo ok\r\n\x1b]133;C\x07",
        );
        assert!(!fixture.vterm.mode().contains(TermMode::MOUSE_REPORT_CLICK));
        assert_eq!(
            Vec::<PromptChange>::new(),
            fixture.processor.collect_prompt_changes()
        );
    }
}
