use alacritty_terminal::{
    Grid,
    grid::{Dimensions, Row},
    index::{Column, Line},
    term::cell::{Cell, Flags},
};
use emacs::{IntoLisp, Result, Value};

use crate::{
    damage::GridDamage,
    gridext::{GridExt, RowExt},
    types::Scrolline,
};

emacs::use_symbols! {
    prompt_sym => "prompt"
    prompt_sp_sym => "prompt_sp"
    colon_source_sym => ":source"
    colon_start_sym => ":start"
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct Prompt {
    source: PromptSource,
    start: Scrolline,
}

impl<'e> IntoLisp<'e> for Prompt {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        env.list((
            prompt_sym,
            colon_source_sym,
            self.source,
            colon_start_sym,
            self.start,
        ))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub(crate) enum PromptSource {
    PromptSp,
}

impl<'e> IntoLisp<'e> for PromptSource {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<Value<'e>> {
        match self {
            PromptSource::PromptSp => Ok(prompt_sp_sym.bind(env)),
        }
    }
}

pub(crate) struct PromptDetector {
    /// Keep track of the length of the current space sequences.
    ///
    /// We're looking for the sequence: screen_columns * SPACE + CR
    /// sent by shells for the PROMPT_SP hack.
    seen_spaces: usize,

    captured: Option<Prompt>,
}
impl PromptDetector {
    pub(crate) fn new() -> Self {
        PromptDetector {
            seen_spaces: 0,
            captured: None,
        }
    }

    /// Take the prompt that was detected.
    pub(crate) fn take(&mut self) -> Option<Prompt> {
        self.captured.take()
    }

    /// Detect the PROMPT_SP hack and return the position at which
    /// handle_prompt_sp should be called.
    pub(crate) fn detect_prompt_sp(
        &mut self,
        screen_columns: usize,
        bytes: &[u8],
    ) -> Option<usize> {
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
    // that doesn't end in a newline is called prompt sp. That trick consists
    // of outputing an optional end-of-line marker, then columns-1 spaces and a
    // CR. If we end up still on the same line, the output ended with a NL and
    // the whole line, and the marker, is then overwritten. If we end up on
    // another line due to line wrap, the previous line and the marker stay in
    // the previous line.
    //
    // This results in a continuation line that shouldn't be continued and a
    // large number of newlines, both of which will look bad when they enter
    // scrollback.
    //
    // To work around it, this call transform the fake newline into a real one
    // and marks the spaces at the end of the previous line as blank.
    //
    // The cursor should should be at the position where the CR is
    // called in the prompt-sp sequence.
    pub(crate) fn handle_prompt_sp(
        &mut self,
        screen_top_scrolline: Scrolline,
        grid: &mut Grid<Cell>,
        damage: &mut GridDamage,
    ) {
        cleanup_prompt_sp(grid, damage);

        self.captured = Some(Prompt {
            source: PromptSource::PromptSp,
            start: screen_top_scrolline + grid.count_scrollines(Line(0), grid.cursor.point.line),
        });
    }
}

/// Cleanup after a shell's PROMPT-SP hack.
///
/// This change marks the spaces issued by the hack as blank, so as not to
/// confuse the display.
pub(crate) fn cleanup_prompt_sp(grid: &mut Grid<Cell>, damage: &mut GridDamage) {
    let cursor_line = grid.cursor.point.line;
    if cursor_line == Line(0) {
        return;
    }

    let cursor_col = grid.cursor.point.column;
    let last_column = grid.last_column();
    if cursor_col != last_column {
        let prev_line: Line = cursor_line - 1;
        let prev_row = &mut grid[prev_line];
        if prev_row.is_wrapped() {
            prev_row[last_column].flags.remove(Flags::WRAPLINE);
            blank_trailing(prev_row);
            damage.add(prev_line);
        }
    }

    let row = &mut grid[cursor_line];
    blank_trailing(row);
    damage.add(cursor_line);
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
    use crate::vterm::VTerm;
    use alacritty_terminal::term::Osc52;

    struct Fixture {
        vterm: VTerm,
    }

    impl Fixture {
        fn setup() -> anyhow::Result<Self> {
            let vterm = VTerm::new(20, 10, Osc52::Disabled);
            Ok(Self { vterm })
        }

        fn row_to_string(&self, line: Line) -> String {
            testing::row_to_string(&self.vterm.grid()[line])
        }
    }

    #[test]
    fn detect_prompt_sp_full_line() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture
            .vterm
            .process_bytes("text\r\n⏎                   \r$ ".as_bytes());

        assert_eq!("$ ", fixture.row_to_string(Line(1)));
        assert_eq!(
            Some(Prompt {
                source: PromptSource::PromptSp,
                start: Scrolline(1)
            }),
            fixture.vterm.take_prompt()
        );
        Ok(())
    }

    #[test]
    fn detect_prompt_sp_split_line() -> anyhow::Result<()> {
        let mut fixture = Fixture::setup()?;
        fixture
            .vterm
            .process_bytes("text⏎                   \r$ ".as_bytes());

        assert_eq!("text⏎", fixture.row_to_string(Line(0)));
        assert_eq!("$ ", fixture.row_to_string(Line(1)));
        assert_eq!(
            Some(Prompt {
                source: PromptSource::PromptSp,
                start: Scrolline(1)
            }),
            fixture.vterm.take_prompt()
        );

        Ok(())
    }
}
