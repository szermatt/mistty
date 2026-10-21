use alacritty_terminal::{index::Line, term::TermDamage};
use std::collections::BTreeSet;

/// Keep track of damage made to the grid.
///
/// This extends [alacritty_Terminal::TermDamage], which is private,
/// for changes made in this module.
pub(crate) struct GridDamage {
    damaged: BTreeSet<Line>,
}

impl GridDamage {
    pub(crate) fn new() -> Self {
        GridDamage {
            damaged: BTreeSet::new(),
        }
    }

    /// Register damaged line.
    pub(crate) fn add(&mut self, line: Line) {
        self.damaged.insert(line);
    }

    /// Return the set of lines to refresh, `None` to refresh the
    /// whole screen.
    ///
    /// This extends the damage extracted from the given
    /// [alacritty_terminal::TermDamage].
    pub(crate) fn damaged_lines(&mut self, damage: TermDamage) -> Option<Vec<Line>> {
        if let TermDamage::Partial(iter) = damage {
            let mut lines: Vec<Line> = iter.map(|d| Line(d.line as i32)).collect();
            lines.extend(self.damaged.iter());
            lines.sort_unstable();
            lines.dedup();
            // damage is sorted by line, one damage per line.

            Some(lines)
        } else {
            None
        }
    }

    /// Reset damage, so the next call to [GridDamage::damaged_lines]
    /// returns empty.
    pub(crate) fn reset(&mut self) {
        self.damaged.clear();
    }
}
