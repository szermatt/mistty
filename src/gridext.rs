//! Define convenience function for working with
// [alacritty_terminal::term::Grid] and associated types.

use alacritty_terminal::Grid;
use alacritty_terminal::grid::{Dimensions, Row};
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::term::cell::{Cell, Flags};

pub trait GridExt {
    /// Iterate through all the rows of history.
    ///
    /// Therse are the row that come before the start of the screen,
    /// with a negative line number.
    fn history_iter(&self) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)>;

    /// Iterate through all the rows of the terminal screen.
    fn screen_iter(&self) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)>;

    /// Iterate through lines within `[start, end)`;
    fn ranged_iter(
        &self,
        start: Line,
        end: Line,
    ) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)>;
}

impl GridExt for Grid<Cell> {
    fn history_iter(&self) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)> {
        self.ranged_iter(self.topmost_line(), Line(0))
    }

    fn screen_iter(&self) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)> {
        self.ranged_iter(Line(0), self.bottommost_line() + 1)
    }

    fn ranged_iter(
        &self,
        start: Line,
        end: Line,
    ) -> impl DoubleEndedIterator<Item = (Line, &Row<Cell>)> {
        (start.0..end.0).map(|line| (Line(line), &self[Line(line)]))
    }
}

pub trait RowExt {
    /// Check thether this is a wrapped line.
    ///
    /// A wrapped line continues in the next row.
    fn is_wrapped(&self) -> bool;

    /// Iterate over all columns in the row.
    fn iter(&self) -> impl DoubleEndedIterator<Item = (Column, &Cell)>;

    /// Iterate over columns before `end`, exclusive.
    fn iter_to(&self, end: Column) -> impl DoubleEndedIterator<Item = (Column, &Cell)>;

    /// Iterate over columns within `[start, end)`
    fn ranged_iter(
        &self,
        start: Column,
        end: Column,
    ) -> impl DoubleEndedIterator<Item = (Column, &Cell)>;
}

impl RowExt for Row<Cell> {
    fn is_wrapped(&self) -> bool {
        self[Column(self.len() - 1)].flags.contains(Flags::WRAPLINE)
    }

    fn iter(&self) -> impl DoubleEndedIterator<Item = (Column, &Cell)> {
        self.ranged_iter(Column(0), Column(self.len()))
    }

    fn iter_to(&self, end: Column) -> impl DoubleEndedIterator<Item = (Column, &Cell)> {
        self.ranged_iter(Column(0), end)
    }

    fn ranged_iter(
        &self,
        start: Column,
        end: Column,
    ) -> impl DoubleEndedIterator<Item = (Column, &Cell)> {
        (start.0..end.0).map(|c| (Column(c), &self[Column(c)]))
    }
}

pub trait CellExt {
    /// Check whether a cell is clear (has not been written to).
    ///
    /// See comment on vterm::HandlerProxy
    fn is_clear(&self) -> bool;

    /// Return the number of characters in this cell.
    fn char_count(&self) -> usize;

    /// Check whether a cell is just there as a spacer.
    ///
    /// A spacer is a cell that comes before or after a cell
    /// containing a multi-column character. Spacers should not be
    /// rendered.
    ///
    /// Skipping spacers assumes that Emacs and Alacritty have the
    /// same idea of what a wide char is and will display them the
    /// same way, so a wide char for which Alacritty allocated two
    /// columns should actually take two columns when displayed by
    /// Emacs.
    ///
    /// TODO: force Emacs to follow Alacritty's lead in case of
    /// inconsistencies.
    fn is_spacer(&self) -> bool;
}

impl CellExt for Cell {
    fn is_clear(&self) -> bool {
        is_clear(&self.flags)
    }

    fn char_count(&self) -> usize {
        if self.is_spacer() {
            return 0;
        }

        1 + self.zerowidth().map(|chars| chars.len()).unwrap_or(0)
    }

    fn is_spacer(&self) -> bool {
        self.flags
            .intersects(Flags::WIDE_CHAR_SPACER | Flags::LEADING_WIDE_CHAR_SPACER)
    }
}

/// Check whether a cell is clear (has not been written to).
///
/// See comment on vterm::HandlerProxy}
pub fn is_clear(flags: &Flags) -> bool {
    !flags.intersects(Flags::DIM)
}
