use alacritty_terminal::{grid::Row, index::Column, term::cell::Cell};

use crate::gridext::CellExt;

/// Return a string representation of the row, without newline.
pub(crate) fn row_to_string(row: &Row<Cell>) -> String {
    let mut str = String::new();
    for col in 0..row.len() {
        let cell = &row[Column(col)];
        if !cell.is_clear() {
            cell.append_content(&mut str);
        }
    }

    str
}
