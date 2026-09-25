mod render;
mod types;
mod vterm;

use crate::vterm::VTerm;
use alacritty_terminal::{
    grid::Dimensions,
    index::{Column, Line, Point},
    term::{Osc52, TermMode},
};
use emacs::{Env, IntoLisp, Result, Value, Vector, defun};
use std::{fmt::Debug, ops::RangeBounds};

emacs::plugin_is_GPL_compatible!();

emacs::use_functions! {
    symbol_value
}
emacs::use_symbols! {
    args_out_of_range
    nil
    only_copy
    only_paste
    copy_paste
    mistty_alacritty_osc52
}

#[emacs::module(
    name = "mistty-alacritty-vt",
    defun_prefix = "mistty-alacritty-vt",
    separator = "-",
    mod_in_name = false
)]
fn init(env: &Env) -> Result<Value<'_>> {
    env.provide("mistty-alacritty-vt")
}

/// Create a virtual terminal wit the given dimensions WIDTH x HEIGHT.
///
/// If scrollback is enabled (not nil), the terminal will move when
/// scrolling down, leaving scrollback lines behind it.
#[defun(user_ptr)]
fn make_vterm(env: &Env, width: usize, height: usize) -> Result<VTerm> {
    let osc52_symbol = env.call(symbol_value, (mistty_alacritty_osc52,))?;
    let osc52 = if osc52_symbol == *only_copy {
        Osc52::OnlyCopy
    } else if osc52_symbol == *only_paste {
        Osc52::OnlyPaste
    } else if osc52_symbol == *copy_paste {
        Osc52::CopyPaste
    } else if osc52_symbol == *nil {
        Osc52::Disabled
    } else {
        env.call(
            "mistty-log",
            ("OSC52 disabled; unknown value: '%s'", osc52_symbol),
        )?;

        Osc52::Disabled
    };
    Ok(VTerm::new(width, height, osc52))
}

/// Tell the virtual terminal to track scrollback.
///
/// Scrollback must rendered at regular intervals using
/// `mistty-mod-write-scrollback`.
#[defun]
fn enable_scrollback(term: &mut VTerm) -> Result<()> {
    term.enable_scrollback();

    Ok(())
}

/// Tell the virtual terminal to stop tracking scrollback.
#[defun]
fn disable_scrollback(term: &mut VTerm) -> Result<()> {
    term.disable_scrollback();

    Ok(())
}

/// Change terminal dimensions
#[defun]
fn resize(term: &mut VTerm, width: usize, height: usize) -> Result<()> {
    term.resize(width, height);

    Ok(())
}

/// Process BYTES coming from a pty and update the virtual terminal.
///
/// Return a list of events to be processed Emacs-side. Events are
/// encoded as list, with an identifying symbol as car followed by an
/// event-specific argument list.
///
/// Events:
///  (`write-pty` data): request to write DATA to the PTY
#[defun]
fn process_bytes<'a>(env: &'a Env, term: &mut VTerm, bytes: Vector) -> Result<Value<'a>> {
    let mut v: Vec<u8> = Vec::with_capacity(bytes.len());
    for val in bytes {
        let b: u8 = val.into_rust()?;
        v.push(b);
    }
    term.process_bytes(&v);

    term.handle_events(env)
}

/// Return the content of the virtual terminal as a string with no
/// properties, without wapped lines.
#[defun]
fn display_string<'a>(term: &VTerm) -> Result<String> {
    Ok(term.display_substring(
        Point::new(Line(0), Column(0)),
        Point::new(term.bottommost_line(), term.last_column()),
    ))
}

/// Return a subset of the content of the virtual terminal as a string
/// with no properties, without wapped lines.
///
/// This returns the content of the range [start, end).
#[defun]
fn display_substring<'a>(
    env: &'a Env,
    term: &VTerm,
    start_line: i32,
    start_col: i32,
    end_line: i32,
    end_col: i32,
) -> Result<Value<'a>> {
    term.display_substring(
        point_range_check(env, start_line, start_col, term)?,
        point_range_boundary_check(env, end_line, end_col, term)?,
    )
    .into_lisp(env)
}

/// Return the position of the cursor as (LINE, COLUMN).
///
/// LINE is a terminal line number betwen 0 and `mistty-mod-bottommost-line`.
///
/// COLUMN is a column number between 0 and `mistty-mod-last-column`.
#[defun]
fn cursor<'a>(env: &'a Env, term: &VTerm) -> Result<Value<'a>> {
    let point = term.cursor_point();

    env.cons(point.line.0, point.column.0)
}

/// Check whether the alternate screen is in use.
#[defun]
fn alt_screen_p(term: &VTerm) -> Result<bool> {
    Ok(term.inner().mode().contains(TermMode::ALT_SCREEN))
}

/// Check whether bracketed paste is enabled.
#[defun]
fn bracketed_paste_p(term: &VTerm) -> Result<bool> {
    Ok(term.inner().mode().contains(TermMode::BRACKETED_PASTE))
}

/// Mark spaces at the given line between beg_chars and end_chars as clear.
#[defun]
fn clear_to_eol(env: &Env, term: &mut VTerm, line: i32, beg_chars: usize) -> Result<()> {
    let line = line_range_check(env, line, term)?;

    term.clear_to_eol(line, beg_chars);

    Ok(())
}

/// Cleanup the effects of the hack that ZSH calls prompt sp.
#[defun]
fn cleanup_prompt_sp(env: &Env, term: &mut VTerm, line: i32) -> Result<()> {
    let line = line_range_check(env, line, term)?;
    term.cleanup_prompt_sp(line);

    Ok(())
}

/// Create a `Column` that's guaranteed to be a valid column for the
/// terminal that is inside the range [0, screen_columns).
fn column_range_check(env: &Env, val: i32, term: &VTerm) -> Result<Column> {
    range_check(env, "column", val, 0..(term.inner().columns() as i32)).map(|c| Column(c as usize))
}

/// Create a `Column` that's valid for a boundary, that is, within
/// the range [0, screen_columns].
fn column_range_boundary_check(env: &Env, val: i32, term: &VTerm) -> Result<Column> {
    range_check(env, "column", val, 0..=(term.inner().columns() as i32)).map(|c| Column(c as usize))
}

/// Create a `Column` that's guaranteed to be a valid line for the
/// terminal that is inside the range [0, bottommost_line].
fn line_range_check(env: &Env, val: i32, term: &VTerm) -> Result<Line> {
    range_check(env, "line", val, 0..=term.bottommost_line().0).map(|c| Line(c))
}

/// Create a `Column` that's guaranteed to be a valid line for the
/// terminal that is inside the range [0, bottommost_line+1].
pub fn line_range_boundary_check(env: &Env, val: i32, term: &VTerm) -> Result<Line> {
    range_check(env, "line", val, 0..=term.inner().screen_lines() as i32).map(|c| Line(c))
}

/// Create a `Point` that's guaranteed to be a valid point within the
/// terminal, possibly in the scrollback area.
fn point_range_check(env: &Env, l: i32, c: i32, term: &VTerm) -> Result<Point> {
    Ok(Point::new(
        line_range_check(env, l, term)?,
        column_range_check(env, c, term)?,
    ))
}

/// Create a `Point` that's guaranteed to be a valid point within the
/// terminal, possibly in the scrollback area just after that at
/// column+1 on a valid line or at (bottomline+1, 0).
fn point_range_boundary_check(env: &Env, l: i32, c: i32, term: &VTerm) -> Result<Point> {
    if l == (term.bottommost_line().0 + 1) && c == 0 {
        return Ok(Point::new(Line(l), Column(0)));
    }
    Ok(Point::new(
        line_range_check(env, l, term)?,
        column_range_boundary_check(env, c, term)?,
    ))
}

/// Return a column guaranteed to be within [0, last_column] or
/// throw an error
pub fn range_check<T>(env: &Env, typename: &'static str, val: i32, range: T) -> Result<i32>
where
    T: RangeBounds<i32> + Debug,
{
    if !range.contains(&val) {
        return env.signal(
            args_out_of_range,
            (format!("{typename}: value out of range {range:?}"), val),
        );
    }

    Ok(val)
}
