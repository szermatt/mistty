mod gridext;
mod render;
mod types;
mod vterm;

use crate::vterm::VTerm;
use alacritty_terminal::{
    grid::Dimensions,
    index::Line,
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
    let before = term.mode().clone();
    term.process_bytes(&v);

    let events = term.handle_events(env)?;
    let mode_changes = term.mode_changes(env, &before)?;

    let mut result = ().into_lisp(env)?;
    for change in mode_changes.into_iter().rev() {
        result = env.cons(change, result)?;
    }
    for event in events.into_iter().rev() {
        result = env.cons(event, result)?;
    }

    Ok(result)
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
    Ok(term.mode().contains(TermMode::ALT_SCREEN))
}

/// Check whether bracketed paste is enabled.
#[defun]
fn bracketed_paste_p(term: &VTerm) -> Result<bool> {
    Ok(term.mode().contains(TermMode::BRACKETED_PASTE))
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

/// Create a `Column` that's guaranteed to be a valid line for the
/// terminal that is inside the range [0, bottommost_line].
fn line_range_check(env: &Env, val: i32, term: &VTerm) -> Result<Line> {
    range_check(env, "line", val, 0..=term.bottommost_line().0).map(|c| Line(c))
}

/// Create a `Column` that's guaranteed to be a valid line for the
/// terminal that is inside the range [0, bottommost_line+1].
pub fn line_range_boundary_check(env: &Env, val: i32, term: &VTerm) -> Result<Line> {
    range_check(env, "line", val, 0..=term.grid().screen_lines() as i32).map(|c| Line(c))
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
