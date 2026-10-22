mod damage;
mod gridext;
mod input;
mod kbd;
mod mouse;
mod prompt;
mod render;
#[cfg(any(test))]
pub mod testing;
mod types;
mod vterm;

use crate::{
    kbd::EmacsKey,
    types::Scrolline,
    vterm::{VTerm, VTermProcessor},
};
use alacritty_terminal::{
    index::Line,
    term::{Osc52, TermMode},
};
use emacs::{Env, IntoLisp, Result, Value, Vector, defun};
use std::{fmt::Debug, ops::RangeBounds};

pub(crate) const VERSION: &str = env!("CARGO_PKG_VERSION");

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

/// VTerm type kept Emacs-side.
///
/// It includes all terminal-related state.
pub(crate) struct VTermFacade {
    vterm: VTerm,
    processor: VTermProcessor,
}

impl VTermFacade {
    pub(crate) fn new(vterm: VTerm) -> Self {
        Self {
            vterm,
            processor: VTermProcessor::new(),
        }
    }
}

#[emacs::module(
    name = "mistty-alacritty-vt",
    defun_prefix = "mistty-alacritty-vt",
    separator = "-",
    mod_in_name = false
)]
fn init(env: &Env) -> Result<Value<'_>> {
    kbd::init(env)?;

    env.provide("mistty-alacritty-vt")
}

// Return the module version
#[defun]
fn version() -> Result<String> {
    Ok(VERSION.to_string())
}

/// Create a virtual terminal wit the given dimensions WIDTH x HEIGHT.
///
/// If scrollback is enabled (not nil), the terminal will move when
/// scrolling down, leaving scrollback lines behind it.
#[defun(user_ptr)]
fn make_vterm(env: &Env, width: usize, height: usize) -> Result<VTermFacade> {
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
    Ok(VTermFacade::new(VTerm::new(width, height, osc52)))
}

/// Tell the virtual terminal to track scrollback.
///
/// Scrollback must rendered at regular intervals using
/// `mistty-mod-write-scrollback`.
#[defun]
fn enable_scrollback(term: &mut VTermFacade) -> Result<()> {
    term.vterm.enable_scrollback();

    Ok(())
}

/// Tell the virtual terminal to stop tracking scrollback.
#[defun]
fn disable_scrollback(term: &mut VTermFacade) -> Result<()> {
    term.vterm.disable_scrollback();

    Ok(())
}

/// Return the terminal line where `scrolline` starts.
///
/// May return negative lines, if the scrolline is within scrollback.
/// May return nil if scrolline is outside the screen and the
/// scrollback data that's still kept in memory.
#[defun]
fn terminal_line_for_scrolline<'a>(
    env: &'a Env,
    term: &VTermFacade,
    scrolline: usize,
) -> Result<Value<'a>> {
    match term.vterm.terminal_line_for_scrolline(Scrolline(scrolline)) {
        Some(line) => line.0.into_lisp(env),
        None => ().into_lisp(env),
    }
}

/// Change terminal dimensions
#[defun]
fn resize(term: &mut VTermFacade, width: usize, height: usize) -> Result<()> {
    term.vterm.resize(width, height);

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
fn process_bytes<'a>(env: &'a Env, term: &mut VTermFacade, bytes: Vector) -> Result<Value<'a>> {
    let mut v: Vec<u8> = Vec::with_capacity(bytes.len());
    for val in bytes {
        let b: u8 = val.into_rust()?;
        v.push(b);
    }
    let before = *term.vterm.mode();
    term.processor.process_bytes(&mut term.vterm, &v);

    let events = term.vterm.handle_events(env)?;
    let mode_changes = term.vterm.mode_changes(env, &before)?;

    let mut result = ().into_lisp(env)?;
    for change in mode_changes.into_iter().rev() {
        result = env.cons(change, result)?;
    }
    for event in events.into_iter().rev() {
        result = env.cons(event, result)?;
    }
    if let Some(prompt) = term.processor.take_prompt() {
        result = env.cons(prompt, result)?;
    }

    Ok(result)
}

/// Return the position of the cursor as (LINE, COLUMN).
///
/// LINE is a terminal line number betwen 0 and `mistty-mod-bottommost-line`.
///
/// COLUMN is a column number between 0 and `mistty-mod-last-column`.
#[defun]
fn cursor<'a>(env: &'a Env, term: &VTermFacade) -> Result<Value<'a>> {
    let point = term.vterm.cursor_point();

    env.cons(point.line.0, point.column.0)
}

/// Check whether the alternate screen is in use.
#[defun]
fn alt_screen_p(term: &VTermFacade) -> Result<bool> {
    Ok(term.vterm.mode().contains(TermMode::ALT_SCREEN))
}

/// Check whether bracketed paste is enabled.
#[defun]
fn bracketed_paste_p(term: &VTermFacade) -> Result<bool> {
    Ok(term.vterm.mode().contains(TermMode::BRACKETED_PASTE))
}

/// Mark spaces at the given line between beg_chars and end_chars as clear.
#[defun]
fn clear_to_eol(env: &Env, term: &mut VTermFacade, line: i32, beg_chars: usize) -> Result<()> {
    let line = line_range_check(env, line, term)?;

    term.vterm.clear_to_eol(line, beg_chars);

    Ok(())
}

/// Clear terminal history.
#[defun]
pub(crate) fn clear_scrollback(term: &mut VTermFacade) -> Result<()> {
    term.vterm.clear_scrollback();

    Ok(())
}

/// Return the number of terminal lines in the scrollback.
#[defun]
fn scrollback_line_count(term: &VTermFacade) -> Result<usize> {
    Ok(term.vterm.scrollback_row_count())
}

/// Write scrollback lines to the current buffer.
///
/// This function writes any scrollback line kept in the virtual
/// terminal to the current buffer at the current point and leaves the
/// point at the end of the written lines.
///
/// It returns the number of lines written. This might be smaller that
/// the number of the corresponding terminal lines if some lines were
/// wrapped.
///
/// If scrollback is disabled on the virtual terminal, this call
/// always returns 0 and does nothing.
#[defun]
pub(crate) fn write_scrollback(env: &Env, term: &mut VTermFacade) -> Result<usize> {
    render::write_scrollback(env, &mut term.vterm)
}

/// Render the state of the terminal in a way Emacs understands.
///
/// Rendering is done in the current buffer in the range from point to
/// end of buffer (or earlier if narrowing is enabled), starting with
/// the scrollback lines, then the screen itself.
///
/// `cursor_marker` is set to the cursor position.
///
/// The point is not conserved. Wrap this call inside a
/// `save_excursion`.
///
/// This function returns the buffer position of the top right corner
/// of the screen and the corresponding absolute scrolline. Everything
/// before that position belongs in the scrollback area.
#[defun]
pub(crate) fn render<'a>(
    env: &'a Env,
    term: &mut VTermFacade,
    cursor_marker: Value,
) -> Result<Value<'a>> {
    render::render(env, &mut term.vterm, cursor_marker)
}

/// Render the terminal content on the current buffer.
///
/// This call renders only the terminal content, ignoring the
/// scrollback area. Call [render] to take the scrollback area into
/// account.
///
/// This call may optimize rendering by only updating the portions of
/// the terminal that have changed since last call to `render` or
/// `render_damage. For this to work, the range from point to end of
/// buffer must contain the unmodified result of the previous call.
///
/// Rendering is done in the current buffer in the range START to end
/// of buffer (or earlier if narrowing is enabled).
///
/// `cursor_marker` is set to the cursor position if that position has
/// changed since last call. If the cursor hasn't moved since last
/// call, the marker might just be left as it is.
///
/// The point is not conserved. Wrap this call inside
/// `save_excursion` if it matters.
#[defun]
pub(crate) fn render_screen(env: &Env, term: &mut VTermFacade, cursor_marker: Value) -> Result<()> {
    render::render_screen(env, &mut term.vterm, cursor_marker)
}

/// Translate a mouse event into the appropriate terminal sequence.
///
/// `action` is a symbol, either `press` or `release`
///
/// button is a number between 1 and 11, corresponding to the mouse
/// button that is pressed, as reported by emacs `mouse-1`, `mouse-2`,
/// and so on. `wheel-down` is 4, `wheel-up` is 5, `wheel-left` is 6,
/// `wheel-right` is 7.
///
/// mods is a vector of symbols that correspond to the key modifiers
/// pressed down during the event.
///
/// line and column are coordinates within the terminal - they may point
/// to a position outside the terminal.
///
/// Returns either nil, if nothing should be sent for that event, or a
/// string containing a terminal sequence to send to the process.
#[defun]
fn mouse_seq<'a>(
    term: &mut VTermFacade,
    env: &'a Env,
    action: Value<'_>,
    button: i32,
    mods: Vector<'_>,
    x: i32,
    y: i32,
) -> Result<Value<'a>> {
    match mouse::mouse_seq(&mut term.vterm, action, button, mods, x, y) {
        None => Ok(().into_lisp(env)?),
        Some(encoded) => Ok(unsafe { String::from_utf8_unchecked(encoded) }.into_lisp(env)?),
    }
}

/// Generate an escape sequence for an Emacs keyboard event.
///
/// This function generates escape sequences according to the kitty
/// protocol. It is not appropriate to call when the kitty protocol is
/// disable - it always retuns nil in such case.
///
/// This function generate escape sequence for keys appropriate for
/// the current state of `term` in `env`. It takes a vector of
/// symbols, created by converting the list `event-modifiers` returns
/// into a vector, and `base_key`, which is the result of calling
/// `event-basic-type`.
///
/// `base_key` is either a number, an Emacs unicode character code, or
/// a symbol, for function keys.
///
/// The result is either nil, if the keyboard event cannot be
/// transformed, or a vector of bytes to send to the terminal.
#[defun]
pub(crate) fn kitty_key_seq<'e>(
    term: &mut VTermFacade,
    env: &'e Env,
    orig_key: Value<'_>,
    mods: Vector,
    base_key: Value<'_>,
) -> Result<Value<'e>> {
    match kbd::kitty_key_seq(
        &term.vterm,
        EmacsKey::from(orig_key)?,
        mods,
        EmacsKey::from(base_key)?,
    ) {
        None => Ok(().into_lisp(env)?),
        Some(str) => str.into_lisp(env),
    }
}

/// Create a `Column` that's guaranteed to be a valid line for the
/// terminal that is inside the range [0, bottommost_line].
fn line_range_check(env: &Env, val: i32, term: &VTermFacade) -> Result<Line> {
    range_check(env, "line", val, 0..=term.vterm.bottommost_line().0).map(Line)
}

/// Return a column guaranteed to be within [0, last_column] or
/// throw an error
pub(crate) fn range_check<T>(env: &Env, typename: &'static str, val: i32, range: T) -> Result<i32>
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
