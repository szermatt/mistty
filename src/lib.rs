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
    vterm::{VTerm, VTermOptions, VTermProcessor},
};
use emacs::{Env, IntoLisp, Result, Value, Vector, defun};

pub(crate) const VERSION: &str = env!("CARGO_PKG_VERSION");

emacs::plugin_is_GPL_compatible!();

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
///
/// OPTIONS_PLIST is a plist with the following possible keys:
///  - :osc52 nil|'only-copy|'only-paste|'copy-paste
///  - :allow-clearing-scrollback boolean
///
/// If an option is unset, its default value is used. Unknown
/// properties are ignored.
#[defun(user_ptr)]
fn make_vterm(width: usize, height: usize, options_plist: Value<'_>) -> Result<VTermFacade> {
    let mut options = VTermOptions::default();
    options.apply_plist(options_plist)?;
    mistty_log(options_plist.env, format!("OPTIONS: {options:?}"));
    Ok(VTermFacade::new(VTerm::new(width, height, options)))
}

/// Update terminal options.
///
/// OPTIONS_PLIST takes the same kind of plist as passed to
/// [make_vterm]. Unspecified properties are unchanged.
#[defun]
fn update_options(term: &mut VTermFacade, options_plist: Value<'_>) -> Result<()> {
    let mut options = term.vterm.options().clone();
    options.apply_plist(options_plist)?;
    mistty_log(options_plist.env, format!("OPTIONS: {options:?}"));
    term.vterm.set_options(options);

    Ok(())
}

/// Return the scrolline that correspond to the top of the screen.
#[defun]
fn scrolline_at_screen_top(term: &VTermFacade) -> Result<Scrolline> {
    Ok(term.vterm.scrolline_at_screen_top())
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
/// Return nil or a string to send back to the pty.
#[defun]
fn process_bytes<'e>(env: &'e Env, term: &mut VTermFacade, bytes: String) -> Result<Value<'e>> {
    term.processor
        .process_bytes(&mut term.vterm, &bytes.as_bytes());

    let responses = term.vterm.prepare_responses(env)?;
    if responses.is_empty() {
        ().into_lisp(env)
    } else {
        responses.join("").into_lisp(env)
    }
}

/// Collect state changes accumulated since the last call to
/// `collect_changes`.
///
/// Return a list of events to be processed Emacs-side. Events are
/// encoded as list, with an identifying symbol as car followed by an
/// event-specific argument list.
///
/// Events can be:
///
/// - `(kbd . ,enabled) to report that the Kitty Keyboard Protocol was enabled/disabled
/// - `(bracketed-paste . ,enabled) to report that bracketed paste was enabled/disabled
/// - `(fullscreen . ,enabled) to report that fullscreen (alt screen) was enabled/disabled
/// - `(mouse . ,mode) to report mouse mode change, nil (off) 'click, 'drag, 'motion
/// - `(cursor . ,enabled) to report that the cursor was shown/hidden
/// - `(title . ,title) to report window title set or clear (title is nil)
/// - `(cwd . ,url) to report working directory set (as a file: url) or clear (url is nil)
/// - `(cleared-screen) to report that the screen was cleared
/// - `(cleared-scrollback) to report that the scrollback was cleared
/// - `(modified . ,scrolline) to report unexpected modification on the given scrollie
/// - `(reset) to report that the terminal was reset
/// - `(start-prompt ,source ,start
///    to report a prompt start with:
///    - source 'osc133 'bracketed-paste or 'prompt_sp start
///    - start the start scrolline
/// - `(update-prompt 'source :user-input-start (,scrolline . ,col)))
///    Sets the line and position of user input in the prompt
/// - `(end-prompt ,source ,end)
#[defun]
fn collect_changes<'a>(env: &'a Env, term: &mut VTermFacade) -> Result<Value<'a>> {
    let mut result = to_list(env, term.vterm.collect_changes()?)?;
    result = build_list(term.processor.collect_prompt_changes(), result)?;

    Ok(result)
}

/// Return a plist of the terminal modes.
///
/// The modes match the changes reported by collect_changes.
///
/// Reported modes:
/// - `(bracketed-paste . nil/t)`
/// - `(fullscreen . nil/t)`
/// - `(mouse . nil/click/drag/motion)`
/// - `(cursor . nil/t)`
/// - `(kbd . nil/t)`
/// - `(show-cursor . nil/t)`
///
/// Unreported modes default to nil
#[defun]
fn mode_alist<'a>(env: &'a Env, term: &VTermFacade) -> Result<Value<'a>> {
    to_list(env, term.vterm.mode_alist())
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

/// Clear dead trailing spaces left on `scrolline` after inserting `content`
/// at character position `pos_in_line` within `scrolline`.
#[defun]
fn detect_dead_spaces_after_insert(
    term: &mut VTermFacade,
    content: String,
    scrolline: usize,
    pos_in_line: usize,
) -> Result<()> {
    term.vterm
        .detect_dead_spaces_after_insert(&content, Scrolline(scrolline), pos_in_line);

    Ok(())
}

/// Return the number of terminal lines in the scrollback.
#[defun]
fn scrollback_line_count(term: &VTermFacade) -> Result<usize> {
    Ok(term.vterm.scrollback_row_count())
}

/// Render the state of the terminal in a way Emacs understands.
///
/// Rendering is done in the current buffer in the range from point to
/// end of buffer (or earlier if narrowing is enabled), starting with
/// the scrollback lines, then the screen itself. Scrollback kept
/// terminal-side is cleared.
///
/// SKIP_ABOVE, a scrolline, tells render to skip rendering of
/// everything below that scrolline. Set it to 0 to not skip anything.
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
    skip_above: usize,
) -> Result<Value<'a>> {
    let result = render::render(env, &mut term.vterm, cursor_marker, Scrolline(skip_above))?;

    Ok(result)
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

/// Start a regexp prompt and make it current, return true if a
/// prompt was created.
///
/// The change will *not* be reported in the next call to
/// `mistty-alacritty-vt-collect-change`.
#[defun]
fn start_regexp_prompt(term: &mut VTermFacade, start: usize) -> Result<bool> {
    Ok(term
        .processor
        .start_regexp_prompt(&mut term.vterm, Scrolline(start)))
}

/// End a regexp prompt.
///
/// The change will *not* be reported in the next call to
/// `mistty-alacritty-vt-collect-change`.
#[defun]
fn end_regexp_prompt(term: &mut VTermFacade) -> Result<()> {
    term.processor.end_regexp_prompt(&mut term.vterm);

    Ok(())
}

/// Log a debug message elisp-side (best effort).
pub(crate) fn mistty_log<S: AsRef<str>>(env: &Env, message: S) {
    let _ = env.call("mistty-log", (message.as_ref(),));
}

/// Turn an iterable type into a list.
fn to_list<'e, T, U>(env: &'e Env, vals: T) -> Result<Value<'e>>
where
    T: IntoIterator<Item = U>,
    U: IntoLisp<'e>,
    <T as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    build_list(vals, ().into_lisp(env)?)
}

/// Turn an iterable type into a list that ends with `rest`.
fn build_list<'e, T, U>(list: T, mut rest: Value<'e>) -> Result<Value<'e>>
where
    T: IntoIterator<Item = U>,
    U: IntoLisp<'e>,
    <T as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    let env = rest.env;
    for event in list.into_iter().rev() {
        rest = env.cons(event, rest)?;
    }

    Ok(rest)
}
