use crate::{input, vterm::VTerm};
use alacritty_terminal::term::TermMode;
use emacs::{Env, IntoLisp, Result, Value, Vector, defun};
use qwertty_term_input::{
    mouse::{Action, Button},
    mouse_encode::{self, Event, MouseEvent, MouseFormat, Pos, Size},
};

/// Fake cell width used to build Options.
///
/// It doesn't matter since reporting pixel positions isn't supported.
const CELL_WIDTH: f32 = 10.0;

/// Fake cell height used to build Options.
const CELL_HEIGHT: f32 = 18.0;

emacs::use_symbols! {
    press_sym => "press"
    release_sym => "release"
    motion_sym => "motion"
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
    term: &mut VTerm,
    env: &'a Env,
    action: Value<'_>,
    button: i32,
    mods: Vector<'_>,
    x: i32,
    y: i32,
) -> Result<Value<'a>> {
    match mouse_seq_internal(term, action, button, mods, x, y) {
        None => Ok(().into_lisp(env)?),
        Some(encoded) => Ok(unsafe { String::from_utf8_unchecked(encoded) }.into_lisp(env)?),
    }
}

fn mouse_seq_internal(
    term: &mut VTerm,
    action: Value<'_>,
    button: i32,
    mods: Vector,
    x: i32,
    y: i32,
) -> Option<Vec<u8>> {
    let options = extract_options(term)?;
    let action = action_from_lisp(action)?;
    let button = button_from_lisp(button)?;
    let mods = input::mods_from_lisp(mods)?;
    let event = Event {
        action,
        button: Some(button),
        mods: mods,
        pos: Pos {
            x: (x as f32) * CELL_WIDTH + CELL_WIDTH / 2.0,
            y: (y as f32) * CELL_HEIGHT + CELL_HEIGHT / 2.0,
        },
    };

    let encoded = mouse_encode::encode(event, &options, term.last_cell());
    if encoded.is_empty() {
        None
    } else {
        Some(encoded)
    }
}

fn extract_options(term: &VTerm) -> Option<mouse_encode::Options> {
    let mode = term.mode();
    let event = if mode.intersects(TermMode::MOUSE_MOTION) {
        MouseEvent::Any
    } else if mode.intersects(TermMode::MOUSE_DRAG) {
        MouseEvent::Button
    } else if mode.intersects(TermMode::MOUSE_REPORT_CLICK) {
        MouseEvent::Normal
    } else {
        return None;
    };
    let format = if mode.intersects(TermMode::SGR_MOUSE) {
        MouseFormat::Sgr
    } else if mode.intersects(TermMode::UTF8_MOUSE) {
        MouseFormat::Utf8
    } else {
        MouseFormat::default()
    };
    let size = Size {
        screen_width: term.columns() as f64 * CELL_WIDTH as f64,
        screen_height: term.screen_lines() as f64 * CELL_HEIGHT as f64,
        cell_width: CELL_WIDTH as f64,
        cell_height: CELL_HEIGHT as f64,
    };
    Some(mouse_encode::Options {
        event,
        format,
        size,
        any_button_pressed: true,
    })
}

fn action_from_lisp(action: Value<'_>) -> Option<Action> {
    if action == *press_sym {
        Some(Action::Press)
    } else if action == *release_sym {
        Some(Action::Release)
    } else if action == *motion_sym {
        Some(Action::Motion)
    } else {
        None
    }
}

fn button_from_lisp(button: i32) -> Option<Button> {
    match button {
        1 => Some(Button::Left),
        // Emacs maps mouse-2 to the middle mouse button and mouse-3
        // to the right mouse button. The numbers follow Emacs
        // convention.
        2 => Some(Button::Middle),
        3 => Some(Button::Right),
        4 => Some(Button::Four),
        5 => Some(Button::Five),
        6 => Some(Button::Six),
        7 => Some(Button::Seven),
        8 => Some(Button::Eight),
        9 => Some(Button::Nine),
        10 => Some(Button::Ten),
        11 => Some(Button::Eleven),
        _ => None,
    }
}
