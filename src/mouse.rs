use crate::{input, vterm::VTerm};
use alacritty_terminal::term::TermMode;
use emacs::{Value, Vector};
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
pub(crate) fn mouse_seq(
    term: &mut VTerm,
    click_events: bool,
    action: Value<'_>,
    button: Option<i32>,
    mods: Vector,
    x: i32,
    y: i32,
) -> Option<Vec<u8>> {
    let action = action_from_lisp(action)?;
    let button = match button {
        None => None,

        // Return None if button is not supported. This is different
        // from not having any button pressed.
        Some(b) => Some(button_from_lisp(b)?),
    };
    let options = extract_options(term, click_events, button.is_some())?;
    let mods = input::mods_from_lisp(mods)?;
    let event = Event {
        action,
        button,
        mods,
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

fn extract_options(
    term: &VTerm,
    click_events: bool,
    any_button_pressed: bool,
) -> Option<mouse_encode::Options> {
    let mode = term.mode();
    let event = if mode.intersects(TermMode::MOUSE_MOTION) {
        MouseEvent::Any
    } else if mode.intersects(TermMode::MOUSE_DRAG) {
        MouseEvent::Button
    } else if mode.intersects(TermMode::MOUSE_REPORT_CLICK) || click_events {
        MouseEvent::Normal
    } else {
        return None;
    };
    let format = if mode.intersects(TermMode::SGR_MOUSE) || click_events {
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
        any_button_pressed,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vterm::{VTerm, VTermOptions, VTermProcessor};

    #[test]
    fn extract_options_mouse_reporting() {
        let mut vterm = VTerm::new(80, 24, VTermOptions::default());
        let mut processor = VTermProcessor::new();

        // Mouse disabled
        assert!(extract_options(&vterm, false, true).is_none());

        // click_events
        let opts = extract_options(&vterm, true, false).unwrap();
        assert_eq!(opts.event, MouseEvent::Normal);
        assert_eq!(opts.format, MouseFormat::Sgr);

        // MOUSE_REPORT_CLICK + click_events
        processor.process_bytes(&mut vterm, b"\x1b[?1000h");
        let opts_with_click = extract_options(&vterm, true, false).unwrap();
        assert_eq!(opts_with_click.event, MouseEvent::Normal);
        assert_eq!(opts_with_click.format, MouseFormat::Sgr);

        // MOUSE_DRAG + click_events
        processor.process_bytes(&mut vterm, b"\x1b[?1002h");
        let opts = extract_options(&vterm, true, false).unwrap();
        assert_eq!(opts.event, MouseEvent::Button);
        assert_eq!(opts.format, MouseFormat::Sgr);

        // MOUSE_MOTION + click_events
        processor.process_bytes(&mut vterm, b"\x1b[?1003h");
        let opts = extract_options(&vterm, true, false).unwrap();
        assert_eq!(opts.event, MouseEvent::Any);
        assert_eq!(opts.format, MouseFormat::Sgr);

        // Terminal enables SGR mouse mode explicitly (\e[?1006h)
        processor.process_bytes(&mut vterm, b"\x1b[?1006h");
        let opts = extract_options(&vterm, false, false).unwrap();
        assert_eq!(opts.format, MouseFormat::Sgr);
    }
}
