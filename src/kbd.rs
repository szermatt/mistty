use std::sync::OnceLock;

use alacritty_terminal::term::TermMode;
use qwertty_term_input::{
    key::{Action, Key, KeyEvent},
    key_encode::{self, KittyFlags},
    key_mods::{Mods, OptionAsAlt},
};

use emacs::{Env, GlobalRef, Result, Value, Vector, defun};

use crate::vterm::VTerm;

emacs::use_symbols! {
    shift_sym => "shift"
    control_sym => "control"
    meta_sym => "meta"
    alt_sym => "alt"
    super_sym => "super"
    nil_sym => "nil"
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
pub fn kitty_key_seq<'e>(
    term: &mut VTerm,
    env: &'e Env,
    orig_key: Value<'_>,
    mods: Vector,
    base_key: Value<'_>,
) -> Result<Value<'e>> {
    match internal_kitty_key_seq(term, orig_key, mods, base_key) {
        None => Ok(nil_sym.bind(env)),
        Some(vector) => {
            let lisp_vec = env.make_vector(vector.len(), 0u8)?;
            for (i, byte) in vector.into_iter().enumerate() {
                lisp_vec.set(i, byte)?;
            }
            Ok(lisp_vec.value())
        }
    }
}

fn internal_kitty_key_seq<'e>(
    term: &VTerm,
    orig_key: Value<'_>,
    mods: Vector,
    base_key: Value<'_>,
) -> Option<Vec<u8>> {
    let opts = extract_options(term);
    if opts.kitty_flags.to_bits() == 0 {
        return None;
    }
    let mods = convert_mods(mods)?;
    let key = convert_key(base_key);
    let mut consumed_mods = Mods::default();
    let oc = orig_key.into_rust::<u32>().ok().and_then(char::from_u32);
    let bc = base_key.into_rust::<u32>().ok().and_then(char::from_u32);
    let utf8;
    let unshifted_codepoint;
    match (oc, bc) {
        (None, Some(bc)) => {
            // control-char
            utf8 = bc.to_string();
            unshifted_codepoint = bc as u32;
        }
        (Some(c), Some(bc)) => {
            if c.is_control() || c == bc {
                // control-char
                utf8 = bc.to_string();
                unshifted_codepoint = bc as u32;
            } else {
                // either:
                //  - normal char
                //  - shifted-char; basic-type reports the original, unshifted key.
                consumed_mods = mods;
                utf8 = c.to_string();
                unshifted_codepoint = bc as u32;
            }
        }
        _ => {
            utf8 = String::new();
            unshifted_codepoint = 0
        }
    }
    let ev = KeyEvent {
        action: Action::Press,
        key,
        mods,
        composing: false,
        utf8,
        consumed_mods,
        unshifted_codepoint,
    };
    let vec = key_encode::encode(&ev, &opts);

    Some(vec)
}

/// Convert terminal modes to key encoding format.
fn extract_options(term: &VTerm) -> key_encode::Options {
    let mode = term.mode();

    key_encode::Options {
        cursor_key_application: mode.intersects(TermMode::APP_CURSOR),
        keypad_key_application: mode.intersects(TermMode::APP_KEYPAD),
        kitty_flags: KittyFlags {
            disambiguate: mode.intersects(TermMode::DISAMBIGUATE_ESC_CODES),
            report_events: mode.intersects(TermMode::REPORT_EVENT_TYPES),
            report_alternates: mode.intersects(TermMode::REPORT_ALTERNATE_KEYS),
            report_all: mode.intersects(TermMode::REPORT_ALL_KEYS_AS_ESC),
            report_associated: mode.intersects(TermMode::REPORT_ASSOCIATED_TEXT),
        },
        macos_option_as_alt: OptionAsAlt::False,

        // TODO: look for these states in alacritty term source code
        backarrow_key_mode: false,
        ignore_keypad_with_numlock: false,
        alt_esc_prefix: false,
        modify_other_keys_state_2: false,
    }
}

/// Convert lisp mode symbols to key encoding format.
fn convert_mods(lisp_mods: Vector) -> Option<Mods> {
    let mut mods = Mods::default();
    for val in lisp_mods {
        if val == *shift_sym {
            mods.shift = true;
        } else if val == *control_sym {
            mods.ctrl = true;
        } else if val == *meta_sym || val == *alt_sym {
            mods.alt = true;
        } else if val == *super_sym {
            mods.super_ = true;
        } else {
            return None;
        }
    }

    Some(mods)
}

/// Convert an emacs key to a [Key], if possible.
fn convert_key(emacs_key: Value<'_>) -> Key {
    if let Ok(num) = emacs_key.into_rust::<u8>()
        && let Some(key) = Key::from_ascii(num)
    {
        return key;
    }

    for (key, sym) in PHYSICAL_KEY_MAP.get().expect("call kbd::init()") {
        if emacs_key == *sym {
            return *key;
        }
    }

    Key::Unidentified
}

/// Map Key constant to global, interned symbols.
///
/// Created at init time from [PHYSICAL_KEY_VEC].
static PHYSICAL_KEY_MAP: OnceLock<Vec<(Key, GlobalRef)>> = OnceLock::new();

/// Map Key constant to Emacs symbols, as much as possible.
///
/// Symbol names are taken from lispy_function_keys , defined in keyboard.c
const PHYSICAL_KEY_VEC: &[(Key, &str)] = &[
    (Key::ArrowDown, "down"),
    (Key::ArrowLeft, "left"),
    (Key::ArrowRight, "right"),
    (Key::ArrowUp, "up"),
    (Key::AudioVolumeDown, "volume-down"),
    (Key::AudioVolumeMute, "volume-mute"),
    (Key::AudioVolumeUp, "volume-up"),
    (Key::Backspace, "backspace"),
    (Key::BrowserRefresh, "browser-refresh"),
    (Key::ContextMenu, "menu"),
    (Key::Copy, "copy"),
    (Key::Cut, "cut"),
    (Key::Delete, "delete"),
    (Key::End, "end"),
    (Key::Enter, "enter"),
    (Key::Escape, "escape"),
    (Key::F1, "f1"),
    (Key::F10, "f10"),
    (Key::F11, "f11"),
    (Key::F12, "f12"),
    (Key::F2, "f2"),
    (Key::F3, "f3"),
    (Key::F4, "f4"),
    (Key::F5, "f5"),
    (Key::F6, "f6"),
    (Key::F7, "f7"),
    (Key::F8, "f8"),
    (Key::F9, "f9"),
    (Key::Help, "help"),
    (Key::Home, "home"),
    (Key::Insert, "insert"),
    (Key::MediaPlayPause, "media-play-pause"),
    (Key::MediaSelect, "media-select"),
    (Key::MediaStop, "media-stop"),
    (Key::MediaTrackNext, "media-skip-forward"),
    (Key::MediaTrackPrevious, "media-skip-backward"),
    (Key::PageDown, "next"),
    (Key::PageUp, "prior"),
    (Key::Paste, "paste"),
    (Key::Power, "power"),
    (Key::ScrollLock, "scroll"),
    (Key::Sleep, "sleep"),
    (Key::Tab, "tab"),
    (Key::Enter, "return"),
];

/// Initialize key map.
///
/// This must be called during module initialization.
pub fn init(env: &'_ Env) -> Result<()> {
    let mut map = vec![];
    for (key, name) in PHYSICAL_KEY_VEC {
        map.push((*key, env.intern(name)?.make_global_ref()));
    }
    PHYSICAL_KEY_MAP.set(map).unwrap();

    Ok(())
}
