use std::sync::OnceLock;

use alacritty_terminal::term::TermMode;
use qwertty_term_input::{
    key::{Action, Key, KeyEvent},
    key_encode::{self, KittyFlags},
    key_mods::{Mods, OptionAsAlt},
};

use emacs::{Env, GlobalRef, Result, Value, Vector};

use crate::{input, vterm::VTerm};

emacs::use_functions! {
    numberp
    symbolp
}
emacs::use_symbols! {
    nil_sym => "nil"
}

/// Generate an escape sequence for an Emacs keyboard event.
pub(crate) fn kitty_key_seq(
    term: &VTerm,
    orig_key: Option<EmacsKey<'_>>,
    mods: Vector,
    base_key: Option<EmacsKey<'_>>,
) -> Option<String> {
    let orig_key = orig_key?;
    let base_key = base_key?;
    let opts = extract_options(term);
    if opts.kitty_flags.to_bits() == 0 {
        return None;
    }
    let mods = input::mods_from_lisp(mods)?;
    let oc = orig_key.as_char();
    let bc = base_key.as_char();
    let key = key_from_lisp(base_key);
    let mut consumed_mods = Mods::default();
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

    // It's not worth checking the string, as it'll go directly to
    // Emacs, which is more permissive.
    Some(unsafe { String::from_utf8_unchecked(key_encode::encode(&ev, &opts)) })
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

/// Convert an emacs key to a [Key], if possible.
fn key_from_lisp(emacs_key: EmacsKey<'_>) -> Key {
    match emacs_key {
        EmacsKey::Code(code) => {
            if code < 128
                && let Some(key) = Key::from_ascii(code as u8)
            {
                return key;
            }
        }

        EmacsKey::Symbol(value) => {
            for (key, sym) in PHYSICAL_KEY_MAP.get().expect("call kbd::init()") {
                if value == *sym {
                    return *key;
                }
            }
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
pub(crate) fn init(env: &'_ Env) -> Result<()> {
    let mut map = vec![];
    for (key, name) in PHYSICAL_KEY_VEC {
        map.push((*key, env.intern(name)?.make_global_ref()));
    }
    PHYSICAL_KEY_MAP.set(map).unwrap();

    Ok(())
}

/// A key from emacs that can be either a codepoint or a symbol.
pub(crate) enum EmacsKey<'a> {
    Code(u32),
    Symbol(Value<'a>),
}

impl<'a> EmacsKey<'a> {
    pub(crate) fn from(val: Value<'a>) -> Result<Option<Self>> {
        if numberp.bind(val.env).call((val,))?.is_not_nil() {
            Ok(Some(Self::Code(val.into_rust::<u32>()?)))
        } else if symbolp.bind(val.env).call((val,))?.is_not_nil() {
            Ok(Some(Self::Symbol(val)))
        } else {
            Ok(None)
        }
    }

    /// Convert into a unicode character, if possible.
    fn as_char(&self) -> Option<char> {
        if let EmacsKey::Code(code) = self {
            char::from_u32(*code)
        } else {
            None
        }
    }
}
