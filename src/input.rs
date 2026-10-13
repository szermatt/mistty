use emacs::Vector;
use qwertty_term_input::key_mods::Mods;

emacs::use_symbols! {
    shift_sym => "shift"
    control_sym => "control"
    meta_sym => "meta"
    alt_sym => "alt"
    super_sym => "super"
    hyper_sym => "hyper"
}

/// Convert lisp mode symbols to key encoding format.
pub fn mods_from_lisp(lisp_mods: Vector) -> Option<Mods> {
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
        } else if val == *hyper_sym {
            // Hyper cannot be supported
            return None;
        }
        // Ignore unknown mods. Emacs reports things like mouse down
        // that way, but we don't need it.
    }

    Some(mods)
}
