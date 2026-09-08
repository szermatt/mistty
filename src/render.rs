use crate::types::BufferPos;
use crate::vterm::VTerm;
use alacritty_terminal::{
    Grid,
    grid::{Dimensions, Row},
    index::{Column, Line},
    term::{
        TermDamage,
        cell::{Cell, Flags, Hyperlink},
    },
    vte::ansi::{Color, NamedColor, Rgb},
};
use emacs::{Env, Result, Value, defun};
use std::{cmp::max, collections::HashMap, str::FromStr};

emacs::use_functions! {
    add_face_text_property
    get_text_property
    char_before
    delete_char
    delete_region
    face_foreground
    face_background
    goto_char
    insert
    make_text_button
    propertize
    set_marker
}

emacs::use_symbols! {
    ansi_color_black
    ansi_color_blue
    ansi_color_bold
    ansi_color_bright_black
    ansi_color_bright_blue
    ansi_color_bright_cyan
    ansi_color_bright_green
    ansi_color_bright_magenta
    ansi_color_bright_red
    ansi_color_bright_white
    ansi_color_bright_yellow
    ansi_color_cyan
    ansi_color_green
    ansi_color_inverse
    ansi_color_italic
    ansi_color_magenta
    ansi_color_red
    ansi_color_underline
    ansi_color_white
    ansi_color_yellow
    ansi_osc_hyperlink
    browse_url_data
    type_sym => "type"
    default_face => "default"
    cursor_face => "cursor"
    foreground_sym => ":foreground"
    background_sym => ":background"
    bold_sym => "bold"
    put_text_property
    term_line_wrap
    yank_handler
    invisible_sym => "invisible"
    mistty_clear
    mistty_skip
    indent_sym => "indent"
    right_prompt_sym => "right-prompt"
}

/// Cell or text properties.
#[derive(PartialEq, Eq, Debug, Clone)]
enum RenderProperty {
    /// Foreground color, [Cell::fg].
    Fg(Color),

    /// Background color, [Cell::bg].
    Bg(Color),

    Link(Hyperlink),

    /// Properties that are either on or off, [Cell::flags].
    Toggle(ToggleProperty),
}

/// Boolean properties, that are either on or off [Cell::flags].
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
enum ToggleProperty {
    Inverse,
    Bold,
    Italic,
    Underline,

    /// Identifies newlines added to split lines longer than the width
    /// of the terminal. This is set on the newline characters.
    Wrapline,

    /// Identifies cells that haven't been written to yet.
    Clear,

    /// Skipped spaces at the beginning of a line.
    Indent,

    /// Suspected right prompt, including skipped spaces before it.
    RightPrompt,
}

impl ToggleProperty {
    /// Set of properties relevant when rendering the terminal area.
    const ON_TERMINAL: &[ToggleProperty] = &[
        ToggleProperty::Inverse,
        ToggleProperty::Bold,
        ToggleProperty::Italic,
        ToggleProperty::Underline,
        ToggleProperty::Clear,
        ToggleProperty::Wrapline,
    ];

    /// Set of properties relevant when writing in the scrollback area.
    const ON_SCROLLBACK: &[ToggleProperty] = &[
        ToggleProperty::Inverse,
        ToggleProperty::Bold,
        ToggleProperty::Italic,
        ToggleProperty::Underline,
    ];

    fn is_set(&self, flags: Flags) -> Option<bool> {
        match self {
            ToggleProperty::Inverse => Some(flags.intersects(Flags::INVERSE)),
            ToggleProperty::Bold => Some(flags.intersects(Flags::BOLD)),
            ToggleProperty::Italic => Some(flags.intersects(Flags::ITALIC)),
            ToggleProperty::Underline => Some(flags.intersects(Flags::UNDERLINE)),

            // Flags::DIM is handled specially in this code. See comment on vterm::HandlerProxy.
            ToggleProperty::Clear => Some(is_clear(flags)),

            // Wrapline is handled specially, as it applies to the
            // newline following the last column, to the column that's
            // flagged. Wrapline must be set with set_toggle.
            ToggleProperty::Wrapline => Some(false),

            // Special properties not mapped to a flag.
            ToggleProperty::Indent | ToggleProperty::RightPrompt => None,
        }
    }
}

/// Clear terminal history without writing it.
///
/// Return how many lines were cleared.
#[defun]
pub fn clear_scrollback(term: &mut VTerm) -> Result<usize> {
    let history_size = term.inner().grid().history_size();
    term.clear_history();

    Ok(history_size)
}

/// Write scrollback lines to the current buffer, clear terminal history.
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
pub fn write_scrollback(env: &Env, term: &mut VTerm) -> Result<usize> {
    let grid = term.inner().grid();
    let topmost_line = grid.topmost_line();
    let last_column = grid.last_column();
    let history_size = grid.history_size();
    if history_size == 0 {
        return Ok(0);
    }
    if term.start_with_wrapped_line() {
        // TODO: move this logic elisp-side
        let c = env.call(char_before, [])?;
        if c.is_not_nil()
            && c.into_rust::<i32>()? == 10 // nl
            && env
                .call(
                    get_text_property,
                    (BufferPos::point(env)? - 1, term_line_wrap),
                )?
                .is_not_nil()
        {
            env.call(delete_char, (-1,))?;
        }
    }
    let mut as_string = String::with_capacity((-topmost_line.0) as usize * (last_column.0 * 2 + 1));
    let origin = BufferPos::point(env)?;
    let mut tracker = PropertyTracker::new(origin, ToggleProperty::ON_SCROLLBACK);
    let mut pos = origin;
    let mut scrollines = 0;
    for line in topmost_line.0..0 {
        let line = Line(line);
        let row = &grid[line];

        if let Some(right_col) = last_written_cell(row) {
            for col in 0..=right_col.0 {
                let col = Column(col);
                let cell = &row[col];
                let cell_pos = pos;
                pos += append_cell_to_string(&cell, &mut as_string);
                tracker.track_change(cell_pos, &cell);
            }
        }
        if !row[last_column].flags.contains(Flags::WRAPLINE) {
            as_string.push('\n');
            pos += 1;
            scrollines += 1;
        }
    }
    env.call(insert, (as_string,))?;
    tracker.apply(env, pos)?;
    term.clear_history();

    if term.start_with_wrapped_line() {
        env.call(
            insert,
            (env.call(propertize, ("\n", term_line_wrap, true))?,),
        )?;
    }

    return Ok(scrollines);
}

/// Return the column of the last cell that isn't clear.
fn last_written_cell(row: &Row<Cell>) -> Option<Column> {
    for col in (0..row.len()).rev() {
        let col = Column(col);
        if !is_clear(row[col].flags) {
            return Some(col);
        }
    }

    None
}

/// Detect clear regions at the start of a line.
///
/// If a clear region is found, return its end position.
fn detect_indent(
    row: &Row<Cell>,
    end_col: Column,
    right_prompt_start: Option<Column>,
) -> Option<Column> {
    if right_prompt_start.is_some() {
        return None;
    }
    for col in 0..end_col.0 {
        let col = Column(col);
        if !is_clear(row[col].flags) {
            if col.0 > 0 {
                return Some(col);
            } else {
                return None;
            }
        }
    }

    Some(end_col)
}

/// Detect a right prompt.
///
/// If a right prompt is found, return its start position.
fn detect_right_prompt(row: &Row<Cell>, last_written: Option<Column>) -> Option<Column> {
    match last_written {
        None => None,
        Some(last_written) => {
            if (last_written.0 + 2) >= row.len() {
                for col in (0..last_written.0).rev() {
                    let col = Column(col);
                    if is_clear(row[col].flags) {
                        let empty = col;
                        for col in (0..empty.0).rev() {
                            let col = Column(col);
                            if !is_clear(row[col].flags) {
                                let start = col + 1;
                                if start <= empty {
                                    return Some(start);
                                }
                                return None;
                            }
                        }
                        return Some(Column(0));
                    }
                }
            }

            None
        }
    }
}

/// Return the line containing the last cell that isn't clear.
fn last_written_line(grid: &Grid<Cell>) -> Option<Line> {
    for line in (0..=grid.bottommost_line().0).rev() {
        let line = Line(line);
        if !last_written_cell(&grid[line]).is_none() {
            return Some(line);
        }
    }

    None
}

/// Render the state of the terminal in a way Emacs understands.
///
/// Rendering is done in the current buffer in the range from point to
/// end of buffer (or earlier if narrowing is enabled).
///
/// `cursor_marker` is set to the cursor position.
///
/// The point is not conserved. Wrap this call inside a
/// `save_excursion`.
#[defun]
pub fn render(env: &Env, term: &mut VTerm, cursor_marker: Value) -> Result<()> {
    render_inner(env, term, cursor_marker, None)
}

/// Re-render modified parts of the terminal, Emacs-side.
///
/// This call optimizes rendering by only updating the portions of the
/// terminal that have changed since last call to `render` or
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
/// `save_excursion`.
#[defun]
pub fn render_damaged(env: &Env, term: &mut VTerm, cursor_marker: Value) -> Result<()> {
    let damage = if let TermDamage::Partial(iter) = term.inner_mut().damage() {
        let mut lines: Vec<Line> = iter.map(|d| Line(d.line as i32)).collect();
        lines.sort_unstable();
        lines.dedup();
        // damage is sorted by line, one damage per line.

        Some(lines)
    } else {
        None
    };

    render_inner(env, term, cursor_marker, damage)
}

fn render_inner(
    env: &Env,
    term: &mut VTerm,
    cursor_marker: Value,
    damage: Option<Vec<Line>>,
) -> Result<()> {
    let mut cursor_pos = None;

    let cursor_line = term.inner().grid().cursor.point.line;
    let last_written = last_written_line(term.inner().grid()).unwrap_or(Line(0));
    let last_line = max(last_written, cursor_line);
    if let Some(damaged_lines) = damage {
        let mut line_at_point = Line(0);
        for line in damaged_lines
            .into_iter()
            .filter(|l| *l <= last_written)
            .chain(
                // if needed, render missing blank lines from
                // last_written to the cursor position
                (last_written.0 + 1..=last_line.0)
                    .into_iter()
                    .map(|l| Line(l)),
            )
        {
            let line_pos = BufferPos::bol(env, line - line_at_point)?;
            env.call(goto_char, (line_pos,))?;
            let next_line_pos = BufferPos::bol(env, Line(1))?;
            env.call(delete_region, (line_pos, next_line_pos))?;
            render_lines(env, term, line, line + 1, Some(&mut cursor_pos))?;
            line_at_point = line + 1;
        }

        if last_line < term.bottommost_line() {
            env.call(
                delete_region,
                (
                    BufferPos::bol(env, last_line + 1 - line_at_point)?,
                    BufferPos::point_max(env)?,
                ),
            )?;
        }
    } else {
        env.call(
            delete_region,
            (BufferPos::point(env)?, BufferPos::point_max(env)?),
        )?;
        render_lines(env, term, Line(0), last_line + 1, Some(&mut cursor_pos))?;
    }

    if let Some(cursor_pos) = cursor_pos {
        env.call(set_marker, (cursor_marker, cursor_pos))?;
    }

    term.inner_mut().reset_damage();

    Ok(())
}

/// Render the display or a subset of the display.
///
/// Write lines [`beg`, `end`) including the newlines to the
/// current buffer at the current position.
///
/// If the lines contain `cursor_pos` the function sets the cursor position, as a buffer
/// position, otherwise it leaves it alone.
pub fn render_lines<'a>(
    env: &Env,
    term: &'a VTerm,
    beg: Line,
    end: Line,
    mut cursor_pos: Option<&mut Option<BufferPos>>,
) -> Result<()> {
    let grid = term.inner().grid();
    let last_column = grid.last_column();
    let cursor_point = grid.cursor.point;
    let origin = BufferPos::point(env)?;
    let mut tracker = PropertyTracker::new(origin, ToggleProperty::ON_TERMINAL);
    let mut as_string = String::with_capacity((end.0 - beg.0) as usize * (last_column.0 + 1));
    let mut pos = origin;
    for line in beg.0..end.0 {
        let line = Line(line);
        let row = &grid[line];
        let last_written = last_written_cell(row).map(|c| c + 1);
        let end_col =
            if line == cursor_point.line && last_written.is_none_or(|v| v < cursor_point.column) {
                cursor_point.column
            } else {
                last_written.unwrap_or(Column(0))
            };
        let right_prompt_start = detect_right_prompt(row, last_written);
        let indent_end = detect_indent(row, end_col, right_prompt_start);
        if indent_end.is_some() {
            tracker.set_toggle(pos, ToggleProperty::Indent, true);
        }
        for col in 0..end_col.0 {
            let col = Column(col);
            let cell = &row[col];
            let cell_pos = pos;
            pos += append_cell_to_string(cell, &mut as_string);
            tracker.track_change(cell_pos, cell);
            if Some(col) == indent_end {
                tracker.set_toggle(cell_pos, ToggleProperty::Indent, false);
            }
            if Some(col) == right_prompt_start {
                tracker.set_toggle(cell_pos, ToggleProperty::RightPrompt, true);
            }

            // Have we found the cursor? If yes, we now know its
            // (upcoming) buffer position.
            if cursor_point.line == line && cursor_point.column == col {
                if let Some(cursor_pos) = &mut cursor_pos {
                    **cursor_pos = Some(cell_pos);
                }
            }
        }
        if indent_end == Some(end_col) {
            tracker.set_toggle(pos, ToggleProperty::Indent, false);
        }
        if right_prompt_start.is_some() {
            tracker.set_toggle(pos, ToggleProperty::RightPrompt, false);
        }
        // end of line
        if cursor_point.line == line && cursor_point.column == end_col {
            if let Some(cursor_pos) = &mut cursor_pos {
                **cursor_pos = Some(pos);
            }
        }

        // A NL is never clear
        tracker.set_toggle(pos, ToggleProperty::Clear, false);
        if row[last_column].flags.contains(Flags::WRAPLINE) {
            tracker.set_toggle(pos, ToggleProperty::Wrapline, true);
        }
        as_string.push('\n');
        pos += 1;
    }

    env.call(insert, (as_string,))?;
    tracker.apply(env, pos)?;

    Ok(())
}

/// Count the number of characters in the cell.
pub fn cell_char_count(c: &Cell) -> usize {
    if is_spacer(c) {
        return 0;
    }

    1 + c.zerowidth().map(|chars| chars.len()).unwrap_or(0)
}

/// Check whether a cell is clear (has not been written to).
///
/// See comment on vterm::HandlerProxy
pub fn is_clear(flags: Flags) -> bool {
    !flags.intersects(Flags::DIM)
}

/// Append the content of a cell to the give string.
///
/// A cell may contain more than one character, as long as they all
/// fit into one column. A cell may also contain wide characters. Such
/// cells are followed or preceded by columns containing spacers
/// (Alacritty takes care of that)
///
/// Returns the number of characters added to `dest`.
fn append_cell_to_string(cell: &Cell, dest: &mut String) -> usize {
    if is_spacer(cell) {
        return 0;
    }
    if cell.c == '\t' {
        // tabs are already stored in the cells as spaces
        dest.push(' ');
    } else {
        dest.push(cell.c);
    }
    let mut charcount = 1;

    for c in cell.zerowidth().into_iter().flatten() {
        charcount += 1;
        dest.push(*c);
    }

    charcount
}

/// Check whether a cell contains as spacer.
///
/// Spacers should not be rendered.
///
/// Skipping spacers assumes that Emacs and Alacritty have the
/// same idea of what a wide char is and will display them the
/// same way, so a wide char for which Alacritty allocated two
/// columns should actually take two columns when displayed by
/// Emacs.
///
/// TODO: force Emacs to follow Alacritty's lead in case of
/// inconsistencies.
fn is_spacer(cell: &Cell) -> bool {
    cell.flags
        .intersects(Flags::WIDE_CHAR_SPACER | Flags::LEADING_WIDE_CHAR_SPACER)
}

/// Track cell flags and apply them Emacs-side as text properties.
struct PropertyTracker {
    fg: (Color, BufferPos),
    bg: (Color, BufferPos),
    toggles: &'static [ToggleProperty],
    toggle_map: HashMap<ToggleProperty, BufferPos>,
    hyperlink: Option<(Hyperlink, BufferPos)>,

    buf: Vec<(RenderProperty, BufferPos, BufferPos)>,
}

impl PropertyTracker {
    fn new(origin: BufferPos, toggles: &'static [ToggleProperty]) -> Self {
        Self {
            fg: (Color::Named(NamedColor::Foreground), origin),
            bg: (Color::Named(NamedColor::Background), origin),
            hyperlink: None,
            toggle_map: HashMap::new(),
            toggles,
            buf: vec![],
        }
    }

    /// Track a flag change at the given buffer position.
    ///
    /// All cells must be passed to track_change in order for the
    /// algorithm to make sense.
    fn track_change(&mut self, pos: BufferPos, cell: &Cell) {
        self.set_fg(pos, cell.fg);
        self.set_bg(pos, cell.bg);
        self.set_link(pos, cell.hyperlink());

        let flags = cell.flags;
        for toggle in self.toggles {
            if let Some(val) = toggle.is_set(flags) {
                self.set_toggle(pos, *toggle, val);
            }
        }
    }

    /// Maybe change background color.
    fn set_bg(&mut self, pos: BufferPos, bg: Color) {
        if bg != self.bg.0 {
            if self.bg.1 < pos {
                self.buf
                    .push((RenderProperty::Bg(self.bg.0), self.bg.1, pos));
            }
            self.bg = (bg, pos);
        }
    }

    /// Maybe change foreground color.
    fn set_fg(&mut self, pos: BufferPos, fg: Color) {
        if fg != self.fg.0 {
            if self.fg.1 < pos {
                self.buf
                    .push((RenderProperty::Fg(self.fg.0), self.fg.1, pos));
            }
            self.fg = (fg, pos);
        }
    }

    fn set_link(&mut self, pos: BufferPos, cell_link: Option<Hyperlink>) {
        match (&self.hyperlink, cell_link) {
            (Some((existing_link, start_pos)), Some(cell_link)) => {
                if *existing_link == cell_link {
                    return;
                }
                self.buf
                    .push((RenderProperty::Link(existing_link.clone()), *start_pos, pos));
                self.hyperlink = Some((cell_link, pos));
            }
            (None, Some(cell_link)) => {
                self.hyperlink = Some((cell_link, pos));
            }
            (Some((existing_link, start_pos)), None) => {
                self.buf
                    .push((RenderProperty::Link(existing_link.clone()), *start_pos, pos));
                self.hyperlink = None;
            }
            (None, None) => {}
        }
    }

    /// Disable or enable a property for the given position.
    fn set_toggle(&mut self, pos: BufferPos, toggle: ToggleProperty, is_set: bool) {
        let start = self.toggle_map.get(&toggle);
        if start.is_some() == is_set {
            return;
        }
        if let Some(start) = start {
            self.buf.push((RenderProperty::Toggle(toggle), *start, pos));
            self.toggle_map.remove(&toggle);
        } else {
            self.toggle_map.insert(toggle, pos);
        }
    }

    /// Set text properties on the current Emacs buffer.
    ///
    /// `end` is the buffer position pointing to the end of the last
    /// cell.
    fn apply(mut self, env: &Env, end: BufferPos) -> Result<()> {
        // close all ranges
        self.set_bg(end, Color::Named(NamedColor::Background));
        self.set_fg(end, Color::Named(NamedColor::Foreground));
        for toggle in self.toggles {
            self.set_toggle(end, *toggle, false);
        }
        self.set_link(end, None);
        assert!(self.toggle_map.is_empty());
        assert_eq!(self.fg.0, Color::Named(NamedColor::Foreground));
        assert_eq!(self.bg.0, Color::Named(NamedColor::Background));

        let Self { buf, .. } = self;
        for (prop, start, end) in buf {
            match prop {
                RenderProperty::Fg(Color::Named(
                    NamedColor::Foreground
                    | NamedColor::BrightForeground
                    | NamedColor::DimForeground,
                )) => {
                    // nothing to do
                }
                RenderProperty::Fg(color) => {
                    let hex = to_emacs_color(env, color, true)?;
                    env.call(
                        add_face_text_property,
                        (start, end, env.list((foreground_sym, hex))?),
                    )?;
                }
                RenderProperty::Bg(Color::Named(NamedColor::Background)) => {
                    // nothing to do
                }
                RenderProperty::Bg(color) => {
                    let hex = to_emacs_color(env, color, false)?;
                    env.call(
                        add_face_text_property,
                        (start, end, env.list((background_sym, hex))?),
                    )?;
                }
                RenderProperty::Link(hyperlink) => {
                    env.call(
                        make_text_button,
                        (
                            start,
                            end,
                            type_sym,
                            ansi_osc_hyperlink,
                            browse_url_data,
                            hyperlink.uri(),
                        ),
                    )?;
                }
                RenderProperty::Toggle(ToggleProperty::Italic) => {
                    env.call(add_face_text_property, (start, end, ansi_color_italic))?;
                }
                RenderProperty::Toggle(ToggleProperty::Bold) => {
                    env.call(add_face_text_property, (start, end, ansi_color_bold))?;
                }
                RenderProperty::Toggle(ToggleProperty::Underline) => {
                    env.call(add_face_text_property, (start, end, ansi_color_underline))?;
                }
                RenderProperty::Toggle(ToggleProperty::Inverse) => {
                    env.call(add_face_text_property, (start, end, ansi_color_inverse))?;
                }
                RenderProperty::Toggle(ToggleProperty::Wrapline) => {
                    env.call(put_text_property, (start, end, term_line_wrap, true))?;
                    env.call(
                        put_text_property,
                        (
                            start,
                            end,
                            yank_handler,
                            env.list((false, "", false, false))?,
                        ),
                    )?;
                    env.call(
                        put_text_property,
                        (start, end, invisible_sym, term_line_wrap),
                    )?;
                }
                RenderProperty::Toggle(ToggleProperty::Clear) => {
                    env.call(put_text_property, (start, end, mistty_clear, true))?;
                }
                RenderProperty::Toggle(ToggleProperty::Indent) => {
                    env.call(put_text_property, (start, end, mistty_skip, indent_sym))?;
                }
                RenderProperty::Toggle(ToggleProperty::RightPrompt) => {
                    env.call(
                        put_text_property,
                        (start, end, mistty_skip, right_prompt_sym),
                    )?;
                }
            }
        }
        Ok(())
    }
}

/// Convert a color to a Rgb value if possible
pub fn to_emacs_color_rgb(env: &Env, color: Color, fg: bool) -> Result<Option<Rgb>> {
    let mut color_str = to_emacs_color(env, color, fg)?;
    if color_str == "unspecified-fg" {
        color_str = face_color(env, default_face, true)?;
    }
    if color_str == "unspecified-bg" {
        color_str = face_color(env, default_face, false)?;
    }
    if let Ok(rgb) = Rgb::from_str(&color_str) {
        Ok(Some(rgb))
    } else {
        Ok(None)
    }
}

/// Transform a color index from a ColorRequest into a NamedColor, if
/// possible.
pub fn named_color_for_color_request(index: usize) -> Option<NamedColor> {
    match index {
        0 => Some(NamedColor::Black),
        1 => Some(NamedColor::Red),
        2 => Some(NamedColor::Green),
        3 => Some(NamedColor::Yellow),
        4 => Some(NamedColor::Blue),
        5 => Some(NamedColor::Magenta),
        6 => Some(NamedColor::Cyan),
        7 => Some(NamedColor::White),
        8 => Some(NamedColor::BrightBlack),
        9 => Some(NamedColor::BrightRed),
        10 => Some(NamedColor::BrightGreen),
        11 => Some(NamedColor::BrightYellow),
        12 => Some(NamedColor::BrightBlue),
        13 => Some(NamedColor::BrightMagenta),
        14 => Some(NamedColor::BrightCyan),
        15 => Some(NamedColor::BrightWhite),
        256 => Some(NamedColor::Foreground),
        257 => Some(NamedColor::Background),
        258 => Some(NamedColor::Cursor),
        259 => Some(NamedColor::DimBlack),
        260 => Some(NamedColor::DimRed),
        261 => Some(NamedColor::DimGreen),
        262 => Some(NamedColor::DimYellow),
        263 => Some(NamedColor::DimBlue),
        264 => Some(NamedColor::DimMagenta),
        265 => Some(NamedColor::DimCyan),
        266 => Some(NamedColor::DimWhite),
        267 => Some(NamedColor::BrightForeground),
        268 => Some(NamedColor::DimForeground),
        _ => None,
    }
}

/// Convert a vte::Color to an emacs color, as a string.
///
/// The color is usually a RGB value starting with #, but it can also
/// be a color name, `unspecified-bg` or `unspecified-fg`.
fn to_emacs_color(env: &Env, color: Color, fg: bool) -> Result<String> {
    Ok(match color {
        Color::Named(NamedColor::Black | NamedColor::DimBlack) | Color::Indexed(0) => {
            face_color(env, ansi_color_black, fg)?
        }
        Color::Named(NamedColor::Red | NamedColor::DimRed) | Color::Indexed(1) => {
            face_color(env, ansi_color_red, fg)?
        }
        Color::Named(NamedColor::Green | NamedColor::DimGreen) | Color::Indexed(2) => {
            face_color(env, ansi_color_green, fg)?
        }
        Color::Named(NamedColor::Yellow | NamedColor::DimYellow) | Color::Indexed(3) => {
            face_color(env, ansi_color_yellow, fg)?
        }
        Color::Named(NamedColor::Blue | NamedColor::DimBlue) | Color::Indexed(4) => {
            face_color(env, ansi_color_blue, fg)?
        }
        Color::Named(NamedColor::Magenta | NamedColor::DimMagenta) | Color::Indexed(5) => {
            face_color(env, ansi_color_magenta, fg)?
        }
        Color::Named(NamedColor::Cyan | NamedColor::DimCyan) | Color::Indexed(6) => {
            face_color(env, ansi_color_cyan, fg)?
        }
        Color::Named(NamedColor::White | NamedColor::DimWhite) | Color::Indexed(7) => {
            face_color(env, ansi_color_white, fg)?
        }
        Color::Named(NamedColor::BrightBlack) | Color::Indexed(8) => {
            face_color(env, ansi_color_black, fg)?
        }
        Color::Named(NamedColor::BrightRed) | Color::Indexed(9) => {
            face_color(env, ansi_color_red, fg)?
        }
        Color::Named(NamedColor::BrightGreen) | Color::Indexed(10) => {
            face_color(env, ansi_color_green, fg)?
        }
        Color::Named(NamedColor::BrightYellow) | Color::Indexed(11) => {
            face_color(env, ansi_color_yellow, fg)?
        }
        Color::Named(NamedColor::BrightBlue) | Color::Indexed(12) => {
            face_color(env, ansi_color_blue, fg)?
        }
        Color::Named(NamedColor::BrightMagenta) | Color::Indexed(13) => {
            face_color(env, ansi_color_magenta, fg)?
        }
        Color::Named(NamedColor::BrightCyan) | Color::Indexed(14) => {
            face_color(env, ansi_color_cyan, fg)?
        }
        Color::Named(NamedColor::BrightWhite) | Color::Indexed(15) => {
            face_color(env, ansi_color_white, fg)?
        }
        Color::Named(
            NamedColor::Foreground | NamedColor::BrightForeground | NamedColor::DimForeground,
        ) => face_color(env, default_face, true)?,
        Color::Named(NamedColor::Background) => face_color(env, default_face, false)?,
        Color::Named(NamedColor::Cursor) => face_color(env, cursor_face, fg)?,
        Color::Spec(rgb) => format!("#{0:02x}{1:02x}{2:02x}", rgb.r, rgb.g, rgb.b),
        Color::Indexed(code) => {
            // indexed_to_rgb must return a value, as code 0 - 15 are handled above
            indexed_to_rgb(code).unwrap()
        }
    })
}

/// Convert an ANSI color code >= 16 to a RGB value.
///
/// Return None if the code is unsupported.
fn indexed_to_rgb(code: u8) -> Option<String> {
    if code >= 16 && code <= 231 {
        // [16-231] 6x6x6 color cube
        fn cube6_level(v: u8) -> u8 {
            if v == 0 { 0 } else { v * 40 + 55 }
        }

        let mut v = code - 16;
        let red = cube6_level(v / 36);
        v = v % 36;
        let green = cube6_level(v / 6);
        v = v % 6;
        let blue = cube6_level(v);

        Some(format!("#{red:02x}{green:02x}{blue:02x}"))
    } else if code >= 232 {
        // [232-255] grayscale
        let gray = (code - 232) * 10 + 8;

        Some(format!("#{gray:02x}{gray:02x}{gray:02x}"))
    } else {
        None
    }
}

/// Get the color of a specific Emacs face, to be used in a color spec.
///
/// This is usually, but not always a RGB code starting with #.
fn face_color(env: &Env, face: &emacs::OnceGlobalRef, fg: bool) -> Result<String> {
    let hex: String = env
        .call(
            if fg { face_foreground } else { face_background },
            (face, (), default_face),
        )?
        .into_rust()?;
    Ok(hex)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexed_to_rgb_handpicked() {
        // cornflower blue
        assert_eq!(Some("#5f87ff"), indexed_to_rgb(69).as_deref());

        // dark sea green 2 light
        assert_eq!(Some("#afffaf"), indexed_to_rgb(157).as_deref());

        // grey15
        assert_eq!(Some("#262626"), indexed_to_rgb(235).as_deref());

        // grey85
        assert_eq!(Some("#dadada"), indexed_to_rgb(253).as_deref());
    }

    #[test]
    fn indexed_to_rgb_unsupported() {
        for code in 0..16 {
            assert_eq!(None, indexed_to_rgb(code));
        }
    }

    #[test]
    fn indexed_to_rgb_colors() {
        for red in 0..6 {
            for green in 0..6 {
                for blue in 0..6 {
                    let code = 16 + (red * 36) + (green * 6) + blue;
                    let r = if red != 0 { red * 40 + 55 } else { 0 };
                    let g = if green != 0 { green * 40 + 55 } else { 0 };
                    let b = if blue != 0 { blue * 40 + 55 } else { 0 };
                    assert_eq!(
                        Some(format!("#{r:02x}{g:02x}{b:02x}")),
                        indexed_to_rgb(code),
                        "({red}, {green}, {blue})"
                    );
                }
            }
        }
    }

    #[test]
    fn indexed_to_rgb_grayscale() {
        for gray in 0..24 {
            let level = gray * 10 + 8;
            let code = 232 + gray;
            assert_eq!(
                Some(format!("#{level:02x}{level:02x}{level:02x}")),
                indexed_to_rgb(code),
                "gray: {gray}"
            );
        }
    }
}
