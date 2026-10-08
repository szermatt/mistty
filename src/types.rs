use alacritty_terminal::index::Line;
use emacs::{Env, IntoLisp, Result};
use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, AddAssign, Sub, SubAssign},
};

emacs::use_functions! {
    point_func => "point"
    point_max_func => "point-max"
    point_min_func => "point-min"
    pos_bol_func => "pos-bol"
}

/// Newtype that represents an Emacs buffer position.
///
/// This is a char number inside of an Emacs buffer, starting at 1.
///
/// To work with buffer pos, get a position from Emacs Lisp, usually
/// with BufferPos::point(), then add or subtract character count from
/// it. To obtain character count from a string, do
/// `str.chars().cout`.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Ord, PartialOrd)]
pub struct BufferPos(pub i32);

impl BufferPos {
    /// Return the position of the point in the current buffer.
    pub fn point(env: &Env) -> Result<Self> {
        Ok(BufferPos(env.call(point_func, [])?.into_rust()?))
    }

    /// Return the highest valid position of the point.
    pub fn point_max(env: &Env) -> Result<Self> {
        Ok(BufferPos(env.call(point_max_func, [])?.into_rust()?))
    }

    /// Return the beginning-of-line position of the give line.
    ///
    /// `line` is relative to the current buffer, `Line(0)` returns
    /// the beginning of the current line.
    pub fn bol(env: &Env, line: Line) -> Result<Self> {
        Ok(BufferPos(
            env.call(pos_bol_func, (line.0 + 1,))?.into_rust()?,
        ))
    }
}
impl fmt::Display for BufferPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<usize> for BufferPos {
    fn from(source: usize) -> Self {
        Self(source as i32)
    }
}

impl Add<usize> for BufferPos {
    type Output = BufferPos;

    #[inline]
    fn add(self, rhs: usize) -> BufferPos {
        BufferPos(self.0 + rhs as i32)
    }
}

impl AddAssign<usize> for BufferPos {
    #[inline]
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs as i32;
    }
}

impl Sub<BufferPos> for BufferPos {
    type Output = i32;

    #[inline]
    fn sub(self, rhs: BufferPos) -> i32 {
        self.0 - rhs.0
    }
}

impl Sub<usize> for BufferPos {
    type Output = BufferPos;

    #[inline]
    fn sub(self, rhs: usize) -> BufferPos {
        BufferPos(self.0 - rhs as i32)
    }
}

impl SubAssign<usize> for BufferPos {
    #[inline]
    fn sub_assign(&mut self, rhs: usize) {
        self.0 -= rhs as i32;
    }
}

impl PartialOrd<usize> for BufferPos {
    #[inline]
    fn partial_cmp(&self, other: &usize) -> Option<Ordering> {
        self.0.partial_cmp(&(*other as i32))
    }
}

impl PartialEq<usize> for BufferPos {
    #[inline]
    fn eq(&self, other: &usize) -> bool {
        self.0.eq(&(*other as i32))
    }
}

impl<'e> IntoLisp<'e> for BufferPos {
    fn into_lisp(self, env: &'e emacs::Env) -> Result<emacs::Value<'e>> {
        self.0.into_lisp(env)
    }
}
