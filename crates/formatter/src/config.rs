//! The configuration options for the formatter
use std::fmt;

/// Configuration for the formatter
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Config {
  /// The max print width to aim for
  pub print_width: u16,
  /// Use single quotes for strings
  pub single_quotes: bool,
  /// The number of spaces to use for indentation, if 0 use tabs
  pub indentation: Indentation,
  /// The line ending to use
  pub line_ending: LineEnding,
}
impl Default for Config {
  fn default() -> Self {
    Self {
      print_width: 80,
      single_quotes: true,
      indentation: Indentation::Space(2),
      line_ending: LineEnding::Native,
    }
  }
}

/// The indentation to use when printing
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Indentation {
  /// Use this number of spaces for indentation
  Space(u16),
  /// Use tabs for indentation
  Tab,
}
impl Indentation {
  pub(super) fn len(self) -> u16 {
    match self {
      Self::Space(n) => n,
      Self::Tab => 2,
    }
  }
}
impl From<u16> for Indentation {
  /// The number of spaces to use for indentation. If 0 use tabs.
  fn from(n: u16) -> Self {
    if n == 0 {
      Self::Tab
    } else {
      Self::Space(n)
    }
  }
}
impl fmt::Display for Indentation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Space(n) => (0..*n).try_for_each(|_| write!(f, " ")),
      Self::Tab => write!(f, "\t"),
    }
  }
}

/// The type of line endings to use for the file
#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub enum LineEnding {
  ///  Line Feed only (\n), common on Linux and macOS as well as inside git repos
  LineFeed,

  /// Carriage Return + Line Feed characters (\r\n), common on Windows
  CarriageReturnLineFeed,

  /// Line endings will be converted to `\n` on Unix and `\r\n` on Windows.
  #[default]
  Native,
}
impl LineEnding {
  #[inline]
  pub(super) const fn as_str(self) -> &'static str {
    match self {
      LineEnding::LineFeed => "\n",
      LineEnding::CarriageReturnLineFeed => "\r\n",

      #[cfg(not(target_os = "windows"))]
      LineEnding::Native => "\n",
      #[cfg(target_os = "windows")]
      LineEnding::Native => "\r\n",
    }
  }

  #[inline]
  pub(super) const fn len(self) -> u32 {
    match self {
      LineEnding::LineFeed => 1,
      LineEnding::CarriageReturnLineFeed => 2,

      #[cfg(not(target_os = "windows"))]
      LineEnding::Native => 1,
      #[cfg(target_os = "windows")]
      LineEnding::Native => 2,
    }
  }
}
