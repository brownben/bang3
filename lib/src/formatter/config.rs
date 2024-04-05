use std::fmt;

/// Configuration for the formatter
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Config {
  /// The max print width to aim for
  pub print_width: u16,
  /// Use single quotes for strings
  pub single_quotes: bool,
  /// The number of spaces to use for indentation, if 0 use tabs
  pub indentation: u8,
}
impl Config {
  pub(crate) fn indentation(self) -> Indentation {
    self.indentation.into()
  }
}
impl Default for Config {
  fn default() -> Self {
    Self {
      print_width: 80,
      single_quotes: true,
      indentation: 2,
    }
  }
}

/// The indentation to use when printing
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Indentation {
  Space(u8),
  Tab,
}
impl Indentation {
  pub(super) fn len(self) -> u16 {
    match self {
      Self::Space(n) => u16::from(n),
      Self::Tab => 2,
    }
  }
}
impl From<u8> for Indentation {
  /// The number of spaces to use for indentation. If 0 use tabs.
  fn from(n: u8) -> Self {
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
