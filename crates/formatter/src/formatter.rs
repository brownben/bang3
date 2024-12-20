use crate::config::{self, Config};
use bumpalo::{Bump as Allocator, boxed::Box, collections::Vec};
use std::{fmt, marker, mem};

/// An item which can be formatted
pub trait Formattable<'source, 'allocator, AST> {
  /// Convert the item into the formatting intermediate representation
  fn format(
    &self,
    f: &Formatter<'source, 'allocator>,
    ast: &'source AST,
  ) -> IR<'source, 'allocator>;
}
impl<'a, 'b, T: Formattable<'a, 'b, AST>, AST> Formattable<'a, 'b, AST> for Option<&T> {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    self.as_ref().map(|x| x.format(f, ast)).unwrap_or_default()
  }
}

/// Formatter used to create then print the intermediate formatting representation
pub struct Formatter<'source, 'allocator> {
  pub(crate) config: Config,
  pub(crate) allocator: &'allocator Allocator,
  _source: marker::PhantomData<&'source ()>,
}
impl<'ast: 'allocator, 'allocator> Formatter<'ast, 'allocator> {
  pub(crate) fn format(
    ast: &bang_syntax::AST,
    config: Config,
    allocator: &'allocator Allocator,
  ) -> String {
    Self {
      config,
      allocator,
      _source: marker::PhantomData,
    }
    .print(ast, ast)
  }

  /// Format the AST Node into a string
  fn print<AST>(&self, item: &dyn Formattable<'ast, 'allocator, AST>, ast: &'ast AST) -> String {
    let ir = item.format(self, ast);
    let display_ir = ir.display(0, 0, true, self.config, self.allocator);
    format!("{display_ir}")
  }

  /// Create a new indentation node
  pub(crate) fn indent<const N: usize>(
    &self,
    mut ir: [IR<'ast, 'allocator>; N],
  ) -> IR<'ast, 'allocator> {
    let ir = match N {
      1 => mem::take(&mut ir[0]),
      _ => self.concat(ir),
    };

    IR::Indent(Box::new_in(ir, self.allocator))
  }

  /// Create a new group, an option for the formatter to break the source on
  pub(crate) fn group<const N: usize>(
    &self,
    mut ir: [IR<'ast, 'allocator>; N],
  ) -> IR<'ast, 'allocator> {
    let ir = match N {
      1 => mem::take(&mut ir[0]),
      _ => self.concat(ir),
    };

    IR::Group(Box::new_in(ir, self.allocator))
  }

  /// Merge multiple IRs into a single IR
  pub(crate) fn concat<const N: usize>(
    &self,
    mut ir: [IR<'ast, 'allocator>; N],
  ) -> IR<'ast, 'allocator> {
    match N {
      1 => mem::take(&mut ir[0]),
      _ => IR::Concat(Vec::from_iter_in(ir, self.allocator)),
    }
  }

  /// Merge multiple IRs together from an iterator
  pub(crate) fn concat_iterator(
    &self,
    ir: impl Iterator<Item = IR<'ast, 'allocator>>,
  ) -> IR<'ast, 'allocator> {
    IR::Concat(Vec::from_iter_in(ir, self.allocator))
  }
}

/// Representation of part of a file to be formatted
#[derive(Default)]
pub enum IR<'source, 'allocator> {
  /// No content
  #[default]
  Empty,
  /// Borrowed String content (up to a single line)
  Text(&'source str),
  /// A possible line break, or nothing
  Line,
  /// Always a line break
  AlwaysLine,
  /// A possible line break, or a space
  LineOrSpace,
  /// Give two options, use the first if the group is broken, else the second
  Option(&'source str, &'source str),
  /// A combination of multiple IRs
  Concat(Vec<'allocator, IR<'source, 'allocator>>),
  /// Indent the given IR, if a line break present
  Indent(Box<'allocator, IR<'source, 'allocator>>),
  /// Mark a section where there are different options to break source code
  Group(Box<'allocator, IR<'source, 'allocator>>),
}
impl<'a, 'b> IR<'a, 'b> {
  /// Check if the option has any always lines
  /// Used to make sure that groups break proeprly around match statements
  fn has_always_line(&self) -> bool {
    match self {
      IR::AlwaysLine => true,
      IR::Empty | IR::Text(_) | IR::LineOrSpace | IR::Line | IR::Option(_, _) => false,
      IR::Concat(x) => x.iter().any(IR::has_always_line),
      IR::Indent(ir) => ir.has_always_line(),
      IR::Group(group) => group.has_always_line(),
    }
  }

  /// Convert the IR into a displayable form
  fn display(
    &self,
    line_length: u16,
    indentation: u16,
    flatten: bool,
    config: Config,
    allocator: &'b Allocator,
  ) -> DisplayIR<'a, 'b> {
    match self {
      IR::Empty => DisplayIR::Empty,
      IR::Text(text) => DisplayIR::Text(text),

      IR::LineOrSpace if flatten => DisplayIR::Text(" "),
      IR::Line if flatten => DisplayIR::Empty,
      IR::Line | IR::AlwaysLine | IR::LineOrSpace => DisplayIR::Line {
        depth: indentation,
        indentation: config.indentation,
        line_ending: config.line_ending,
      },

      IR::Option(a, _) if flatten => DisplayIR::Text(a),
      IR::Option(_, b) => DisplayIR::Text(b),

      IR::Indent(ir) => ir.display(line_length, indentation + 1, flatten, config, allocator),
      IR::Concat(items) => DisplayIR::Collection({
        let mut collection = Vec::new_in(allocator);
        let mut line_length = line_length;

        for ir in items {
          let ir = ir.display(line_length, indentation, flatten, config, allocator);

          if ir.contains_line() {
            line_length = ir.end_len();
          } else {
            line_length += ir.len();
          }

          collection.push(ir);
        }
        collection
      }),
      IR::Group(ir) => {
        let option_a = ir.display(line_length, indentation, true, config, allocator);
        let has_forced_line = ir.has_always_line();

        if !has_forced_line && option_a.fits(config.print_width, line_length) {
          option_a
        } else {
          ir.display(line_length, indentation, false, config, allocator)
        }
      }
    }
  }
}
impl fmt::Debug for IR<'_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Empty => write!(f, "Empty"),
      Self::Text(text) => write!(f, "Text '{text}'"),
      Self::LineOrSpace => write!(f, "Line"),
      Self::Line => write!(f, "OptionalLine"),
      Self::AlwaysLine => write!(f, "ForcedLine"),
      Self::Concat(items) => f.debug_list().entries(items).finish(),
      Self::Indent(ir) => f.debug_tuple("Indent").field(ir).finish(),
      Self::Group(a) => f.debug_tuple("Union").field(a).finish(),
      Self::Option(a, b) => f.debug_tuple("Option").field(a).field(b).finish(),
    }
  }
}

/// Final representation which can directly be printed
/// Created so length of current line (and possible variants) can be calculated
#[derive(Debug)]
pub enum DisplayIR<'a, 'b> {
  Empty,
  Text(&'a str),
  Line {
    depth: u16,
    indentation: config::Indentation,
    line_ending: config::LineEnding,
  },
  Collection(Vec<'b, DisplayIR<'a, 'b>>),
}
impl DisplayIR<'_, '_> {
  /// Will the display item fit in the given size?
  pub fn fits(&self, max_width: u16, current_length: u16) -> bool {
    let size = i32::from(max_width) - i32::from(current_length);

    if size < 0 {
      return false;
    };

    match self {
      DisplayIR::Empty | DisplayIR::Line { .. } => true,
      DisplayIR::Text(text) => size >= i32::try_from(text.len()).unwrap(),
      DisplayIR::Collection(_) => size >= i32::from(self.len()),
    }
  }

  /// The length of the display item on the current line
  pub fn len(&self) -> u16 {
    match self {
      DisplayIR::Empty => 0,
      DisplayIR::Text(text) => u16::try_from(text.len()).unwrap(),
      DisplayIR::Line {
        depth, indentation, ..
      } => indentation.len() * depth,
      DisplayIR::Collection(x) => x
        .iter()
        .take_while(|x| !matches!(x, DisplayIR::Line { .. }))
        .map(DisplayIR::len)
        .sum(),
    }
  }

  /// The length of the display item on the final line
  pub fn end_len(&self) -> u16 {
    match self {
      DisplayIR::Empty | DisplayIR::Line { .. } => 0,
      DisplayIR::Text(text) => u16::try_from(text.len()).unwrap(),
      DisplayIR::Collection(x) => x
        .iter()
        .rev()
        .take_while(|x| !x.contains_line())
        .map(DisplayIR::end_len)
        .sum(),
    }
  }

  /// Does the current display item contain a line?
  fn contains_line(&self) -> bool {
    match self {
      DisplayIR::Line { .. } => true,
      DisplayIR::Collection(x) => x.iter().any(DisplayIR::contains_line),
      DisplayIR::Empty | DisplayIR::Text(_) => false,
    }
  }
}
impl fmt::Display for DisplayIR<'_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Empty => Ok(()),
      Self::Text(text) => write!(f, "{text}"),
      Self::Line {
        depth,
        indentation,
        line_ending,
      } => {
        f.write_str(line_ending.as_str())?;
        (0..*depth).try_for_each(|_| write!(f, "{indentation}"))
      }
      Self::Collection(x) => x.iter().try_for_each(|item| write!(f, "{item}")),
    }
  }
}
