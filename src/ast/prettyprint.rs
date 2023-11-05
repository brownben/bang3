//! Pretty-print the AST into a human readable format.
//!
//! Based upon this [blog post](https://www.georgevreilly.com/blog/2023/01/24/TreeInRust2PrintingTrees.html)

use super::expression::*;
use std::fmt;

impl fmt::Display for Expression<'_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.pretty(f, "", false)
  }
}

const OTHER_CHILD: &str = "│  ";
const OTHER_ENTRY: &str = "├─ ";
const FINAL_CHILD: &str = "   ";
const FINAL_ENTRY: &str = "╰─ ";

pub trait PrettyPrint {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result;
}

impl PrettyPrint for Expression<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    match self {
      Expression::Binary(x) => x.pretty(f, prefix, last),
      Expression::Call(x) => x.pretty(f, prefix, last),
      Expression::Comment(x) => x.pretty(f, prefix, last),
      Expression::Group(x) => x.pretty(f, prefix, last),
      Expression::If(x) => x.pretty(f, prefix, last),
      Expression::Literal(x) => x.pretty(f, prefix, last),
      Expression::Unary(x) => x.pretty(f, prefix, last),
      Expression::Variable(x) => x.pretty(f, prefix, last),
    }
  }
}

impl PrettyPrint for Binary<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };

    writeln!(f, "{prefix}{connector}Binary ({})", self.operator)?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.left.pretty(f, &new_prefix, false)?;
    self.right.pretty(f, &new_prefix, true)
  }
}
impl PrettyPrint for Call<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, _last: bool) -> fmt::Result {
    self.expression.pretty(f, prefix, false)?;

    writeln!(f, "{prefix}{OTHER_CHILD}{FINAL_ENTRY}Call")?;
    if let Some(argument) = &self.argument {
      argument.pretty(f, &format!("{prefix}{OTHER_CHILD}{FINAL_CHILD}"), true)?;
    }

    Ok(())
  }
}
impl PrettyPrint for Comment<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, _last: bool) -> fmt::Result {
    self.expression.pretty(f, prefix, false)?;

    writeln!(
      f,
      "{prefix}{OTHER_CHILD}{FINAL_ENTRY}Comment ({})",
      self.text.trim()
    )
  }
}
impl PrettyPrint for Group<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Group")?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression.pretty(f, &new_prefix, true)
  }
}
impl PrettyPrint for If<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}If")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let child_prefix = format!("{prefix}{OTHER_CHILD}");
    let final_child_prefix = format!("{prefix}{FINAL_CHILD}");

    writeln!(f, "{prefix}{OTHER_ENTRY}Condition")?;
    self.condition.pretty(f, &child_prefix, true)?;

    if let Some(otherwise) = &self.otherwise {
      writeln!(f, "{prefix}{OTHER_ENTRY}Then")?;
      self.then.pretty(f, &child_prefix, true)?;

      writeln!(f, "{prefix}{FINAL_ENTRY}Otherwise")?;
      otherwise.pretty(f, &final_child_prefix, true)?;
    } else {
      writeln!(f, "{prefix}{FINAL_ENTRY}Then")?;
      self.then.pretty(f, &final_child_prefix, true)?;
    }

    Ok(())
  }
}
impl PrettyPrint for Literal<'_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };

    match self.kind {
      LiteralKind::Boolean(x) => writeln!(f, "{prefix}{connector}Boolean ({x})"),
      LiteralKind::Number(x) => writeln!(f, "{prefix}{connector}Number ({x})"),
      LiteralKind::String(x) => writeln!(f, "{prefix}{connector}String '{x}'"),
    }
  }
}
impl PrettyPrint for Unary<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Unary ({})", self.operator)?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression.pretty(f, &new_prefix, true)
  }
}
impl PrettyPrint for Variable<'_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Variable ({})", self.name)
  }
}
