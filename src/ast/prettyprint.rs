//! Pretty-print the AST into a human readable format.
//!
//! Based upon this [blog post](https://www.georgevreilly.com/blog/2023/01/24/TreeInRust2PrintingTrees.html)

use super::{expression::*, statement::*, AST};
use std::fmt;

impl fmt::Display for AST<'_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for statement in &self.statements {
      statement.pretty(f, "", false)?;
    }

    Ok(())
  }
}
impl fmt::Display for Statement<'_, '_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.pretty(f, "", false)
  }
}
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
      Self::Binary(x) => x.pretty(f, prefix, last),
      Self::Block(x) => x.pretty(f, prefix, last),
      Self::Call(x) => x.pretty(f, prefix, last),
      Self::Comment(x) => x.pretty(f, prefix, last),
      Self::Function(x) => x.pretty(f, prefix, last),
      Self::Group(x) => x.pretty(f, prefix, last),
      Self::If(x) => x.pretty(f, prefix, last),
      Self::Literal(x) => x.pretty(f, prefix, last),
      Self::Match(x) => x.pretty(f, prefix, last),
      Self::Unary(x) => x.pretty(f, prefix, last),
      Self::Variable(x) => x.pretty(f, prefix, last),
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
impl PrettyPrint for Block<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Block")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let (last_statement, statements) = self.statements.split_last().unwrap();
    for statement in statements {
      statement.pretty(f, &prefix, false)?;
    }
    last_statement.pretty(f, &prefix, true)
  }
}
impl PrettyPrint for Call<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Call")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let child_prefix = format!("{prefix}{OTHER_CHILD}");
    let final_child_prefix = format!("{prefix}{FINAL_CHILD}");

    writeln!(f, "{prefix}{OTHER_ENTRY}Callee")?;
    self.expression.pretty(f, &child_prefix, true)?;

    if let Some(argument) = &self.argument {
      writeln!(f, "{prefix}{FINAL_ENTRY}Argument")?;
      argument.pretty(f, &final_child_prefix, true)?;
    }

    Ok(())
  }
}
impl PrettyPrint for Comment<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Comment ({})", self.text.trim())?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression.pretty(f, &new_prefix, true)
  }
}
impl PrettyPrint for Function<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    write!(f, "{prefix}{connector}Function")?;
    if let Some(name) = self.name {
      write!(f, " ({name})")?;
    }
    writeln!(f, ": {} =>", self.parameter)?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.body.pretty(f, &new_prefix, true)
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

    writeln!(f, "{prefix}{OTHER_ENTRY}Then")?;
    self.then.pretty(f, &child_prefix, true)?;

    writeln!(f, "{prefix}{FINAL_ENTRY}Otherwise")?;
    self.otherwise.pretty(f, &final_child_prefix, true)?;

    Ok(())
  }
}
impl PrettyPrint for Literal<'_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };

    match self.kind {
      LiteralKind::Boolean(x) => writeln!(f, "{prefix}{connector}Boolean ({x})"),
      LiteralKind::Number { value, .. } => writeln!(f, "{prefix}{connector}Number ({value})"),
      LiteralKind::String(x) => writeln!(f, "{prefix}{connector}String '{x}'"),
    }
  }
}
impl PrettyPrint for Match<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Match")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.value.pretty(f, &prefix, false)?;

    writeln!(f, "{prefix}{FINAL_ENTRY}Cases:")?;
    let (last_case, cases) = self.cases.split_last().unwrap();
    for case in cases {
      case.pretty(f, &prefix, false)?;
    }
    last_case.pretty(f, &prefix, true)?;

    Ok(())
  }
}
impl PrettyPrint for MatchCase<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let prefix = format!("{prefix}{FINAL_CHILD}");
    self.pattern.pretty(f, &prefix, last)?;

    let gap = if last { FINAL_CHILD } else { OTHER_CHILD };
    self.expression.pretty(f, &format!("{prefix}{gap}"), true)
  }
}
impl PrettyPrint for Pattern<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    fn display_pattern_part(f: &mut fmt::Formatter<'_>, pattern: &Pattern<'_, '_>) -> fmt::Result {
      match pattern {
        Pattern::Literal(x) => match x.kind {
          LiteralKind::Boolean(x) => write!(f, "{x}"),
          LiteralKind::Number { value, .. } => write!(f, "{value}"),
          LiteralKind::String(x) => write!(f, "'{x}'"),
        },
        Pattern::Identifier(x) => write!(f, "{}", x.name),
        Pattern::Range(_) => unreachable!("not a basic pattern part"),
      }
    }

    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    write!(f, "{prefix}{connector}Pattern ─ ")?;

    match self {
      Self::Range(range) => {
        if let Some(start) = &range.start {
          display_pattern_part(f, start)?;
          write!(f, " ")?;
        }
        write!(f, "..")?;
        if let Some(end) = &range.end {
          write!(f, " ")?;
          display_pattern_part(f, end)?;
        };
      }
      pattern => display_pattern_part(f, pattern)?,
    };

    writeln!(f)
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

impl PrettyPrint for Statement<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    match self {
      Self::Comment(x) => x.pretty(f, prefix, last),
      Self::Expression(x) => x.pretty(f, prefix, last),
      Self::Let(x) => x.pretty(f, prefix, last),
    }
  }
}
impl PrettyPrint for CommentStmt<'_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Comment ({})", self.text.trim())
  }
}
impl PrettyPrint for Let<'_, '_> {
  fn pretty(&self, f: &mut fmt::Formatter<'_>, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Let '{}' =", self.identifier)?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression.pretty(f, &new_prefix, true)
  }
}
