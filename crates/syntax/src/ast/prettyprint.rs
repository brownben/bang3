//! Pretty-print the AST into a human readable format.
//!
//! Based upon this [blog post](https://www.georgevreilly.com/blog/2023/01/24/TreeInRust2PrintingTrees.html)

use super::{AST, expression::*, statement::*, types::*};
use std::fmt;

impl fmt::Display for AST {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for statement in &self.root_statements {
      statement.pretty(f, self, "", false)?;
    }

    Ok(())
  }
}

const OTHER_CHILD: &str = "│  ";
const OTHER_ENTRY: &str = "├─ ";
const FINAL_CHILD: &str = "   ";
const FINAL_ENTRY: &str = "╰─ ";

pub trait PrettyPrint {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result;
}

impl PrettyPrint for Expression {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    match self {
      Self::Binary(x) => x.pretty(f, ast, prefix, last),
      Self::Block(x) => x.pretty(f, ast, prefix, last),
      Self::Call(x) => x.pretty(f, ast, prefix, last),
      Self::Comment(x) => x.pretty(f, ast, prefix, last),
      Self::FormatString(x) => x.pretty(f, ast, prefix, last),
      Self::Function(x) => x.pretty(f, ast, prefix, last),
      Self::Group(x) => x.pretty(f, ast, prefix, last),
      Self::If(x) => x.pretty(f, ast, prefix, last),
      Self::List(x) => x.pretty(f, ast, prefix, last),
      Self::Literal(x) => x.pretty(f, ast, prefix, last),
      Self::Match(x) => x.pretty(f, ast, prefix, last),
      Self::ModuleAccess(x) => x.pretty(f, ast, prefix, last),
      Self::Unary(x) => x.pretty(f, ast, prefix, last),
      Self::Variable(x) => x.pretty(f, ast, prefix, last),
      Self::Invalid(_) => {
        let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
        writeln!(f, "{prefix}{connector}Invalid")
      }
    }
  }
}
impl PrettyPrint for Binary {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Binary ({})", self.operator(ast))?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.left(ast).pretty(f, ast, &new_prefix, false)?;
    self.right(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for Block {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Block")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let (last_statement, statements) = self.statements.split_last().unwrap();
    for statement in statements {
      ast[*statement].pretty(f, ast, &prefix, false)?;
    }
    ast[*last_statement].pretty(f, ast, &prefix, true)
  }
}
impl PrettyPrint for Call {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Call")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let child_prefix = format!("{prefix}{OTHER_CHILD}");
    let final_child_prefix = format!("{prefix}{FINAL_CHILD}");

    writeln!(f, "{prefix}{OTHER_ENTRY}Callee")?;
    self.callee(ast).pretty(f, ast, &child_prefix, true)?;

    if let Some(argument) = &self.argument(ast) {
      writeln!(f, "{prefix}{FINAL_ENTRY}Argument")?;
      argument.pretty(f, ast, &final_child_prefix, true)?;
    }

    Ok(())
  }
}
impl PrettyPrint for Comment {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Comment ({})", self.text(ast))?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for FormatString {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Format String")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });

    for (expression, string) in self.expressions(ast).zip(self.strings(ast)) {
      writeln!(f, "{prefix}{OTHER_ENTRY}'{string}'")?;
      expression.pretty(f, ast, &prefix, false)?;
    }

    let last_string = self.strings(ast).last().unwrap();
    writeln!(f, "{prefix}{FINAL_ENTRY}'{last_string}'")?;

    Ok(())
  }
}
impl PrettyPrint for Function {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    write!(f, "{prefix}{connector}Function")?;
    if let Some(name) = &self.name(ast) {
      write!(f, " ({name})")?;
    }
    writeln!(f, ": {} =>", self.parameter.name(ast))?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.body(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for Group {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Group")?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for If {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}If")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let child_prefix = format!("{prefix}{OTHER_CHILD}");
    let final_child_prefix = format!("{prefix}{FINAL_CHILD}");

    writeln!(f, "{prefix}{OTHER_ENTRY}Condition")?;
    self.condition(ast).pretty(f, ast, &child_prefix, true)?;

    if let Some(otherwise) = &self.otherwise(ast) {
      writeln!(f, "{prefix}{OTHER_ENTRY}Then")?;
      self.then(ast).pretty(f, ast, &child_prefix, true)?;

      writeln!(f, "{prefix}{FINAL_ENTRY}Otherwise")?;
      otherwise.pretty(f, ast, &final_child_prefix, true)?;
    } else {
      writeln!(f, "{prefix}{FINAL_ENTRY}Then")?;
      self.then(ast).pretty(f, ast, &final_child_prefix, true)?;
    }

    Ok(())
  }
}
impl PrettyPrint for List {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}List")?;

    let items: Vec<_> = self.items(ast).collect();
    if items.is_empty() {
      return Ok(());
    }

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let (last, items) = items.split_last().unwrap();
    for item in items {
      item.pretty(f, ast, &new_prefix, false)?;
    }
    last.pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for Literal {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };

    match self.value(ast) {
      LiteralValue::Boolean(x) => writeln!(f, "{prefix}{connector}Boolean ({x})"),
      LiteralValue::Number(x) => writeln!(f, "{prefix}{connector}Number ({x})"),
      LiteralValue::String(x) => writeln!(f, "{prefix}{connector}String '{x}'"),
    }
  }
}
impl PrettyPrint for Match {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Match")?;

    let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.value(ast).pretty(f, ast, &prefix, false)?;

    writeln!(f, "{prefix}{FINAL_ENTRY}Cases:")?;
    let (last_case, cases) = self.arms.split_last().unwrap();
    for case in cases {
      case.pretty(f, ast, &prefix, false)?;
    }
    last_case.pretty(f, ast, &prefix, true)?;

    Ok(())
  }
}
impl PrettyPrint for MatchArm {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix_raw: &str, last: bool) -> fmt::Result {
    let prefix = format!("{prefix_raw}{FINAL_CHILD}");
    self.pattern.pretty(f, ast, &prefix, last)?;

    if let Some(guard) = &self.guard(ast) {
      let prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
      let child_prefix = format!("{prefix}{OTHER_CHILD}");

      writeln!(f, "{prefix}{OTHER_ENTRY}Guard")?;
      guard.pretty(f, ast, &child_prefix, true)?;
      self.expression(ast).pretty(f, ast, &prefix, true)
    } else {
      let gap = if last { FINAL_CHILD } else { OTHER_CHILD };
      self
        .expression(ast)
        .pretty(f, ast, &format!("{prefix}{gap}"), true)
    }
  }
}
impl PrettyPrint for Pattern {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    fn display_literal(f: &mut fmt::Formatter, literal: &Literal, ast: &AST) -> fmt::Result {
      match literal.value(ast) {
        LiteralValue::Boolean(x) => write!(f, "{x}"),
        LiteralValue::Number(x) => write!(f, "{x}"),
        LiteralValue::String(x) => write!(f, "'{x}'"),
      }
    }

    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    write!(f, "{prefix}{connector}Pattern ─ ")?;

    match self {
      Self::Identifier(identifier) => write!(f, "{}", identifier.name(ast))?,
      Self::Literal(literal) => display_literal(f, literal, ast)?,
      Self::Range(range) => {
        if let Some(start) = &range.start {
          display_literal(f, start, ast)?;
          write!(f, " ")?;
        }
        write!(f, "..")?;
        if let Some(end) = &range.end {
          write!(f, " ")?;
          display_literal(f, end, ast)?;
        };
      }
      Self::List(list_pattern) => {
        write!(f, "[")?;
        match (list_pattern.first(ast), list_pattern.rest(ast)) {
          (Some(first), Some(rest)) => write!(f, "{}, ..{}", first.name(ast), rest.name(ast))?,
          (Some(first), None) => write!(f, "{}", first.name(ast))?,
          (None, Some(rest)) => write!(f, "..{}", rest.name(ast))?,
          (None, None) => {}
        };
        write!(f, "]")?;
      }
      Self::Option(option) => {
        if let Some(variable) = option.variable() {
          write!(f, "Some({})", variable.name(ast))?;
        } else {
          write!(f, "None")?;
        }
      }
      Self::Invalid => write!(f, "invalid")?,
    };

    writeln!(f)
  }
}
impl PrettyPrint for ModuleAccess {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    let module = self.module(ast);
    let item = self.item(ast);

    writeln!(f, "{prefix}{connector}Module Access ({module}::{item})",)
  }
}
impl PrettyPrint for Unary {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Unary ({})", self.operator(ast))?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for Variable {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Variable ({})", self.name(ast))
  }
}

impl PrettyPrint for Statement {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    match self {
      Self::Comment(x) => x.pretty(f, ast, prefix, last),
      Self::Expression(x) => x.expression(ast).pretty(f, ast, prefix, last),
      Self::Import(x) => x.pretty(f, ast, prefix, last),
      Self::Let(x) => x.pretty(f, ast, prefix, last),
      Self::Return(x) => x.pretty(f, ast, prefix, last),
    }
  }
}
impl PrettyPrint for CommentStmt {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    write!(f, "{prefix}{connector}Comment (",)?;
    let mut comment_lines = self.text(ast);
    write!(f, "{}", comment_lines.next().unwrap())?; // There is always at least one line
    for line in comment_lines {
      write!(f, " {line}")?;
    }
    writeln!(f, ")")
  }
}
impl PrettyPrint for Import {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}From '{}' Import", self.module(ast))?;

    let items: Vec<ImportItem> = self.items(ast).collect();
    if items.is_empty() {
      return Ok(());
    }

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    let (last, items) = items.split_last().unwrap();
    for item in items {
      item.pretty(f, ast, &new_prefix, false)?;
    }
    last.pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for ImportItem<'_> {
  fn pretty(&self, f: &mut fmt::Formatter, _ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    if let Some(alias) = self.alias {
      writeln!(f, "{prefix}{connector}{} as {alias}", self.name)
    } else {
      writeln!(f, "{prefix}{connector}{}", self.name)
    }
  }
}
impl PrettyPrint for Let {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    if let Some(doc_comment) = self.doc_comment(ast) {
      doc_comment.pretty(f, ast, prefix, false)?;
    }

    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Let '{}' =", self.identifier(ast))?;

    if let Some(annotation) = self.annotation(ast) {
      write!(f, "{prefix}{OTHER_CHILD}{OTHER_ENTRY}Type: ")?;
      annotation.pretty(f, ast, "", false)?;
      writeln!(f)?;
    }

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.value(ast).pretty(f, ast, &new_prefix, true)
  }
}
impl PrettyPrint for Return {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    let connector = if last { FINAL_ENTRY } else { OTHER_ENTRY };
    writeln!(f, "{prefix}{connector}Return")?;

    let new_prefix = format!("{prefix}{}", if last { FINAL_CHILD } else { OTHER_CHILD });
    self.expression(ast).pretty(f, ast, &new_prefix, true)
  }
}

impl PrettyPrint for Type {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    match self {
      Type::Primitive(type_primitive) => type_primitive.pretty(f, ast, prefix, last),
      Type::Variable(type_variable) => type_variable.pretty(f, ast, prefix, last),
      Type::Function(type_function) => type_function.pretty(f, ast, prefix, last),
      Type::Group(type_group) => type_group.pretty(f, ast, prefix, last),
      Type::Structure(structure) => structure.pretty(f, ast, prefix, last),
      Type::Invalid(_) => write!(f, "Invalid"),
    }
  }
}
impl PrettyPrint for TypePrimitive {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, _: &str, _: bool) -> fmt::Result {
    write!(f, "{}", self.name(ast))
  }
}
impl PrettyPrint for TypeVariable {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, _: &str, _: bool) -> fmt::Result {
    write!(f, "^{}", self.name(ast))
  }
}
impl PrettyPrint for TypeFunction {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    self.parameter(ast).pretty(f, ast, prefix, last)?;
    write!(f, " => ")?;
    self.return_(ast).pretty(f, ast, prefix, last)
  }
}
impl PrettyPrint for TypeGroup {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    write!(f, "(")?;
    self.type_(ast).pretty(f, ast, prefix, last)?;
    write!(f, ")")
  }
}
impl PrettyPrint for TypeStructure {
  fn pretty(&self, f: &mut fmt::Formatter, ast: &AST, prefix: &str, last: bool) -> fmt::Result {
    write!(f, "{}<", self.structure(ast))?;
    self.parameter(ast).pretty(f, ast, prefix, last)?;
    write!(f, ">")
  }
}
