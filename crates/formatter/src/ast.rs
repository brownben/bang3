use super::formatter::{Formattable, Formatter, IR};
use bang_syntax::{
  AST,
  ast::{expression::*, statement::*, types::*},
};
use bumpalo::collections::Vec;

impl<'a, 'b> Formattable<'a, 'b, AST> for AST {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if self.root_statements.is_empty() {
      return IR::Empty;
    }

    let line_index = ast.line_index();
    let mut last_line = line_index.line(self.root_statements.first().unwrap().span(ast));

    f.concat_iterator(self.root_statements.iter().map(|statement| {
      let gap_to_previous = line_index.line(statement.span(ast)) > last_line + 1;
      last_line = line_index.final_line(statement.span(ast));

      if gap_to_previous {
        f.concat([IR::AlwaysLine, statement.format(f, ast), IR::AlwaysLine])
      } else {
        f.concat([statement.format(f, ast), IR::AlwaysLine])
      }
    }))
  }
}

impl<'a, 'b> Formattable<'a, 'b, AST> for Expression {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    match self {
      Expression::Binary(binary) => binary.format(f, ast),
      Expression::Block(block) => block.format(f, ast),
      Expression::Call(call) => call.format(f, ast),
      Expression::Comment(comment) => comment.format(f, ast),
      Expression::FormatString(format_string) => format_string.format(f, ast),
      Expression::Function(function) => function.format(f, ast),
      Expression::Group(group) => group.format(f, ast),
      Expression::If(if_) => if_.format(f, ast),
      Expression::List(list) => list.format(f, ast),
      Expression::Literal(literal) => literal.format(f, ast),
      Expression::Match(match_) => match_.format(f, ast),
      Expression::ModuleAccess(module_access) => module_access.format(f, ast),
      Expression::Unary(unary) => unary.format(f, ast),
      Expression::Variable(variable) => variable.format(f, ast),
      Expression::Invalid(_) => IR::Empty,
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Binary {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if self.operator(ast) == BinaryOperator::Pipeline {
      // If a pipeline, flatten it so all operators break at the same time
      let mut output = Vec::new_in(f.allocator);
      pipeline(f, self, ast, &mut output);
      return f.group([f.indent([IR::Concat(output)])]);
    }

    f.concat([
      self.left(ast).format(f, ast),
      IR::Text(" "),
      IR::Text(self.operator(ast).as_str()),
      IR::Text(" "),
      self.right(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Block {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if self.len() == 1
      && let Statement::Expression(expression) = self.statement(0, ast)
      && let Expression::Block(block) = expression.expression(ast)
    {
      return block.format(f, ast);
    }

    // Blocks that can be written on a single line
    if self.len() == 1 {
      let statement = self.statement(0, ast);
      let is_single_expression = matches!(
        statement,
        Statement::Expression(e)
          if !matches!(e.expression(ast), Expression::Comment(_))
      );
      let is_single_return = matches!(
        statement,
        Statement::Return(e)
          if !matches!(e.expression(ast), Expression::Comment(_))
      );

      let statement = statement.format(f, ast);
      if is_single_expression || is_single_return {
        return f.group([
          IR::Text("{"),
          f.indent([IR::LineOrSpace, statement]),
          IR::LineOrSpace,
          IR::Text("}"),
        ]);
      }
    }

    let line_index = ast.line_index();
    let mut last_line = line_index.line(self.statements(ast).next().unwrap().span(ast));
    let statements = f.concat_iterator(self.statements(ast).map(|statement| {
      let gap_to_previous = line_index.line(statement.span(ast)) > last_line + 1;
      last_line = line_index.final_line(statement.span(ast));

      if gap_to_previous {
        f.concat([IR::AlwaysLine, IR::AlwaysLine, statement.format(f, ast)])
      } else {
        f.concat([IR::AlwaysLine, statement.format(f, ast)])
      }
    }));

    f.group([
      IR::Text("{"),
      f.indent([statements]),
      IR::AlwaysLine,
      IR::Text("}"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Call {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if let Some(argument) = self.argument(ast) {
      // If it contains a comment, we have to break to preserve the comment.
      let line = if let Expression::Comment(_) = argument {
        IR::AlwaysLine
      } else {
        IR::Line
      };

      f.group([
        self.callee(ast).format(f, ast),
        IR::Text("("),
        f.indent([line, unwrap(argument, ast).format(f, ast)]),
        IR::Line,
        IR::Text(")"),
      ])
    } else {
      f.concat([self.callee(ast).format(f, ast), IR::Text("()")])
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Comment {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      self.expression(ast).format(f, ast),
      IR::Text(" // "),
      IR::Text(self.text(ast)),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for FormatString {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    let mut strings = self.strings(ast);
    let first_string = strings.next().unwrap();

    f.concat([
      IR::Text("`"),
      IR::Text(first_string),
      f.concat_iterator(
        self
          .expressions(ast)
          .zip(strings)
          .map(|(expression, string)| {
            f.concat([
              IR::Text("{"),
              expression.format(f, ast),
              IR::Text("}"),
              IR::Text(string),
            ])
          }),
      ),
      IR::Text("`"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Function {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.parameter.name(ast)),
      IR::Text(" => "),
      self.body(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Group {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if let Expression::Block(_) | Expression::Function(_) = self.expression(ast) {
      return f.concat([
        IR::Text("("),
        self.expression(ast).format(f, ast),
        IR::Text(")"),
      ]);
    }

    if let Expression::Group(group) = self.expression(ast) {
      return group.format(f, ast);
    }

    // If it contains a comment, we have to break to preserve the comment.
    let line = if let Expression::Comment(_) = self.expression(ast) {
      IR::AlwaysLine
    } else {
      IR::Line
    };

    f.group([
      IR::Text("("),
      f.indent([line, self.expression(ast).format(f, ast)]),
      IR::Line,
      IR::Text(")"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for If {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    // If it contains a comment, we have to break to preserve the comment.
    let line = if let Expression::Comment(_) = self.condition(ast) {
      IR::AlwaysLine
    } else {
      IR::Line
    };

    f.concat([
      f.group([
        IR::Text("if ("),
        f.indent([line, unwrap(self.condition(ast), ast).format(f, ast)]),
        IR::Line,
        IR::Text(") "),
      ]),
      self.then(ast).format(f, ast),
      if let Some(otherwise) = self.otherwise(ast) {
        f.concat([IR::Text(" else "), otherwise.format(f, ast)])
      } else {
        IR::Empty
      },
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for List {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if self.length() == 0 {
      return IR::Text("[]");
    }

    let items = Vec::from_iter_in(self.items(ast), f.allocator);
    let (last, items) = items.split_last().unwrap();

    f.group([
      IR::Text("["),
      f.indent([
        IR::Line,
        f.concat_iterator(
          items.iter().map(|expression| {
            f.concat([expression.format(f, ast), IR::Text(","), IR::LineOrSpace])
          }),
        ),
      ]),
      last.format(f, ast),
      IR::Option("", ","),
      IR::Line,
      IR::Text("]"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Literal {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    match self.value(ast) {
      LiteralValue::Boolean(true) => IR::Text("true"),
      LiteralValue::Boolean(false) => IR::Text("false"),
      LiteralValue::Number(_) => {
        let raw_value = self.raw_value(ast);

        if raw_value.contains('_') {
          IR::Text(raw_value)
        } else {
          remove_leading_trailing_zeros(f, raw_value)
        }
      }
      LiteralValue::String(string) => {
        let quote = if f.config.single_quotes && !string.contains('\'') {
          "'"
        } else {
          "\""
        };

        f.concat([IR::Text(quote), IR::Text(string), IR::Text(quote)])
      }
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Match {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text("match "),
      self.value(ast).format(f, ast),
      f.indent([f.concat_iterator(
        self
          .arms()
          .map(|case| f.concat([IR::AlwaysLine, case.format(f, ast)])),
      )]),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for MatchArm {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text("| "),
      self.pattern.format(f, ast),
      self
        .guard(ast)
        .as_ref()
        .map(|guard| f.concat([IR::Text(" if "), guard.format(f, ast)]))
        .unwrap_or_default(),
      IR::Text(" -> "),
      self.expression(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Pattern {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    match self {
      Pattern::Identifier(variable) => variable.format(f, ast),
      Pattern::Literal(literal) => literal.format(f, ast),
      Pattern::Range(start, end) => f.concat([
        (*start).format(f, ast),
        IR::Text(".."),
        (*end).format(f, ast),
      ]),
      Pattern::Invalid => IR::Empty,
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Unary {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    if let Expression::Unary(unary2) = &self.expression(ast)
      && self.operator(ast) == unary2.operator(ast)
    {
      if let Expression::Unary(unary3) = &unary2.expression(ast)
        && unary2.operator(ast) == unary3.operator(ast)
      {
        return unary3.format(f, ast);
      }

      if self.operator(ast) == UnaryOperator::Minus
        && let Expression::Literal(literal) = &unary2.expression(ast)
        && let LiteralValue::Number(number) = literal.value(ast)
        && number.is_sign_negative()
      {
        return literal.format(f, ast);
      }
    }

    f.concat([
      IR::Text(self.operator(ast).as_str()),
      self.expression(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for ModuleAccess {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.module(ast)),
      IR::Text("::"),
      IR::Text(self.item(ast)),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Variable {
  fn format(&self, _: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    IR::Text(self.name(ast))
  }
}

impl<'a, 'b> Formattable<'a, 'b, AST> for Statement {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    match self {
      Statement::Comment(comment) => comment.format(f, ast),
      Statement::Expression(expression) => expression.expression(ast).format(f, ast),
      Statement::Import(import) => import.format(f, ast),
      Statement::Let(let_) => let_.format(f, ast),
      Statement::Return(return_) => return_.format(f, ast),
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for CommentStmt {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    comment(f, self, ast, "// ")
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Import {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    let mut items = Vec::from_iter_in(self.items(ast), f.allocator);
    if f.config.sort_imports && !items.is_sorted_by_key(|item| item.name) {
      items.sort_by_key(|item| item.name);
    };

    if items.is_empty() {
      return f.concat([
        IR::Text("from "),
        IR::Text(self.module(ast)),
        IR::Text(" import {  }"),
      ]);
    }

    let (last, items) = items.split_last().unwrap();

    let items = f.indent([
      IR::LineOrSpace,
      f.concat_iterator(
        items
          .iter()
          .map(|item| f.concat([item.format(f, ast), IR::Text(","), IR::LineOrSpace])),
      ),
      last.format(f, ast),
      IR::Option("", ","),
    ]);

    f.concat([f.group([
      IR::Text("from "),
      IR::Text(self.module(ast)),
      IR::Text(" import {"),
      items,
      IR::LineOrSpace,
      IR::Text("}"),
    ])])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for ImportItem<'a> {
  fn format(&self, f: &Formatter<'a, 'b>, _ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.name),
      if let Some(alias) = &self.alias
        && *alias != self.name
      {
        f.concat([IR::Text(" as "), IR::Text(alias)])
      } else {
        IR::Empty
      },
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Let {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      if let Some(doc_comment) = self.doc_comment(ast) {
        f.concat([comment(f, doc_comment, ast, "/// "), IR::AlwaysLine])
      } else {
        IR::Empty
      },
      IR::Text("let "),
      IR::Text(self.identifier(ast)),
      if let Some(annotation) = self.annotation(ast) {
        f.concat([IR::Text(": "), annotation.format(f, ast)])
      } else {
        IR::Empty
      },
      IR::Text(" = "),
      self.value(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for Return {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([IR::Text("return "), self.expression(ast).format(f, ast)])
  }
}

impl<'a, 'b> Formattable<'a, 'b, AST> for Type {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    match self {
      Type::Primitive(type_primitive) => type_primitive.format(f, ast),
      Type::Variable(type_variable) => type_variable.format(f, ast),
      Type::Function(type_function) => type_function.format(f, ast),
      Type::Group(type_group) => type_group.format(f, ast),
      Type::Structure(structure) => structure.format(f, ast),
      Type::Invalid(_) => IR::Empty,
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for TypePrimitive {
  fn format(&self, _: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    IR::Text(self.name(ast))
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for TypeVariable {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([IR::Text("^"), IR::Text(self.name(ast))])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for TypeFunction {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      self.parameter(ast).format(f, ast),
      IR::Text(" => "),
      self.return_(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for TypeGroup {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    let inner = self.type_(ast);
    match inner {
      Type::Group(group) => return group.format(f, ast),
      Type::Primitive(primitive) => return primitive.format(f, ast),
      Type::Variable(variable) => return variable.format(f, ast),
      _ => {}
    }

    f.group([
      IR::Text("("),
      f.indent([IR::Line, inner.format(f, ast)]),
      IR::Line,
      IR::Text(")"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST> for TypeStructure {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &'a AST) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.structure(ast)),
      IR::Text("<"),
      self.parameter(ast).format(f, ast),
      IR::Text(">"),
    ])
  }
}

/// Extract a single expression from a group or block
fn unwrap<'a>(expression: &'a Expression, ast: &'a AST) -> &'a Expression {
  match expression {
    Expression::Group(group) => unwrap(group.expression(ast), ast),
    Expression::Block(block) => {
      if block.len() == 1
        && let Some(statement) = block.statements(ast).next()
        && let Statement::Expression(expression) = statement
      {
        unwrap(expression.expression(ast), ast)
      } else {
        expression
      }
    }
    _ => expression,
  }
}

/// Make sure that numbers don't have any unnecessary leading or trailing zeros
/// Also ensure numbers don't start with a decimal point
fn remove_leading_trailing_zeros<'a, 'b>(f: &Formatter<'a, 'b>, raw: &'a str) -> IR<'a, 'b> {
  // if it doesn't have unnecessary zeros, return it
  if !raw.starts_with('0') && !raw.starts_with('-') && !raw.starts_with('.') && !raw.ends_with('0')
  {
    return IR::Text(raw);
  }

  let (raw, negative) = if let Some(raw) = raw.strip_prefix('-') {
    (raw, true)
  } else {
    (raw, false)
  };

  // If it is a decimal, we make sure that there is at least one digit each side of the point
  if let Some((before, after)) = raw.split_once('.') {
    let mut leading_zeros = before.len() - before.trim_start_matches('0').len();
    let mut trailing_zeros = after.len() - after.trim_end_matches('0').len();

    // If there is no leading zeros, we add a 0 before the point
    if before.is_empty() {
      return f.concat([
        if negative { IR::Text("-") } else { IR::Empty },
        IR::Text("0"),
        IR::Text(&raw[leading_zeros..(raw.len() - trailing_zeros)]),
      ]);
    }

    // Make sure that there is at least one digit each side of the point
    if trailing_zeros == after.len() {
      trailing_zeros -= 1;
    }
    if leading_zeros == before.len() {
      leading_zeros -= 1;
    }

    return f.concat([
      if negative { IR::Text("-") } else { IR::Empty },
      IR::Text(&raw[leading_zeros..(raw.len() - trailing_zeros)]),
    ]);
  }

  // Otherwise its just an integer, and we check there is atleast one digit
  let integer_part = if raw.trim_start_matches('0').is_empty() {
    IR::Text("0")
  } else {
    IR::Text(raw.trim_start_matches('0'))
  };

  f.concat([
    if negative { IR::Text("-") } else { IR::Empty },
    integer_part,
  ])
}

/// Flatten a Pipeline chain into a single group, which all breaks at once
fn pipeline<'a, 'b>(
  f: &Formatter<'a, 'b>,
  expression: &Binary,
  ast: &'a AST,
  output: &mut Vec<IR<'a, 'b>>,
) {
  if let Expression::Binary(left) = expression.left(ast)
    && left.operator(ast) == BinaryOperator::Pipeline
  {
    pipeline(f, left, ast, output);
  } else {
    output.push(expression.left(ast).format(f, ast));
  }

  output.extend([
    IR::LineOrSpace,
    IR::Text(">> "),
    expression.right(ast).format(f, ast),
  ]);
}
fn comment<'a, 'b>(
  f: &Formatter<'a, 'b>,
  comment: &CommentStmt,
  ast: &'a AST,
  line_start: &'static str,
) -> IR<'a, 'b> {
  let mut lines = comment.text(ast);

  f.concat([
    IR::Text(line_start),
    IR::Text(lines.next().unwrap()), // There is always at least one line
    f.concat_iterator(
      lines.map(|text| f.concat([IR::AlwaysLine, IR::Text(line_start), IR::Text(text)])),
    ),
  ])
}
