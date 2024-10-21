use super::formatter::{Formattable, Formatter, IR};
use bang_syntax::{
  ast::{expression::*, statement::*},
  AST,
};

impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for AST<'_> {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    let mut last_line = 0;
    f.concat_iterator(self.root_statements.iter().map(|statement| {
      let gap_to_previous = ast.line_index().line(statement.span(ast)) > last_line + 1;
      last_line = ast.line_index().final_line(statement.span(ast));

      if gap_to_previous {
        f.concat([IR::AlwaysLine, statement.format(f, ast), IR::AlwaysLine])
      } else {
        f.concat([statement.format(f, ast), IR::AlwaysLine])
      }
    }))
  }
}

impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Expression {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    match self {
      Expression::Binary(binary) => binary.format(f, ast),
      Expression::Block(block) => block.format(f, ast),
      Expression::Call(call) => call.format(f, ast),
      Expression::Comment(comment) => comment.format(f, ast),
      Expression::FormatString(format_string) => format_string.format(f, ast),
      Expression::Function(function) => function.format(f, ast),
      Expression::Group(group) => group.format(f, ast),
      Expression::If(if_) => if_.format(f, ast),
      Expression::Literal(literal) => literal.format(f, ast),
      Expression::Match(match_) => match_.format(f, ast),
      Expression::Unary(unary) => unary.format(f, ast),
      Expression::Variable(variable) => variable.format(f, ast),
      Expression::Invalid(_) => IR::Empty,
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Binary {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    if self.operator(ast) == BinaryOperator::Pipeline {
      return f.group([
        self.left(ast).format(f, ast),
        f.indent([
          IR::LineOrSpace,
          IR::Text(">> "),
          self.right(ast).format(f, ast),
        ]),
      ]);
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Block {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Call {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Comment {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    f.concat([
      self.expression(ast).format(f, ast),
      IR::Text(" // "),
      IR::Text(self.text(ast)),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for FormatString {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Function {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.parameter.name(ast)),
      IR::Text(" => "),
      self.body(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Group {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for If {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Literal {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    match self.value(ast) {
      LiteralValue::Boolean(true) => IR::Text("true"),
      LiteralValue::Boolean(false) => IR::Text("false"),
      LiteralValue::Number(_) => IR::Text(self.raw_value(ast)),
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Match {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for MatchArm {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Pattern {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Unary {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    if let Expression::Unary(unary2) = &self.expression(ast)
      && self.operator(ast) == unary2.operator(ast)
      && let Expression::Unary(unary3) = &unary2.expression(ast)
      && unary2.operator(ast) == unary3.operator(ast)
    {
      return unary3.format(f, ast);
    }

    f.concat([
      IR::Text(self.operator(ast).as_str()),
      self.expression(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Variable {
  fn format(&self, _: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    IR::Text(self.name(ast))
  }
}

impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Statement {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    match self {
      Statement::Comment(comment) => comment.format(f, ast),
      Statement::Expression(expression) => expression.expression(ast).format(f, ast),
      Statement::Import(import) => import.format(f, ast),
      Statement::Let(let_) => let_.format(f, ast),
      Statement::Return(return_) => return_.format(f, ast),
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for CommentStmt {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    f.concat([IR::Text("// "), IR::Text(self.text(ast))])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Import {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    let mut items: Vec<_> = self.items(ast).collect();
    if f.config.sort_imports && !items.is_sorted_by_key(|item| item.name) {
      items.sort_by_key(|item| item.name);
    };

    let items_in_line = if items.is_empty() {
      IR::Text(" ")
    } else {
      let (last, items) = items.split_last().unwrap();

      f.concat([
        IR::Text(" "),
        f.concat_iterator(
          items
            .iter()
            .map(|item| f.concat([item.format(f, ast), IR::Text(", ")])),
        ),
        last.format(f, ast),
      ])
    };

    let items_on_new_lines = f.indent([f.concat_iterator(
      items
        .iter()
        .map(|item| f.concat([IR::LineOrSpace, item.format(f, ast), IR::Text(",")])),
    )]);

    f.concat([
      IR::Text("from "),
      IR::Text(self.module(ast)),
      IR::Text(" import "),
      f.group([
        IR::Text("{"),
        f.option(items_in_line, items_on_new_lines),
        IR::LineOrSpace,
        IR::Text("}"),
      ]),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for ImportItem<'a> {
  fn format(&self, f: &Formatter<'a, 'b>, _ast: &AST<'a>) -> IR<'a, 'b> {
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
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Let {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    f.concat([
      IR::Text("let "),
      IR::Text(self.identifier(ast)),
      IR::Text(" = "),
      self.value(ast).format(f, ast),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b, AST<'a>> for Return {
  fn format(&self, f: &Formatter<'a, 'b>, ast: &AST<'a>) -> IR<'a, 'b> {
    f.concat([IR::Text("return "), self.expression(ast).format(f, ast)])
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
