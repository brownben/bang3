use super::formatter::{Formattable, Formatter, IR};
use bang_parser::{
  ast::{expression::*, statement::*},
  GetSpan, AST,
};

impl<'a, 'b> Formattable<'a, 'b> for AST<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    let mut last_line = 0;
    f.concat_iterator(self.statements.iter().map(|statement| {
      let gap_to_previous = f.line_index.get_line(statement.span()) > last_line + 1;
      last_line = f.line_index.get_final_line(statement.span());

      if gap_to_previous {
        f.concat([IR::AlwaysLine, statement.format(f), IR::AlwaysLine])
      } else {
        f.concat([statement.format(f), IR::AlwaysLine])
      }
    }))
  }
}

impl<'a, 'b> Formattable<'a, 'b> for Expression<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    match self {
      Expression::Binary(binary) => binary.format(f),
      Expression::Block(block) => block.format(f),
      Expression::Call(call) => call.format(f),
      Expression::Comment(comment) => comment.format(f),
      Expression::FormatString(format_string) => format_string.format(f),
      Expression::Function(function) => function.format(f),
      Expression::Group(group) => group.format(f),
      Expression::If(if_) => if_.format(f),
      Expression::Literal(literal) => literal.format(f),
      Expression::Match(match_) => match_.format(f),
      Expression::Unary(unary) => unary.format(f),
      Expression::Variable(variable) => variable.format(f),
      Expression::Invalid(_) => IR::Empty,
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Binary<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if self.operator == BinaryOperator::Pipeline {
      return f.group([
        self.left.format(f),
        f.indent([IR::LineOrSpace, IR::Text(">> "), self.right.format(f)]),
      ]);
    }

    f.concat([
      self.left.format(f),
      IR::Text(" "),
      IR::Text(self.operator.as_str()),
      IR::Text(" "),
      self.right.format(f),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Block<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if self.statements.len() == 1
      && let Statement::Expression(expression) = &self.statements[0]
      && let Expression::Block(block) = expression.as_ref()
    {
      return block.format(f);
    }

    // Blocks that can be written on a single line
    if self.statements.len() == 1 {
      let is_single_expression = matches!(
        &self.statements[0],
        Statement::Expression(e)
          if !matches!(**e, Expression::Comment(_))
      );
      let is_single_return = matches!(
        &self.statements[0],
        Statement::Return(e)
          if !matches!(e.expression, Expression::Comment(_))
      );

      let statement = self.statements[0].format(f);
      if is_single_expression || is_single_return {
        return f.group([
          IR::Text("{"),
          f.indent([IR::LineOrSpace, statement]),
          IR::LineOrSpace,
          IR::Text("}"),
        ]);
      }
    }

    let mut last_line = f.line_index.get_line(self.statements[0].span());
    let statements = f.concat_iterator(self.statements.iter().map(|statement| {
      let gap_to_previous = f.line_index.get_line(statement.span()) > last_line + 1;
      last_line = f.line_index.get_final_line(statement.span());

      if gap_to_previous {
        f.concat([IR::AlwaysLine, IR::AlwaysLine, statement.format(f)])
      } else {
        f.concat([IR::AlwaysLine, statement.format(f)])
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
impl<'a, 'b> Formattable<'a, 'b> for Call<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if let Some(argument) = &self.argument {
      // If it contains a comment, we have to break to preserve the comment.
      let line = if let Expression::Comment(_) = argument {
        IR::AlwaysLine
      } else {
        IR::Line
      };

      f.group([
        self.expression.format(f),
        IR::Text("("),
        f.indent([line, argument.unwrap_groups().format(f)]),
        IR::Line,
        IR::Text(")"),
      ])
    } else {
      f.concat([self.expression.format(f), IR::Text("()")])
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Comment<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      self.expression.format(f),
      IR::Text(" // "),
      IR::Text(self.text.trim()),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for FormatString<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      IR::Text("`"),
      f.concat_iterator(self.strings.iter().zip(self.expressions.iter()).map(
        |(string, expression)| {
          f.concat([
            IR::Text(string.string),
            IR::Text("{"),
            expression.format(f),
            IR::Text("}"),
          ])
        },
      )),
      IR::Text(self.strings.last().unwrap().string),
      IR::Text("`"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Function<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      IR::Text(self.parameter.name),
      IR::Text(" => "),
      self.body.format(f),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Group<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if matches!(
      self.expression,
      Expression::Block(_) | Expression::Function(_)
    ) {
      return f.concat([IR::Text("("), self.expression.format(f), IR::Text(")")]);
    }

    if let Expression::Group(group) = &self.expression {
      return group.format(f);
    }

    // If it contains a comment, we have to break to preserve the comment.
    let line = if let Expression::Comment(_) = self.expression {
      IR::AlwaysLine
    } else {
      IR::Line
    };

    f.group([
      IR::Text("("),
      f.indent([line, self.expression.format(f)]),
      IR::Line,
      IR::Text(")"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for If<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    // If it contains a comment, we have to break to preserve the comment.
    let line = if let Expression::Comment(_) = self.condition {
      IR::AlwaysLine
    } else {
      IR::Line
    };

    f.concat([
      f.group([
        IR::Text("if ("),
        f.indent([line, self.condition.unwrap_groups().format(f)]),
        IR::Line,
        IR::Text(") "),
      ]),
      self.then.format(f),
      IR::Text(" else "),
      self.otherwise.format(f),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Literal<'a> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    match self.kind {
      LiteralKind::Boolean(true) => IR::Text("true"),
      LiteralKind::Boolean(false) => IR::Text("false"),
      LiteralKind::Number { raw, .. } => IR::Text(raw),
      LiteralKind::String(string) => {
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
impl<'a, 'b> Formattable<'a, 'b> for Match<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      IR::Text("match "),
      self.value.format(f),
      f.indent([f.concat_iterator(
        self
          .cases
          .iter()
          .map(|case| f.concat([IR::AlwaysLine, case.format(f)])),
      )]),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for MatchCase<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      IR::Text("| "),
      self.pattern.format(f),
      self
        .guard
        .as_ref()
        .map(|guard| f.concat([IR::Text(" if "), guard.format(f)]))
        .unwrap_or_default(),
      IR::Text(" -> "),
      self.expression.format(f),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Pattern<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    match self {
      Pattern::Identifier(variable) => variable.format(f),
      Pattern::Literal(literal) => literal.format(f),
      Pattern::Range(range) => range.format(f),
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b> for PatternRange<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([self.start.format(f), IR::Text(".."), self.end.format(f)])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Unary<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if let Expression::Unary(unary2) = &self.expression
      && self.operator == unary2.operator
      && let Expression::Unary(unary3) = &unary2.expression
      && unary2.operator == unary3.operator
    {
      return unary3.format(f);
    }

    f.concat([IR::Text(self.operator.as_str()), self.expression.format(f)])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Variable<'a> {
  fn format(&self, _: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    IR::Text(self.name)
  }
}

impl<'a, 'b> Formattable<'a, 'b> for Statement<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    match self {
      Statement::Comment(comment) => comment.format(f),
      Statement::Expression(expression) => expression.format(f),
      Statement::Let(let_) => let_.format(f),
      Statement::Return(return_) => return_.format(f),
    }
  }
}
impl<'a, 'b> Formattable<'a, 'b> for CommentStmt<'a> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([IR::Text("// "), IR::Text(self.text.trim())])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Let<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      IR::Text("let "),
      IR::Text(self.identifier.name),
      IR::Text(" = "),
      self.expression.format(f),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Return<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([IR::Text("return "), self.expression.format(f)])
  }
}
