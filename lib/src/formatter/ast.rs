use super::{Formattable, Formatter, IR};
use crate::{
  ast::{expression::*, statement::*, GetSpan},
  parser::AST,
};

impl<'a, 'b> Formattable<'a, 'b> for AST<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    let mut last_line = 0;
    f.concat_iterator(self.statements.iter().map(|statement| {
      let gap_to_previous = statement.span().start > last_line + f.config.line_ending.len();
      last_line = statement.span().end;

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
      Expression::Function(function) => function.format(f),
      Expression::Group(group) => group.format(f),
      Expression::If(if_) => if_.format(f),
      Expression::Literal(literal) => literal.format(f),
      Expression::Match(match_) => match_.format(f),
      Expression::Unary(unary) => unary.format(f),
      Expression::Variable(variable) => variable.format(f),
      Expression::Invalid => IR::Empty,
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

    let line = if self.statements.len() > 1 {
      IR::AlwaysLine
    } else {
      IR::LineOrSpace
    };
    let statements = f.concat_iterator(
      self
        .statements
        .iter()
        .map(|statement| f.concat([IR::LineOrSpace, statement.format(f)])),
    );

    f.group([IR::Text("{"), f.indent([statements]), line, IR::Text("}")])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for Call<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    if let Some(argument) = &self.argument {
      f.group([
        self.expression.format(f),
        IR::Text("("),
        f.indent([IR::Line, argument.unwrap().format(f)]),
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
    if matches!(self.expression, Expression::Block(_)) {
      return f.concat([IR::Text("("), self.expression.format(f), IR::Text(")")]);
    }

    if let Expression::Group(group) = &self.expression {
      return group.format(f);
    }

    f.group([
      IR::Text("("),
      f.indent([IR::Line, self.expression.format(f)]),
      IR::Line,
      IR::Text(")"),
    ])
  }
}
impl<'a, 'b> Formattable<'a, 'b> for If<'a, '_> {
  fn format(&self, f: &Formatter<'a, 'b>) -> IR<'a, 'b> {
    f.concat([
      f.group([
        IR::Text("if ("),
        f.indent([IR::Line, self.condition.unwrap().format(f)]),
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
