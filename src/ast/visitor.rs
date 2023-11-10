use super::{expression::*, statement::*};

pub trait Visitor {
  fn enter_expression(&mut self, _: &Expression) {}
  fn exit_expression(&mut self, _: &Expression) {}

  fn enter_statement(&mut self, _: &Statement) {}
  fn exit_statement(&mut self, _: &Statement) {}

  fn visit_expression(&mut self, expression: &Expression) {
    self.enter_expression(expression);

    match expression {
      Expression::Binary(x) => self.visit_binary(x),
      Expression::Block(x) => self.visit_block(x),
      Expression::Call(x) => self.visit_call(x),
      Expression::Comment(x) => self.visit_comment(x),
      Expression::Function(x) => self.visit_function(x),
      Expression::Group(x) => self.visit_group(x),
      Expression::If(x) => self.visit_if(x),
      Expression::Unary(x) => self.visit_unary(x),
      Expression::Literal(_) | Expression::Variable(_) => {}
    }

    self.exit_expression(expression);
  }
  fn visit_binary(&mut self, binary: &Binary) {
    self.visit_expression(&binary.left);
    self.visit_expression(&binary.right);
  }
  fn visit_block(&mut self, block: &Block) {
    for statement in &block.statements {
      self.visit_statement(statement);
    }
  }
  fn visit_call(&mut self, call: &Call) {
    self.visit_expression(&call.expression);
    if let Some(argument) = &call.argument {
      self.visit_expression(argument);
    }
  }
  fn visit_comment(&mut self, comment: &Comment) {
    self.visit_expression(&comment.expression);
  }
  fn visit_function(&mut self, function: &Function) {
    self.visit_expression(&function.body);
  }
  fn visit_group(&mut self, group: &Group) {
    self.visit_expression(&group.expression);
  }
  fn visit_if(&mut self, if_: &If) {
    self.visit_expression(&if_.condition);
    self.visit_expression(&if_.then);
    if let Some(otherwise) = &if_.otherwise {
      self.visit_expression(otherwise);
    }
  }
  fn visit_unary(&mut self, unary: &Unary) {
    self.visit_expression(&unary.expression);
  }

  fn visit_statement(&mut self, statement: &Statement) {
    self.enter_statement(statement);

    match statement {
      Statement::Comment(_) => {}
      Statement::Expression(x) => self.visit_expression(x),
      Statement::Let(x) => self.visit_let(x),
    }

    self.exit_statement(statement);
  }
  fn visit_let(&mut self, let_: &Let) {
    self.visit_expression(&let_.expression);
  }
}
