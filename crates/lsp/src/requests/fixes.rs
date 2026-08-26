use super::diagnostics::IntoDiagnostic;
use crate::documents::Document;
use crate::locations::{lsp_range_from_span, span_from_lsp_range};

use lsp_types as lsp;
use std::collections::HashMap;

use bang_syntax::Span;

pub fn fixes(file: &Document, range: lsp::Range) -> Vec<lsp::CodeAction> {
  let range = span_from_lsp_range(range, file);
  let mut actions = vec![];

  parse_errors::fixes(file, range, &mut actions);
  type_errors::fixes(file, range, &mut actions);
  refactors::fixes(file, range, &mut actions);

  actions
}

fn insert_edit(file: &Document, location: u32, replacement: &str) -> lsp::WorkspaceEdit {
  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), vec![lsp::TextEdit {
    range: lsp_range_from_span(Span::new(location, location), file),
    new_text: replacement.to_owned(),
  }])]))
}

fn replace_edit(file: &Document, span: Span, replacement: &str) -> lsp::WorkspaceEdit {
  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), vec![lsp::TextEdit {
    range: lsp_range_from_span(span, file),
    new_text: replacement.to_owned(),
  }])]))
}

fn surround_edit(file: &Document, span: Span, before: &str, after: &str) -> lsp::WorkspaceEdit {
  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), vec![
    lsp::TextEdit {
      range: lsp_range_from_span(Span::new(span.start, span.start), file),
      new_text: before.to_owned(),
    },
    lsp::TextEdit {
      range: lsp_range_from_span(Span::new(span.end, span.end), file),
      new_text: after.to_owned(),
    },
  ])]))
}

fn multiple_edits(
  file: &Document,
  edits: impl IntoIterator<Item = (Span, String)>,
) -> lsp::WorkspaceEdit {
  let text_edits = edits
    .into_iter()
    .map(|(span, new_text)| lsp::TextEdit {
      range: lsp_range_from_span(span, file),
      new_text,
    })
    .collect();

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}

fn delete_edit(file: &Document, span: Span) -> lsp::WorkspaceEdit {
  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), vec![lsp::TextEdit {
    range: lsp_range_from_span(span, file),
    new_text: String::new(),
  }])]))
}

mod parse_errors {
  use crate::requests::diagnostics::IntoDiagnostic;

  use super::{Document, Span, delete_edit, replace_edit, surround_edit};
  use bang_syntax::ParseError;
  use lsp_types as lsp;

  pub fn fixes(file: &Document, range: Span, actions: &mut Vec<lsp::CodeAction>) {
    actions.extend(
      (file.ast.errors.iter())
        .filter(|error| range.contains(error.span()) || error.span().contains(range))
        .filter_map(|error| match error {
          ParseError::ReturnOutsideFunction(token) => {
            Some(return_outside_function(file, error, token.into()))
          }
          ParseError::InvalidAssignmentTarget(token) => {
            Some(invalid_assignment_target(file, error, token.into()))
          }
          ParseError::ReturnAsExpression { statement, .. } => {
            Some(return_as_expression(file, error, *statement))
          }
          ParseError::ExtraDot(token) => Some(extra_dot(file, error, token.into())),
          _ => None,
        }),
    );
  }

  fn invalid_assignment_target(file: &Document, error: &ParseError, span: Span) -> lsp::CodeAction {
    lsp::CodeAction {
      title: "Replace with `==`".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(replace_edit(file, span, "==")),
      ..Default::default()
    }
  }

  fn return_outside_function(file: &Document, error: &ParseError, span: Span) -> lsp::CodeAction {
    lsp::CodeAction {
      title: "Delete `return`".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(delete_edit(file, span)),
      ..Default::default()
    }
  }

  fn return_as_expression(file: &Document, error: &ParseError, span: Span) -> lsp::CodeAction {
    lsp::CodeAction {
      title: "Wrap `return` in a block".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(surround_edit(file, span, "{ ", " }")),
      ..Default::default()
    }
  }

  fn extra_dot(file: &Document, error: &ParseError, span: Span) -> lsp::CodeAction {
    lsp::CodeAction {
      title: "Delete extra `.`".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(delete_edit(file, span)),
      ..Default::default()
    }
  }
}

mod type_errors {
  use super::{Document, IntoDiagnostic, Span, delete_edit, insert_edit, replace_edit};
  use bang_typechecker::TypeError;
  use lsp_types as lsp;

  pub fn fixes(file: &Document, range: Span, actions: &mut Vec<lsp::CodeAction>) {
    (file.typechecker().problems().iter())
      .filter(|error| range.contains(error.span()) || error.span().contains(range))
      .for_each(|error| match error {
        TypeError::UndefinedVariable { .. } => undefined_variable(file, error, actions),
        TypeError::UnreachableCase { span } => actions.push(unreachable_case(file, error, *span)),
        TypeError::UnusedVariable { span, .. } => actions.push(unused_variable(file, error, *span)),

        #[rustfmt::skip]
        TypeError::ItemNotFound { span, did_you_mean: Some(suggestion), .. }
        | TypeError::ModuleNotFound { span, did_you_mean: Some(suggestion), .. }
        | TypeError::UnknownTypeAnnotation { span, did_you_mean: Some(suggestion) } => {
          actions.push(unknown_item(file, error, suggestion, *span));
        }

        TypeError::ModuleAccessAlreadyImported { .. } => {
          actions.push(already_imported_item(file, error));
        }

        _ => {}
      });
  }

  fn undefined_variable(file: &Document, error: &TypeError, actions: &mut Vec<lsp::CodeAction>) {
    let TypeError::UndefinedVariable {
      identifier,
      span,
      did_you_mean,
      possible_imports,
    } = error
    else {
      unreachable!("Wrong Error Type Passed");
    };

    if let Some(suggestion) = did_you_mean {
      actions.push(lsp::CodeAction {
        title: format!("Replace with `{suggestion}`"),
        kind: Some(lsp::CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![error.diagnostic(file)]),
        edit: Some(replace_edit(file, *span, suggestion)),
        ..Default::default()
      });
    }

    for module in possible_imports {
      actions.push(lsp::CodeAction {
        title: format!("Qualify as `{module}::{identifier}`"),
        kind: Some(lsp::CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![error.diagnostic(file)]),
        edit: Some(insert_edit(file, span.start, &format!("{module}::"))),
        ..Default::default()
      });
    }
  }

  fn unreachable_case(file: &Document, error: &TypeError, span: Span) -> lsp::CodeAction {
    lsp::CodeAction {
      title: "Delete Unreachable Case".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(delete_edit(file, span)),
      ..Default::default()
    }
  }

  fn unknown_item(
    file: &Document,
    error: &TypeError,
    suggestion: &str,
    span: Span,
  ) -> lsp::CodeAction {
    lsp::CodeAction {
      title: format!("Replace with `{suggestion}`"),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(replace_edit(file, span, suggestion)),
      ..Default::default()
    }
  }

  fn unused_variable(file: &Document, error: &TypeError, span: Span) -> lsp::CodeAction {
    // TODO: Suggestion to delete the declaration, but not the expression

    lsp::CodeAction {
      title: "Prefix with `_`".to_owned(),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(insert_edit(file, span.start, "_")),
      ..Default::default()
    }
  }

  fn already_imported_item(file: &Document, error: &TypeError) -> lsp::CodeAction {
    let TypeError::ModuleAccessAlreadyImported {
      path,
      defined_as,
      span,
      ..
    } = error
    else {
      unreachable!()
    };

    lsp::CodeAction {
      title: format!("Replace `{path}` with `{defined_as}`"),
      kind: Some(lsp::CodeActionKind::QUICKFIX),
      diagnostics: Some(vec![error.diagnostic(file)]),
      edit: Some(replace_edit(file, *span, defined_as)),
      ..Default::default()
    }
  }
}

mod refactors {
  use super::{Document, Span, multiple_edits};
  use crate::requests::variables::find_variable;

  use bang_syntax::ast::{Expression, Statement, statement::Let};
  use bang_typechecker::VariableKind;
  use lsp_types as lsp;

  pub fn fixes(file: &Document, range: Span, actions: &mut Vec<lsp::CodeAction>) {
    actions.extend(inline_variable(file, range));
  }

  /// Replaces the use of a variable with its value,
  /// removing the declaration if it was the last use
  fn inline_variable(file: &Document, range: Span) -> Option<lsp::CodeAction> {
    let variable = find_variable(range, file.typechecker())?;
    let VariableKind::Declaration {
      name,
      defined,
      parameter: false,
      ..
    } = &variable.kind
    else {
      return None;
    };

    // only the use which the cursor is on is inlined
    let usage =
      (variable.used.iter()).find(|used| used.contains(range) || range.contains(**used))?;

    let declaration = find_declaration(file, *defined)?;
    let declaration_span = declaration_span(file, declaration);

    // a recursive function uses itself in its own value, which can't be replaced
    if declaration_span.contains(*usage) {
      return None;
    }

    let value = inlined_value(file, declaration.value(&file.ast))?;

    let mut edits = vec![(*usage, value)];
    if variable.used.len() == 1 {
      // it was the only use, so the declaration is no longer needed
      edits.push((declaration_span, String::new()));
    }

    Some(lsp::CodeAction {
      title: format!("Inline variable `{name}`"),
      kind: Some(lsp::CodeActionKind::REFACTOR_INLINE),
      edit: Some(multiple_edits(file, edits)),
      ..Default::default()
    })
  }

  /// The `let` statement which declares the variable defined at the given span
  fn find_declaration(file: &Document, defined: Span) -> Option<&Let> {
    (file.ast.all_statements()).find_map(|statement| match statement {
      Statement::Let(let_) if let_.identifier_span(&file.ast) == defined => Some(let_),
      _ => None,
    })
  }

  /// The span to remove for the declaration, including any doc comment,
  /// the indentation before it, and any trailing comment and the line ending
  fn declaration_span(file: &Document, declaration: &Let) -> Span {
    let source = &file.ast.source;
    let span = match declaration.doc_comment(&file.ast) {
      Some(doc_comment) => doc_comment
        .span(&file.ast)
        .merge(declaration.span(&file.ast)),
      None => declaration.span(&file.ast),
    };

    let start = source[..span.start as usize].trim_end_matches([' ', '\t']);

    // the rest of the line can only be whitespace or a comment, but check to be safe
    let rest_of_line = source[span.end as usize..].split('\n').next().unwrap_or("");
    let end = if rest_of_line.trim().is_empty() || rest_of_line.trim_start().starts_with("//") {
      span.end as usize + rest_of_line.len() + 1
    } else {
      span.end as usize
    };

    #[expect(clippy::cast_possible_truncation, reason = "source.len() < u32::MAX")]
    Span::new(start.len() as u32, end.min(source.len()) as u32)
  }

  /// The text of the value to be inlined, wrapped in parentheses if it could be reparsed
  ///
  /// Returns `None` if the value can't be safely inlined
  fn inlined_value(file: &Document, value: &Expression) -> Option<String> {
    let ast = &file.ast;

    // a comment after the value is left behind with the declaration
    let value = match value {
      Expression::Comment(comment) => comment.expression(ast),
      value => value,
    };

    let text = value.span(ast).source_text(&ast.source);
    if text.is_empty() || text.contains('\n') {
      // multiline values would be inlined into the middle of another expression
      return None;
    }

    match value {
      Expression::Invalid(_) => None,

      // could bind differently to the surrounding expression, so are grouped
      Expression::Assignment(_)
      | Expression::Binary(_)
      | Expression::Function(_)
      | Expression::If(_)
      | Expression::Match(_)
      | Expression::Unary(_) => Some(format!("({text})")),

      _ => Some(text.to_owned()),
    }
  }
}
