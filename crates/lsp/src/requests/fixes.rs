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

fn delete_edit(file: &Document, span: Span) -> lsp::WorkspaceEdit {
  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), vec![lsp::TextEdit {
    range: lsp_range_from_span(span, file),
    new_text: String::new(),
  }])]))
}

mod parse_errors {
  use crate::requests::diagnostics::IntoDiagnostic;

  use super::{Document, Span, delete_edit, replace_edit};
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
          ParseError::NoSingleEqualOperator { token, .. } => {
            Some(no_single_equal_operator(file, error, token.into()))
          }
          _ => None,
        }),
    );
  }

  fn no_single_equal_operator(file: &Document, error: &ParseError, span: Span) -> lsp::CodeAction {
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
}

mod type_errors {
  use super::{Document, IntoDiagnostic, Span, delete_edit, insert_edit, replace_edit};
  use bang_stdlib::{MODULES, StdlibModule};
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

    for module in is_from_module(identifier) {
      actions.push(lsp::CodeAction {
        title: format!("Qualify as `{module}::{identifier}`"),
        kind: Some(lsp::CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![error.diagnostic(file)]),
        edit: Some(insert_edit(file, span.start, &format!("{module}::"))),
        ..Default::default()
      });
    }
  }

  fn is_from_module(identifier: &str) -> impl Iterator<Item = &'static str> {
    MODULES
      .into_iter()
      .filter(move |module| module.items().contains(&identifier))
      .map(StdlibModule::name)
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
