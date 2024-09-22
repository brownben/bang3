use super::documents::{Document, DocumentIndex};
use super::locations::{lsp_range_from_span, span_from_lsp_position};
use lsp_types as lsp;
use std::{collections::HashMap, iter};

use bang_formatter::{format, FormatterConfig};
use bang_linter::{lint, LintDiagnostic};
use bang_parser::{parse, Allocator, GetSpan, ParseError};
use bang_typechecker::{get_enviroment, typecheck, TypeError};

pub fn handle(
  request: lsp_server::Request,
  files: &mut DocumentIndex,
) -> Option<lsp_server::Response> {
  use lsp::request::*;

  match request.method.as_str() {
    DocumentDiagnosticRequest::METHOD => {
      let (request_id, params) = get_params::<DocumentDiagnosticRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = file_diagnostics(file);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    Formatting::METHOD => {
      let (request_id, params) = get_params::<Formatting>(request);
      let file = files.get(&params.text_document.uri);
      let result = format_file(file);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    GotoDeclaration::METHOD => {
      let (request_id, params) = get_params::<GotoDeclaration>(request);
      let file = files.get(&params.text_document_position_params.text_document.uri);
      let result = goto_definition(file, params.text_document_position_params.position);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    GotoDefinition::METHOD => {
      let (request_id, params) = get_params::<GotoDefinition>(request);
      let file = files.get(&params.text_document_position_params.text_document.uri);
      let result = goto_definition(file, params.text_document_position_params.position);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    References::METHOD => {
      let (request_id, params) = get_params::<References>(request);
      let file = files.get(&params.text_document_position.text_document.uri);
      let result = get_references(file, params.text_document_position.position);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    Rename::METHOD => {
      let (request_id, params) = get_params::<Rename>(request);
      let file = files.get(&params.text_document_position.text_document.uri);
      let position = params.text_document_position.position;
      let result = rename(file, position, &params.new_name);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    request => {
      eprintln!("Unknown Request: {request:?}");

      None
    }
  }
}

fn file_diagnostics(file: &Document) -> lsp::DocumentDiagnosticReport {
  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  let mut diagnostics = Vec::new();

  if ast.errors.is_empty() {
    let lints = lint(&ast);
    diagnostics.extend(lints.iter().map(|lint| diagnostic_from_lint(lint, file)));

    let type_problems = typecheck(&ast);
    diagnostics.extend(
      type_problems
        .iter()
        .map(|error| diagnostic_from_type_error(error, file)),
    );
  } else {
    diagnostics.extend(
      ast
        .errors
        .iter()
        .map(|error| diagnostic_from_parse_error(error, file)),
    );
  }

  lsp::DocumentDiagnosticReport::Full(lsp::RelatedFullDocumentDiagnosticReport {
    related_documents: None,
    full_document_diagnostic_report: lsp::FullDocumentDiagnosticReport {
      result_id: None,
      items: diagnostics,
    },
  })
}

fn format_file(file: &Document) -> Option<Vec<lsp::TextEdit>> {
  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  if !ast.is_valid() {
    // don't format if the file is not valid
    return None;
  }

  let config = FormatterConfig::default(); // TODO: request parameter options
  let new_text = format(&ast, config);

  Some(vec![lsp::TextEdit {
    range: lsp_range_from_span(file.line_index.get_file_span(), file),
    new_text,
  }])
}

fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let declaration = variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(declaration.span(), file),
  ))
}

fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let declaration = variables
    .defined_variables()
    .find(|variable| variable.span().contains(position))?;

  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location::new(file.id.clone(), lsp_range_from_span(*span, file)))
    .collect();

  Some(references)
}

fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let Some(declaration) = variables
    .defined_variables()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))
  else {
    return lsp::WorkspaceEdit::default();
  };

  let text_edits = iter::once(&declaration.span())
    .chain(declaration.used.iter())
    .map(|span| lsp::TextEdit {
      range: lsp_range_from_span(*span, file),
      new_text: new_name.to_owned(),
    })
    .collect();

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}

fn diagnostic_from_parse_error(error: &ParseError, file: &Document) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::ERROR),
    source: Some("Syntax Error".into()),
    message: error.full_message(),
    range: lsp_range_from_span(error.span(), file),

    ..Default::default()
  }
}

fn diagnostic_from_lint(lint: &LintDiagnostic, file: &Document) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::WARNING),
    source: Some("Lint".into()),
    message: lint.full_message(),
    range: lsp_range_from_span(lint.span(), file),

    tags: {
      if lint.title == "No Unused Variables" {
        Some(vec![lsp::DiagnosticTag::UNNECESSARY])
      } else {
        None
      }
    },
    ..Default::default()
  }
}

fn diagnostic_from_type_error(type_error: &TypeError, file: &Document) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: if type_error.is_warning() {
      Some(lsp::DiagnosticSeverity::WARNING)
    } else {
      Some(lsp::DiagnosticSeverity::ERROR)
    },
    source: Some("Type Error".into()),
    message: type_error.full_message(),
    range: lsp_range_from_span(type_error.span(), file),
    tags: {
      if type_error.is_warning() {
        Some(vec![lsp::DiagnosticTag::UNNECESSARY])
      } else {
        None
      }
    },
    ..Default::default()
  }
}

fn get_params<R: lsp::request::Request>(
  request: lsp_server::Request,
) -> (lsp_server::RequestId, R::Params) {
  request.extract(R::METHOD).unwrap()
}
