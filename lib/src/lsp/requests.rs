use super::documents::{Document, DocumentIndex};
use crate::{GetSpan, LineIndex};
use lsp_types as lsp;
use std::{collections::HashMap, iter};

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
  let allocator = crate::Allocator::new();
  let ast = crate::parse(&file.source, &allocator);

  let lints = if ast.errors.is_empty() {
    crate::lint(&ast)
      .iter()
      .map(|lint| diagnostic_from_lint(*lint, &file.line_index))
      .collect()
  } else {
    ast
      .errors
      .iter()
      .map(|error| diagnostic_from_parse_error(*error, &file.line_index))
      .collect()
  };

  lsp::DocumentDiagnosticReport::Full(lsp::RelatedFullDocumentDiagnosticReport {
    related_documents: None,
    full_document_diagnostic_report: lsp::FullDocumentDiagnosticReport {
      result_id: None,
      items: lints,
    },
  })
}

fn format_file(file: &Document) -> Option<Vec<lsp::TextEdit>> {
  let allocator = crate::Allocator::new();
  let ast = crate::parse(&file.source, &allocator);

  if !ast.is_valid() {
    // don't format if the file is not valid
    return None;
  }

  let config = crate::FormatterConfig::default(); // TODO: request parameter options
  let new_text = crate::format(&ast, config);
  let new_line_index = LineIndex::from_source(&new_text);

  Some(vec![lsp::TextEdit {
    range: new_line_index.lsp_range_from_span(new_line_index.get_file_span()),
    new_text,
  }])
}

fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = file.line_index.span_from_lsp_position(position);

  let allocator = crate::Allocator::new();
  let ast = crate::parse(&file.source, &allocator);
  let variables = crate::linter::Variables::from(&ast);

  let declaration = variables
    .defined()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))?;

  Some(lsp::Location::new(
    file.id.clone(),
    file.line_index.lsp_range_from_span(declaration.span()),
  ))
}

fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = file.line_index.span_from_lsp_position(position);

  let allocator = crate::Allocator::new();
  let ast = crate::parse(&file.source, &allocator);
  let variables = crate::linter::Variables::from(&ast);

  let declaration = variables
    .defined()
    .find(|variable| variable.span().contains(position))?;

  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location {
      uri: file.id.clone(),
      range: file.line_index.lsp_range_from_span(*span),
    })
    .collect();

  Some(references)
}

fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = file.line_index.span_from_lsp_position(position);

  let allocator = crate::Allocator::new();
  let ast = crate::parse(&file.source, &allocator);
  let variables = crate::linter::Variables::from(&ast);

  let Some(declaration) = variables
    .defined()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))
  else {
    return lsp::WorkspaceEdit::default();
  };

  let text_edits = iter::once(&declaration.span())
    .chain(declaration.used.iter())
    .map(|span| lsp::TextEdit {
      range: file.line_index.lsp_range_from_span(*span),
      new_text: new_name.to_owned(),
    })
    .collect();

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}

fn diagnostic_from_parse_error(error: crate::ParseError, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::ERROR),
    source: Some("Syntax Error".into()),
    message: error.full_message(),
    range: lines.lsp_range_from_span(error.span()),
    ..Default::default()
  }
}

fn diagnostic_from_lint(lint: crate::LintDiagnostic, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::WARNING),
    source: Some("Lint".into()),
    message: lint.full_message(),
    range: lines.lsp_range_from_span(lint.span()),
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

fn get_params<R: lsp::request::Request>(
  request: lsp_server::Request,
) -> (lsp_server::RequestId, R::Params) {
  request.extract(R::METHOD).unwrap()
}
