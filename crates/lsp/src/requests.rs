use super::documents::{Document, DocumentIndex};
use lsp_types as lsp;
use std::{collections::HashMap, iter};

use bang_formatter::{format, FormatterConfig};
use bang_linter::{lint, LintDiagnostic};
use bang_parser::{parse, Allocator, GetSpan, LineIndex, ParseError, Span};

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

  let lints = if ast.errors.is_empty() {
    lint(&ast)
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
  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  if !ast.is_valid() {
    // don't format if the file is not valid
    return None;
  }

  let config = FormatterConfig::default(); // TODO: request parameter options
  let new_text = format(&ast, config);
  let new_line_index = LineIndex::from_source(&new_text);

  Some(vec![lsp::TextEdit {
    range: lsp_range_from_span(&new_line_index, new_line_index.get_file_span()),
    new_text,
  }])
}

fn goto_definition(file: &Document, position: lsp::Position) -> Option<lsp::Location> {
  let position = span_from_lsp_position(&file.line_index, position);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = bang_linter::Variables::from(&ast);

  let declaration = variables
    .defined()
    .filter(|variable| variable.is_active(position))
    .find(|variable| variable.used.iter().any(|used| used.contains(position)))?;

  Some(lsp::Location::new(
    file.id.clone(),
    lsp_range_from_span(&file.line_index, declaration.span()),
  ))
}

fn get_references(file: &Document, position: lsp::Position) -> Option<Vec<lsp::Location>> {
  let position = span_from_lsp_position(&file.line_index, position);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = bang_linter::Variables::from(&ast);

  let declaration = variables
    .defined()
    .find(|variable| variable.span().contains(position))?;

  let references = declaration
    .used
    .iter()
    .map(|span| lsp::Location {
      uri: file.id.clone(),
      range: lsp_range_from_span(&file.line_index, *span),
    })
    .collect();

  Some(references)
}

fn rename(file: &Document, position: lsp::Position, new_name: &str) -> lsp::WorkspaceEdit {
  let position = span_from_lsp_position(&file.line_index, position);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = bang_linter::Variables::from(&ast);

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
      range: lsp_range_from_span(&file.line_index, *span),
      new_text: new_name.to_owned(),
    })
    .collect();

  lsp::WorkspaceEdit::new(HashMap::from([(file.id.clone(), text_edits)]))
}

fn diagnostic_from_parse_error(error: ParseError, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::ERROR),
    source: Some("Syntax Error".into()),
    message: error.full_message(),
    range: lsp_range_from_span(lines, error.span()),
    ..Default::default()
  }
}

fn diagnostic_from_lint(lint: LintDiagnostic, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::WARNING),
    source: Some("Lint".into()),
    message: lint.full_message(),
    range: lsp_range_from_span(lines, lint.span()),
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

fn span_from_lsp_position(line_index: &LineIndex, position: lsp_types::Position) -> Span {
  let start_line = position.line.try_into().unwrap();
  let start = line_index.to_offset(start_line, position.character);

  Span {
    start,
    end: start + 1,
  }
}

fn lsp_range_from_span(line_index: &LineIndex, span: Span) -> lsp_types::Range {
  let start_line = line_index.get_line(span) - 1;
  let start_char = span.start - line_index.get_line_start(start_line);

  let end_line = line_index.get_final_line(span) - 1;
  let end_char = span.end - line_index.get_line_start(end_line);

  let start = lsp_types::Position::new(start_line.try_into().unwrap(), start_char);
  let end = lsp_types::Position::new(end_line.try_into().unwrap(), end_char);

  lsp_types::Range::new(start, end)
}
