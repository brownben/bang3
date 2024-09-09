use super::documents::{Document, DocumentIndex};
use crate::{GetSpan, LineIndex, Span};
use lsp_types as lsp;

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
    request => {
      eprintln!("Unknown Request:\n\t{request:?}");

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
    range: range_from_span(new_line_index.get_file_span(), &new_line_index),
    new_text,
  }])
}

fn range_from_span(span: Span, line_index: &LineIndex) -> lsp::Range {
  let (start_line, start_char) = line_index.get_offset(span);
  let (end_line, end_char) = line_index.get_final_offset(span);

  let start = lsp::Position::new(u32::try_from(start_line).unwrap() - 1, start_char);
  let end = lsp::Position::new(u32::try_from(end_line).unwrap() - 1, end_char);

  lsp::Range::new(start, end)
}

fn diagnostic_from_parse_error(error: crate::ParseError, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::ERROR),
    source: Some("Syntax Error".into()),
    message: error.full_message(),
    range: range_from_span(error.span(), lines),
    ..Default::default()
  }
}

fn diagnostic_from_lint(lint: crate::LintDiagnostic, lines: &LineIndex) -> lsp::Diagnostic {
  lsp::Diagnostic {
    severity: Some(lsp::DiagnosticSeverity::WARNING),
    source: Some("Lint".into()),
    message: lint.full_message(),
    range: range_from_span(lint.span(), lines),
    ..Default::default()
  }
}

fn get_params<R: lsp_types::request::Request>(
  request: lsp_server::Request,
) -> (lsp_server::RequestId, R::Params) {
  request.extract(R::METHOD).unwrap()
}
