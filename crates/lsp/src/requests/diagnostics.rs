use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_linter::{lint, LintDiagnostic};
use bang_parser::{parse, Allocator, GetSpan, ParseError};
use bang_typechecker::{typecheck, TypeError};

pub fn file_diagnostics(file: &Document) -> lsp::DocumentDiagnosticReport {
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
      if lint.is_unused() {
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
