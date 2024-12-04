use crate::documents::Document;
use crate::locations::lsp_range_from_span;
use lsp_types as lsp;

use bang_linter::{LintDiagnostic, lint};
use bang_syntax::ParseError;
use bang_typechecker::TypeError;

pub fn file_diagnostics(file: &Document) -> lsp::DocumentDiagnosticReport {
  let mut diagnostics = Vec::new();

  let parse_errors = file.ast.errors.iter();
  diagnostics.extend(parse_errors.map(|error| error.diagnostic(file)));

  let lints = lint(&file.ast);
  diagnostics.extend(lints.iter().map(|lint| lint.diagnostic(file)));

  if file.ast.is_valid() {
    diagnostics.extend((file.typechecker().problems().iter()).map(|error| error.diagnostic(file)));
  }

  lsp::DocumentDiagnosticReport::Full(lsp::RelatedFullDocumentDiagnosticReport {
    related_documents: None,
    full_document_diagnostic_report: lsp::FullDocumentDiagnosticReport {
      result_id: None,
      items: diagnostics,
    },
  })
}

pub trait IntoDiagnostic {
  fn diagnostic(&self, file: &Document) -> lsp::Diagnostic;
}

impl IntoDiagnostic for ParseError {
  fn diagnostic(&self, file: &Document) -> lsp::Diagnostic {
    lsp::Diagnostic {
      severity: Some(lsp::DiagnosticSeverity::ERROR),
      source: Some("Syntax Error".into()),
      message: self.full_message(),
      range: lsp_range_from_span(self.span(), file),

      ..Default::default()
    }
  }
}
impl IntoDiagnostic for LintDiagnostic {
  fn diagnostic(&self, file: &Document) -> lsp::Diagnostic {
    lsp::Diagnostic {
      severity: Some(lsp::DiagnosticSeverity::WARNING),
      source: Some("Lint".into()),
      message: self.full_message(),
      range: lsp_range_from_span(self.span(), file),
      tags: {
        if self.is_unused() {
          Some(vec![lsp::DiagnosticTag::UNNECESSARY])
        } else {
          None
        }
      },
      ..Default::default()
    }
  }
}
impl IntoDiagnostic for TypeError {
  fn diagnostic(&self, file: &Document) -> lsp::Diagnostic {
    lsp::Diagnostic {
      severity: if self.is_warning() {
        Some(lsp::DiagnosticSeverity::WARNING)
      } else {
        Some(lsp::DiagnosticSeverity::ERROR)
      },
      source: Some("Type Error".into()),
      message: self.full_message(),
      range: lsp_range_from_span(self.span(), file),
      tags: {
        if self.is_unused() {
          Some(vec![lsp::DiagnosticTag::UNNECESSARY])
        } else {
          None
        }
      },
      related_information: self.related_info().map(|(span, message)| {
        vec![lsp::DiagnosticRelatedInformation {
          location: lsp::Location {
            uri: file.id.clone(),
            range: lsp_range_from_span(span, file),
          },
          message,
        }]
      }),
      ..Default::default()
    }
  }
}
