#![allow(clippy::module_name_repetitions)]

use super::documents::DocumentIndex;
use lsp_types as lsp;

mod completions;
mod diagnostics;
mod fixes;
mod folding;
mod format;
mod hover;
mod inlay_hints;
mod selection_range;
mod symbols;
mod variables;

use completions::completions;
use diagnostics::file_diagnostics;
use fixes::fixes;
use folding::folding_ranges;
use format::format_file;
use hover::hover;
use inlay_hints::inlay_hints;
use selection_range::selection_ranges;
use symbols::document_symbols;
use variables::{get_references, goto_definition, rename};

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
    Completion::METHOD => {
      let (request_id, params) = get_params::<Completion>(request);
      let file = files.get(&params.text_document_position.text_document.uri);
      let position = params.text_document_position.position;
      let result = completions(file, position);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    HoverRequest::METHOD => {
      let (request_id, params) = get_params::<HoverRequest>(request);
      let file = files.get(&params.text_document_position_params.text_document.uri);
      let position = params.text_document_position_params.position;
      let result = hover(file, position);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    DocumentSymbolRequest::METHOD => {
      let (request_id, params) = get_params::<DocumentSymbolRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = document_symbols(file);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    FoldingRangeRequest::METHOD => {
      let (request_id, params) = get_params::<FoldingRangeRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = folding_ranges(file);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    InlayHintRequest::METHOD => {
      let (request_id, params) = get_params::<InlayHintRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = inlay_hints(file, params.range);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    SelectionRangeRequest::METHOD => {
      let (request_id, params) = get_params::<SelectionRangeRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = selection_ranges(file, &params.positions);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    CodeActionRequest::METHOD => {
      let (request_id, params) = get_params::<CodeActionRequest>(request);
      let file = files.get(&params.text_document.uri);
      let result = fixes(file, params.range);

      Some(lsp_server::Response::new_ok(request_id, result))
    }
    request => {
      eprintln!("Unknown Request: {request:?}");

      None
    }
  }
}

fn get_params<R: lsp::request::Request>(
  request: lsp_server::Request,
) -> (lsp_server::RequestId, R::Params) {
  request.extract(R::METHOD).unwrap()
}

fn is_screaming_snake_case(name: &str) -> bool {
  name.chars().all(|c| c.is_ascii_uppercase() || c == '_')
}
