//! # Language Server Protocol
//! Implements the Language Server Protocol for the Bang language.
//!
//! Supports the following features:
//! - Formatting
//! - Diagnostics
//! - Folding Ranges
//! - Go to Definition
//! - Get References
//! - Rename Variable
//! - Document Symbols
//! - Inlay Hints (for variable declaration types)
//! - Type and Documentation on Variable Hover
//! - Completion Suggestions
//!   - Basic Snippets
//!   - Variables in Scope
//!   - Module Items in Import
//! - Selection Ranges

#![deny(unsafe_code)]

mod documents;
mod locations;
mod notifications;
mod requests;

use documents::DocumentIndex;

/// An instance of a Language Server
pub struct LanguageServer {
  connection: lsp_server::Connection,
  connection_threads: lsp_server::IoThreads,
  files: DocumentIndex,
}
impl LanguageServer {
  /// Create a new language server communicating over stdio
  ///
  /// # Panics
  /// Panics if the protocol has changed and capabilities can't be exchanged.
  #[must_use]
  pub fn new() -> Self {
    let (connection, connection_threads) = lsp_server::Connection::stdio();

    let server_capabilities = serde_json::to_value(Self::capabilities()).unwrap();
    connection.initialize(server_capabilities).unwrap();

    Self {
      connection,
      connection_threads,

      files: DocumentIndex::default(),
    }
  }

  /// Run the main loop of the language server responding to requests
  ///
  /// # Panics
  /// If problem clearing up threads.
  pub fn run(mut self) {
    eprintln!("Bang Language Server Running");

    self.main_loop();
    self.connection_threads.join().unwrap();

    eprintln!("Stopping Bang Language Server");
  }

  fn main_loop(&mut self) {
    use lsp_server::Message;

    for msg in &self.connection.receiver {
      match msg {
        Message::Request(request) => {
          if self.connection.handle_shutdown(&request).unwrap() {
            return;
          }

          if let Some(message) = requests::handle(request, &mut self.files) {
            self.connection.sender.send(message.into()).unwrap();
          }
        }
        Message::Notification(notification) => {
          notifications::handle(notification, &mut self.files);
        }
        Message::Response(response) => {
          eprintln!("Unknown Response: {response:?}");
        }
      }
    }
  }

  fn capabilities() -> lsp_types::ServerCapabilities {
    lsp_types::ServerCapabilities {
      // Be sent files on open or change
      text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
        lsp_types::TextDocumentSyncOptions {
          open_close: Some(true),
          change: Some(lsp_types::TextDocumentSyncKind::FULL),
          ..Default::default()
        },
      )),

      // Provides diagnostics
      diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
        lsp_types::DiagnosticOptions {
          identifier: Some("bang".into()),
          ..Default::default()
        },
      )),

      // Provides formatter
      document_formatting_provider: Some(lsp_types::OneOf::Left(true)),

      // Go to definition
      declaration_provider: Some(lsp_types::DeclarationCapability::Simple(true)),
      definition_provider: Some(lsp_types::OneOf::Left(true)),
      references_provider: Some(lsp_types::OneOf::Left(true)),

      // Rename
      rename_provider: Some(lsp_types::OneOf::Left(true)),

      // Autocompletion
      completion_provider: Some(lsp_types::CompletionOptions::default()),

      // Hover
      hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),

      // Document Symbols
      document_symbol_provider: Some(lsp_types::OneOf::Left(true)),

      // Folding Ranges
      folding_range_provider: Some(lsp_types::FoldingRangeProviderCapability::Simple(true)),

      // Inlay Hints
      inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),

      // Selection Ranges
      selection_range_provider: Some(lsp_types::SelectionRangeProviderCapability::Simple(true)),

      // Code Actions (autofixes)
      code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),

      // Doesn't provide all capabilities
      ..Default::default()
    }
  }
}
impl Default for LanguageServer {
  fn default() -> Self {
    Self::new()
  }
}
