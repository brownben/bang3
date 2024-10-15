use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::span_from_lsp_position;
use lsp_types as lsp;

use bang_parser::{parse, Allocator};
use bang_typechecker::{get_enviroment, VariableKind};

pub fn completions(file: &Document, position: lsp::Position) -> lsp::CompletionList {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);
  let variables = get_enviroment(&ast);

  let items = variables
    .defined_variables()
    .filter(|var| var.is_active(position))
    .map(|var| (var.name.clone(), var.type_info.as_ref().unwrap()))
    .chain(
      variables
        .builtin_variables()
        .map(|var| (var.name.to_owned(), &var.type_info)),
    )
    .map(|(name, type_info)| match type_info.kind {
      VariableKind::Function => lsp::CompletionItem {
        kind: Some(lsp::CompletionItemKind::FUNCTION),
        insert_text: Some(format!("{name}($0)")),
        insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
        label: name,
        label_details: Some(lsp::CompletionItemLabelDetails {
          detail: None,
          description: Some(type_info.string.clone()),
        }),
        ..Default::default()
      },
      VariableKind::Variable => lsp::CompletionItem {
        kind: Some(if is_screaming_snake_case(&name) {
          lsp::CompletionItemKind::VARIABLE
        } else {
          lsp::CompletionItemKind::CONSTANT
        }),
        label: name,
        label_details: Some(lsp::CompletionItemLabelDetails {
          detail: None,
          description: Some(type_info.string.clone()),
        }),
        ..Default::default()
      },
    })
    .chain(constant_snippets())
    .collect();

  lsp::CompletionList {
    is_incomplete: false,
    items,
  }
}

fn constant_snippets() -> [lsp::CompletionItem; 8] {
  [
    lsp::CompletionItem {
      label: "true".to_owned(),
      kind: Some(lsp::CompletionItemKind::VALUE),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "false".to_owned(),
      kind: Some(lsp::CompletionItemKind::VALUE),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "and".to_owned(),
      kind: Some(lsp::CompletionItemKind::OPERATOR),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "or".to_owned(),
      kind: Some(lsp::CompletionItemKind::OPERATOR),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "if".to_owned(),
      insert_text: Some("if ($1) $2 else $3".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "match".to_owned(),
      insert_text: Some("match $1 | $2 -> $3".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "let".to_owned(),
      insert_text: Some("let $1 = $2".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "return".to_owned(),
      insert_text: Some("return $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
  ]
}
