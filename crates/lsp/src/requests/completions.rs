use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::span_from_lsp_position;
use lsp_types as lsp;

use bang_interpreter::stdlib::{MATHS_ITEMS, STRING_ITEMS};
use bang_parser::{ast::Statement, parse, Allocator, GetSpan, Span, AST};
use bang_typechecker::{get_enviroment, import_type_info, VariableKind};

pub fn completions(file: &Document, position: lsp::Position) -> lsp::CompletionList {
  let position = span_from_lsp_position(position, file);

  let allocator = Allocator::new();
  let ast = parse(&file.source, &allocator);

  if in_import_module(&ast, position) {
    return module_completions();
  };

  if let Some(module_name) = in_import_items(&ast, position) {
    return module_item_completions(module_name);
  };

  variable_completions(&ast, position)
}

fn constant_snippets() -> [lsp::CompletionItem; 11] {
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
      insert_text: Some("if ($1) $2 else $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "match".to_owned(),
      insert_text: Some("match $1 | $2 -> $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "let".to_owned(),
      insert_text: Some("let $1 = $0".to_owned()),
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
    lsp::CompletionItem {
      label: "from".to_owned(),
      insert_text: Some("from $1 import { $0 }".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "import".to_owned(),
      insert_text: Some("from $1 import { $0 }".to_owned()),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "as".to_owned(),
      insert_text: Some("as".to_owned()),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
  ]
}

fn variable_completions(ast: &AST, position: Span) -> lsp::CompletionList {
  let variables = get_enviroment(ast);

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

fn module_completions() -> lsp::CompletionList {
  let items = ["maths", "string"]
    .into_iter()
    .map(|module| lsp::CompletionItem {
      kind: Some(lsp::CompletionItemKind::MODULE),
      label: module.to_owned(),
      ..Default::default()
    })
    .collect();

  lsp::CompletionList {
    is_incomplete: false,
    items,
  }
}

fn module_item_completions(module: &str) -> lsp::CompletionList {
  let item_names = match module {
    "maths" => MATHS_ITEMS.iter(),
    "string" => STRING_ITEMS.iter(),
    _ => [].iter(),
  };

  let items = item_names
    .map(|item| (*item, import_type_info(module, item)))
    .map(|(name, type_info)| lsp::CompletionItem {
      kind: Some(if type_info.kind == VariableKind::Function {
        lsp::CompletionItemKind::FUNCTION
      } else if is_screaming_snake_case(name) {
        lsp::CompletionItemKind::VARIABLE
      } else {
        lsp::CompletionItemKind::CONSTANT
      }),
      label: name.to_string(),
      label_details: Some(lsp::CompletionItemLabelDetails {
        detail: None,
        description: Some(type_info.string.clone()),
      }),
      ..Default::default()
    })
    .collect();

  lsp::CompletionList {
    is_incomplete: false,
    items,
  }
}

fn in_import_module(ast: &AST, position: Span) -> bool {
  for statement in &ast.statements {
    if let Statement::Import(import) = statement
      && import.span().contains(position)
    {
      if let Some(item) = import.items.first() {
        return position.start < item.span().start;
      }

      return true;
    }
  }

  false
}

fn in_import_items<'a>(ast: &'a AST, position: Span) -> Option<&'a str> {
  for statement in &ast.statements {
    if let Statement::Import(import) = statement
      && import.items_span.contains(position)
    {
      return Some(import.module.name);
    }
  }

  None
}
