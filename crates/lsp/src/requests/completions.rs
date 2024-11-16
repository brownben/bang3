use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::span_from_lsp_position;
use lsp_types as lsp;

use bang_interpreter::stdlib::{MATHS_ITEMS, MODULES, STRING_ITEMS};
use bang_syntax::{
  ast::{Expression, Statement},
  parse, Span, AST,
};
use bang_typechecker::{get_enviroment, import_type_info, VariableKind};

pub fn completions(file: &Document, position: lsp::Position) -> lsp::CompletionList {
  let position = span_from_lsp_position(position, file);

  let ast = parse(&file.source);

  if let Some(import_statement) = in_import_statement(&ast, position) {
    let first_item_start = import_statement.items(&ast).next().map(|i| i.span.start);
    let before_items = first_item_start.is_none_or(|start| position.end < start);
    if before_items {
      return module_completions();
    }

    let in_items = import_statement.items_span(&ast).contains(position);
    if in_items {
      return module_item_completions(import_statement.module(&ast), true);
    }
  }

  if let Some(module_access) = in_module_access(&ast, position) {
    return module_item_completions(module_access.module(&ast), false);
  }

  if in_type_annotation(&ast, position) {
    return lsp::CompletionList {
      is_incomplete: false,
      items: type_annotation_snippets().into(),
    };
  }

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
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      insert_text: Some("if ($1) $2 else $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "match".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      insert_text: Some("match $1 | $2 -> $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "let".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      insert_text: Some("let $1 = $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "return".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      insert_text: Some("return $0".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "from ".to_owned(),
      label_details: Some(lsp::CompletionItemLabelDetails {
        detail: Some("_ import { … }".to_owned()),
        description: None,
      }),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      insert_text: Some("from $1 import { $0 }".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "import".to_owned(),
      kind: Some(lsp::CompletionItemKind::SNIPPET),
      insert_text: Some("from $1 import { $0 }".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "as".to_owned(),
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
        label: format!("{name}(…)"),
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
    .chain(module_access_snippets())
    .collect();

  lsp::CompletionList {
    items,
    is_incomplete: false,
  }
}

fn module_completions() -> lsp::CompletionList {
  let items = MODULES
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

fn module_access_snippets() -> impl Iterator<Item = lsp::CompletionItem> {
  MODULES.iter().map(|module| lsp::CompletionItem {
    label: (*module).to_owned(),
    label_details: Some(lsp::CompletionItemLabelDetails {
      detail: Some("::".to_owned()),
      description: None,
    }),
    insert_text: Some(format!("{module}::$0")),
    insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
    kind: Some(lsp::CompletionItemKind::MODULE),

    // Trigger a suggestions for the items in the module
    // TODO: make this generic and not reliant on VSCode
    command: Some(lsp::Command {
      title: "Trigger Suggestions".to_owned(),
      command: "editor.action.triggerSuggest".to_owned(),
      arguments: None,
    }),

    ..Default::default()
  })
}

fn module_item_completions(module: &str, in_import: bool) -> lsp::CompletionList {
  let item_names = match module {
    "maths" => MATHS_ITEMS.iter(),
    "string" => STRING_ITEMS.iter(),
    _ => [].iter(),
  };

  let items = item_names
    .map(|item| (*item, import_type_info(module, item)))
    .map(|(name, type_info)| match type_info.kind {
      VariableKind::Function if !in_import => lsp::CompletionItem {
        kind: Some(lsp::CompletionItemKind::FUNCTION),
        insert_text: Some(format!("{name}($0)")),
        insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
        label: format!("{name}(…)"),
        label_details: Some(lsp::CompletionItemLabelDetails {
          detail: None,
          description: Some(type_info.string.clone()),
        }),
        ..Default::default()
      },
      _ => lsp::CompletionItem {
        kind: match type_info.kind {
          VariableKind::Function => Some(lsp::CompletionItemKind::FUNCTION),
          VariableKind::Variable => Some(lsp::CompletionItemKind::CONSTANT),
        },
        label: name.to_owned(),
        label_details: Some(lsp::CompletionItemLabelDetails {
          detail: None,
          description: Some(type_info.string.clone()),
        }),
        ..Default::default()
      },
    })
    .collect();

  lsp::CompletionList {
    is_incomplete: false,
    items,
  }
}

fn type_annotation_snippets() -> [lsp::CompletionItem; 4] {
  [
    lsp::CompletionItem {
      label: "string".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "number".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "boolean".to_owned(),
      kind: Some(lsp::CompletionItemKind::KEYWORD),
      ..Default::default()
    },
    lsp::CompletionItem {
      label: "function".to_owned(),
      kind: Some(lsp::CompletionItemKind::SNIPPET),
      insert_text: Some("$0 => $1".to_owned()),
      insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
      ..Default::default()
    },
  ]
}

fn in_type_annotation(ast: &AST, position: Span) -> bool {
  for ty in &ast.types {
    if ty.span(ast).contains(position) {
      return true;
    }
  }

  false
}

fn in_import_statement(ast: &AST, position: Span) -> Option<&bang_syntax::ast::statement::Import> {
  for statement in ast.all_statements() {
    if let Statement::Import(import) = statement
      && import.span(ast).contains(position)
    {
      return Some(import);
    }
  }

  None
}

fn in_module_access(
  ast: &AST,
  position: Span,
) -> Option<&bang_syntax::ast::expression::ModuleAccess> {
  for expression in &ast.expressions {
    if let Expression::ModuleAccess(module_access) = expression {
      if module_access.span(ast).contains(position) {
        return Some(module_access);
      }
    }
  }

  None
}
