use super::is_screaming_snake_case;
use crate::documents::Document;
use crate::locations::span_from_lsp_position;
use lsp_types as lsp;

use bang_interpreter::stdlib::MODULES;
use bang_syntax::{
  AST, Span,
  ast::{Expression, Statement},
};
use bang_typechecker::{StaticTypeInfo, StdlibModule, VariableType};

pub fn completions(file: &Document, position: lsp::Position) -> lsp::CompletionList {
  let position = span_from_lsp_position(position, file);

  if let Some(import_statement) = in_import_statement(&file.ast, position) {
    let first_item_start = import_statement
      .items(&file.ast)
      .next()
      .map(|i| i.span.start);
    let before_items = first_item_start.is_none_or(|start| position.end < start);
    if before_items {
      return module_completions();
    }

    let in_items = import_statement.items_span(&file.ast).contains(position);
    if in_items {
      return module_item_completions(import_statement.module(&file.ast), true);
    }
  }

  if let Some(module_access) = in_module_access(&file.ast, position) {
    return module_item_completions(module_access.module(&file.ast), false);
  }

  if in_type_annotation(&file.ast, position) {
    return lsp::CompletionList {
      is_incomplete: false,
      items: type_annotation_snippets().into(),
    };
  }

  variable_completions(file, position)
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

fn variable_completions(file: &Document, position: Span) -> lsp::CompletionList {
  let typechecker = file.typechecker();

  let items = typechecker
    .variables()
    .filter(|var| var.is_active(position))
    .map(|variable| {
      let name = variable.name();
      let type_info = variable.get_type_info().unwrap();

      variable_completion(name, type_info, variable.documentation(), false)
    })
    .chain(constant_snippets())
    .chain(module_access_snippets())
    .collect();

  lsp::CompletionList {
    items,
    is_incomplete: false,
  }
}

fn variable_completion(
  name: &str,
  type_info: &StaticTypeInfo,
  documentation: Option<&str>,
  in_import: bool,
) -> lsp::CompletionItem {
  lsp::CompletionItem {
    kind: Some(match type_info.kind {
      VariableType::Function | VariableType::FunctionNoArgs => lsp::CompletionItemKind::FUNCTION,
      VariableType::Variable if is_screaming_snake_case(name) => lsp::CompletionItemKind::CONSTANT,
      VariableType::Variable => lsp::CompletionItemKind::VARIABLE,
    }),

    label: match type_info.kind {
      VariableType::Function if !in_import => format!("{name}(…)"),
      VariableType::FunctionNoArgs if !in_import => format!("{name}()"),
      _ => name.to_owned(),
    },
    label_details: Some(lsp::CompletionItemLabelDetails {
      detail: None,
      description: Some(type_info.string.clone()),
    }),
    documentation: documentation.map(|docs| {
      lsp::Documentation::MarkupContent(lsp::MarkupContent {
        kind: lsp::MarkupKind::Markdown,
        value: format!("```\n{}\n```\n{docs}", type_info.string),
      })
    }),

    // If it is a function, we want to use the function snippet
    insert_text: match type_info.kind {
      VariableType::Function if !in_import => Some(format!("{name}($0)")),
      VariableType::FunctionNoArgs if !in_import => Some(format!("{name}()")),
      _ => Some(name.to_owned()),
    },
    insert_text_format: match type_info.kind {
      VariableType::Function if !in_import => Some(lsp::InsertTextFormat::SNIPPET),
      _ => None,
    },

    ..Default::default()
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
    kind: Some(lsp::CompletionItemKind::MODULE),

    label: (*module).to_owned(),
    label_details: Some(lsp::CompletionItemLabelDetails {
      detail: Some("::".to_owned()),
      description: None,
    }),

    insert_text: Some(format!("{module}::$0")),
    insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),

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
  let module = StdlibModule::get(module);

  let items = module
    .items()
    .iter()
    .map(|item| {
      let type_info = module.type_info(item);
      let documentation = module.docs(item);

      variable_completion(item, &type_info, documentation, in_import)
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
