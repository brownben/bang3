use lsp_types::{self as lsp};

use crate::locations::{lsp_position_from_span_start, span_from_lsp_range};
use crate::{documents::Document, requests::variables::find_variable};

use bang_syntax::{AST, Span, Token, TokenKind, ast, tokenise};
use bang_typechecker::Variable;

pub enum SemanticTokenKind {
  Type,
  TypeParameter,
  Parameter,
  Variable,
  Function,
  Keyword,
  Comment,
  String,
  Number,
  Operator,
  Boolean,
  Module,
}
impl SemanticTokenKind {
  pub const SUPPORTED_KINDS: [lsp::SemanticTokenType; 12] = [
    lsp::SemanticTokenType::TYPE,
    lsp::SemanticTokenType::TYPE_PARAMETER,
    lsp::SemanticTokenType::PARAMETER,
    lsp::SemanticTokenType::VARIABLE,
    lsp::SemanticTokenType::FUNCTION,
    lsp::SemanticTokenType::KEYWORD,
    lsp::SemanticTokenType::COMMENT,
    lsp::SemanticTokenType::STRING,
    lsp::SemanticTokenType::NUMBER,
    lsp::SemanticTokenType::OPERATOR,
    lsp::SemanticTokenType::new("boolean"),
    lsp::SemanticTokenType::new("module"),
  ];
}
impl From<SemanticTokenKind> for u32 {
  fn from(kind: SemanticTokenKind) -> Self {
    match kind {
      SemanticTokenKind::Type => 1,
      SemanticTokenKind::TypeParameter => 1,
      SemanticTokenKind::Parameter => 2,
      SemanticTokenKind::Variable => 3,
      SemanticTokenKind::Function => 4,
      SemanticTokenKind::Keyword => 5,
      SemanticTokenKind::Comment => 6,
      SemanticTokenKind::String => 7,
      SemanticTokenKind::Number => 8,
      SemanticTokenKind::Operator => 9,
      SemanticTokenKind::Boolean => 10,
      SemanticTokenKind::Module => 11,
    }
  }
}

pub enum SemanticTokenModifier {
  None,
  Documentation,
  Declaration,
}
impl SemanticTokenModifier {
  pub const SUPPORTED_MODIFIERS: [lsp::SemanticTokenModifier; 2] = [
    lsp::SemanticTokenModifier::DOCUMENTATION,
    lsp::SemanticTokenModifier::DECLARATION,
  ];
}
impl From<SemanticTokenModifier> for u32 {
  fn from(modifier: SemanticTokenModifier) -> Self {
    match modifier {
      SemanticTokenModifier::None => 0,
      SemanticTokenModifier::Documentation => 1 << 0,
      SemanticTokenModifier::Declaration => 1 << 1,
    }
  }
}

pub fn semantic_tokens(file: &Document, range: Option<lsp::Range>) -> lsp::SemanticTokens {
  let range = if let Some(range) = range {
    span_from_lsp_range(range, file)
  } else {
    file.ast.line_index().file_span()
  };

  let mut last_token_location = lsp::Position::new(0, 0);
  let tokens = tokenise(&file.ast.source)
    .filter(|token| range.contains(Span::from(token)))
    .filter_map(|token| token_type(token, file).map(|x| (x, token)))
    .map(|(token_type, token)| {
      let token_position = lsp_position_from_span_start(Span::from(token), file);
      let (delta_line, delta_start) = position_delta(last_token_location, token_position);
      last_token_location = token_position;

      lsp::SemanticToken {
        delta_line,
        delta_start,
        length: token.length.into(),
        token_type: token_type.into(),
        token_modifiers_bitset: token_modifiers(token, &file.ast).into(),
      }
    })
    .collect();

  lsp::SemanticTokens {
    result_id: None,
    data: tokens,
  }
}

fn position_delta(last: lsp::Position, current: lsp::Position) -> (u32, u32) {
  let delta_line = current.line - last.line;
  let delta_start = if delta_line == 0 {
    current.character - last.character
  } else {
    current.character
  };

  (delta_line, delta_start)
}

fn token_type(token: Token, file: &Document) -> Option<SemanticTokenKind> {
  match token.kind {
    TokenKind::Identifier => identifier_token_type(token, file),
    TokenKind::Comment => Some(SemanticTokenKind::Comment),
    TokenKind::Number => Some(SemanticTokenKind::Number),
    TokenKind::True | TokenKind::False => Some(SemanticTokenKind::Boolean),
    kind if kind.is_operator() => Some(SemanticTokenKind::Operator),
    kind if kind.is_string() => Some(SemanticTokenKind::String),
    kind if kind.is_keyword() => Some(SemanticTokenKind::Keyword),
    _ => None,
  }
}

fn identifier_token_type(token: Token, file: &Document) -> Option<SemanticTokenKind> {
  if is_module_access(token, &file.ast) {
    return Some(SemanticTokenKind::Module);
  }

  let typechecker = file.typechecker();
  if let Some(variable) = find_variable(Span::from(token), typechecker) {
    return Some(variable_token_type(token, variable, &file.ast));
  }

  if let Some(type_) =
    (file.ast.types.iter()).find(|ty| ty.span(&file.ast).contains(Span::from(token)))
  {
    return match type_ {
      ast::Type::Primitive(_) | ast::Type::Structure(_) => Some(SemanticTokenKind::Type),
      // we don't highlight type variables for now, as we don't highlight the leading `^`
      // ast::Type::Variable(_) => None,
      _ => None,
    };
  }

  Some(SemanticTokenKind::Variable)
}

fn variable_token_type(token: Token, variable: &Variable, ast: &AST) -> SemanticTokenKind {
  if let Some(type_info) = variable.get_type_info()
    && type_info.kind.is_function()
  {
    if let Some(identifier_span) = variable.span()
      && is_let_body_a_function(identifier_span, ast)
    {
      return SemanticTokenKind::Function;
    }

    if is_callee(token, ast) {
      return SemanticTokenKind::Function;
    }
  }

  if variable.is_parameter() {
    return SemanticTokenKind::Parameter;
  }

  SemanticTokenKind::Variable
}

fn token_modifiers(token: Token, ast: &AST) -> SemanticTokenModifier {
  match token.kind {
    TokenKind::Let => SemanticTokenModifier::Declaration,
    TokenKind::Comment if is_documentation_comment(token, ast) => {
      SemanticTokenModifier::Documentation
    }
    _ => SemanticTokenModifier::None,
  }
}

fn is_documentation_comment(token: Token, ast: &AST) -> bool {
  (ast.all_statements())
    .filter_map(|stmt| match stmt {
      ast::Statement::Let(let_) => let_.doc_comment(ast),
      _ => None,
    })
    .any(|doc_comment| doc_comment.span(ast).contains(Span::from(token)))
}

fn is_module_access(token: Token, ast: &AST) -> bool {
  (ast.expressions.iter()).any(|expr| match expr {
    ast::Expression::ModuleAccess(module) => module.module_span(ast) == Span::from(token),
    _ => false,
  })
}

fn is_callee(token: Token, ast: &AST) -> bool {
  (ast.expressions.iter()).any(|expr| match expr {
    ast::Expression::Call(call) => call.callee(ast).span(ast).contains(Span::from(token)),
    _ => false,
  })
}

fn is_let_body_a_function(identifier: Span, ast: &AST) -> bool {
  (ast.all_statements()).any(|stmt| match stmt {
    ast::Statement::Let(let_) => {
      let_.identifier_span(ast) == identifier
        && matches!(let_.value(ast), ast::Expression::Function(_))
    }
    _ => false,
  })
}
