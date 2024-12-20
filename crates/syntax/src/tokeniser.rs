use crate::span::Span;
use std::{fmt, iter};

/// Convert a string of source code into an [Iterator] of [Token]s
pub struct Tokeniser<'source> {
  /// The source code to tokenise
  source: &'source [u8],
  /// The current position in the source code
  position: usize,
  /// The stack of curly braces, used to check if it is for a format string or block
  curly_stack: Vec<BlockLocation>,
}
impl<'source> From<&'source str> for Tokeniser<'source> {
  /// Create a new [Tokeniser] from a source code string
  ///
  /// # Panics
  /// Panics if the length of the source code is greater than `u32::MAX`
  fn from(value: &'source str) -> Self {
    assert!(value.len() < u32::MAX as usize);

    Self {
      source: value.as_ref(),
      position: 0,
      curly_stack: Vec::with_capacity(8),
    }
  }
}
impl Tokeniser<'_> {
  /// Has the end of the source code been reached?
  fn is_end(&self, position: usize) -> bool {
    position >= self.source.len()
  }

  /// Get the next token from the source code
  fn get_next_token(&mut self) -> (TokenKind, usize) {
    if self.is_end(self.position) {
      return (TokenKind::EndOfFile, 0);
    }

    let character = &self.source[self.position];
    let next_character = self.source.get(self.position + 1);

    match character {
      // Whitespace + Comments
      b' ' | b'\r' | b'\t' => {
        self.position += 1;
        self.get_next_token()
      }
      b'\n' => (TokenKind::EndOfLine, 1),
      b'/' if matches!(next_character, Some(b'/')) => self.comment(),

      // Values
      quote @ (b'"' | b'\'' | b'`') => self.string(*quote),
      b'0'..=b'9' => self.number(),
      b'-' if matches!(next_character, Some(b'0'..=b'9')) => self.number(),
      b'_' | b'a'..=b'z' | b'A'..=b'Z' => self.identifier(),
      b'.' if matches!(next_character, Some(b'0'..=b'9')) => self.number(),

      // Brackets
      b'(' => (TokenKind::LeftParen, 1),
      b')' => (TokenKind::RightParen, 1),
      b'[' => (TokenKind::LeftSquare, 1),
      b']' => (TokenKind::RightSquare, 1),
      b'{' => {
        if !self.curly_stack.is_empty() {
          self.curly_stack.push(BlockLocation::Block);
        }
        (TokenKind::LeftCurly, 1)
      }
      b'}' => {
        if let Some(BlockLocation::FormatStringExpression) = self.curly_stack.pop() {
          self.format_string()
        } else {
          (TokenKind::RightCurly, 1)
        }
      }

      // Separators
      b',' => (TokenKind::Comma, 1),
      b':' if matches!(next_character, Some(b':')) => (TokenKind::ColonColon, 2),
      b':' => (TokenKind::Colon, 1),
      b'^' => (TokenKind::Caret, 1),

      // Logical Operators
      b'&' if matches!(next_character, Some(b'&')) => (TokenKind::And, 2),
      b'|' if matches!(next_character, Some(b'|')) => (TokenKind::Or, 2),

      // Pattern
      b'|' => (TokenKind::Pipe, 1),
      b'-' if matches!(next_character, Some(b'>')) => (TokenKind::RightArrow, 2),
      b'.' if matches!(next_character, Some(b'.')) => (TokenKind::DotDot, 2),

      // Operators
      b'+' if matches!(next_character, Some(b'+')) => (TokenKind::PlusPlus, 2),
      b'+' => (TokenKind::Plus, 1),
      b'-' => (TokenKind::Minus, 1),
      b'/' => (TokenKind::Slash, 1),
      b'*' => (TokenKind::Star, 1),
      b'%' => (TokenKind::Percent, 1),

      // Functions
      b'>' if matches!(next_character, Some(b'>')) => (TokenKind::RightRight, 2),
      b'=' if matches!(next_character, Some(b'>')) => (TokenKind::FatRightArrow, 2),

      // Equalities
      b'!' if matches!(next_character, Some(b'=')) => (TokenKind::BangEqual, 2),
      b'=' if matches!(next_character, Some(b'=')) => (TokenKind::EqualEqual, 2),
      b'<' if matches!(next_character, Some(b'=')) => (TokenKind::LessEqual, 2),
      b'>' if matches!(next_character, Some(b'=')) => (TokenKind::GreaterEqual, 2),
      b'!' => (TokenKind::Bang, 1),
      b'=' => (TokenKind::Equal, 1),
      b'<' => (TokenKind::Less, 1),
      b'>' => (TokenKind::Greater, 1),

      // Unknown character
      x if (x & 0b1111_0000) == 0b1111_0000 => (TokenKind::Unknown, 4),
      x if (x & 0b1110_0000) == 0b1110_0000 => (TokenKind::Unknown, 3),
      x if (x & 0b1100_0000) == 0b1100_0000 => (TokenKind::Unknown, 2),
      _ => (TokenKind::Unknown, 1),
    }
  }

  /// Skip to the end of a comment token (a newline)
  fn comment(&self) -> (TokenKind, usize) {
    let length = self.source[self.position..]
      .iter()
      .take_while(|c| **c != b'\n')
      .count();

    (TokenKind::Comment, length)
  }

  /// Go to the end of a string token, the closing quote
  fn string(&mut self, quote: u8) -> (TokenKind, usize) {
    let mut pos = self.position + 1;

    loop {
      if self.is_end(pos) {
        break (TokenKind::UnterminatedString, pos - self.position);
      } else if self.source[pos] == quote {
        break (TokenKind::String, pos - self.position + 1);
      } else if quote == b'`' && self.source[pos] == b'{' {
        self.curly_stack.push(BlockLocation::FormatStringExpression);
        break (TokenKind::FormatStringStart, pos - self.position + 1);
      }

      pos += 1;
    }
  }

  fn format_string(&mut self) -> (TokenKind, usize) {
    let mut pos = self.position + 1;

    loop {
      if self.is_end(pos) {
        break (TokenKind::FormatStringUnterminated, pos - self.position);
      } else if self.source[pos] == b'`' {
        break (TokenKind::FormatStringEnd, pos - self.position + 1);
      } else if self.source.get(pos + 1) == Some(&b'{') {
        self.curly_stack.push(BlockLocation::FormatStringExpression);
        break (TokenKind::FormatStringPart, pos - self.position + 2);
      }

      pos += 1;
    }
  }

  /// Get a number token, with possible decimal part and numeric separators
  fn number(&self) -> (TokenKind, usize) {
    let mut position = self.position + 1;

    // Match numbers before the decimal point
    position += self.source[position..]
      .iter()
      .take_while(|c| matches!(c, b'0'..=b'9' | b'_'))
      .count();

    // Match a decimal point
    if !self.is_end(position + 1)
      && self.source[position] == b'.'
      && self.source[position + 1].is_ascii_digit()
    {
      position += 1;
    }

    // Match numbers after the decimal point
    let after_decimal = self.source[position..]
      .iter()
      .take_while(|c| matches!(c, b'0'..=b'9' | b'_'))
      .count();
    if after_decimal > 0 {
      position += after_decimal;
    }

    (TokenKind::Number, position - self.position)
  }

  /// Get an identifier token, a sequence of [a-zA-Z0-9_]
  fn identifier(&self) -> (TokenKind, usize) {
    let mut position = self.position;

    while !self.is_end(position + 1)
      && let next_char = self.source[position + 1]
      && (next_char.is_ascii_alphanumeric() || next_char == b'_')
    {
      position += 1;
    }

    let length = position + 1 - self.position;
    (self.identifier_type(length), length)
  }

  /// Determines the type of the identifier, is it a keyword or a standard identifier
  fn identifier_type(&self, length: usize) -> TokenKind {
    match self.source[self.position] {
      b'a' if self.is_keyword(length, "as") => TokenKind::As,
      b'a' if self.is_keyword(length, "and") => TokenKind::And,
      b'e' if self.is_keyword(length, "else") => TokenKind::Else,
      b'f' if self.is_keyword(length, "false") => TokenKind::False,
      b'f' if self.is_keyword(length, "from") => TokenKind::From,
      b'i' if self.is_keyword(length, "if") => TokenKind::If,
      b'i' if self.is_keyword(length, "import") => TokenKind::Import,
      b'l' if self.is_keyword(length, "let") => TokenKind::Let,
      b'm' if self.is_keyword(length, "match") => TokenKind::Match,
      b'o' if self.is_keyword(length, "or") => TokenKind::Or,
      b'r' if self.is_keyword(length, "return") => TokenKind::Return,
      b't' if self.is_keyword(length, "true") => TokenKind::True,
      _ => TokenKind::Identifier,
    }
  }

  /// Checks if the source of the current token is equal to a keyword
  fn is_keyword(&self, length: usize, keyword: &'static str) -> bool {
    let end = self.position + length;
    &self.source[self.position..end] == keyword.as_bytes()
  }
}
impl Iterator for Tokeniser<'_> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    if self.is_end(self.position) {
      return None;
    }

    let (kind, len) = self.get_next_token();
    let start = self.position;
    self.position += len;

    Some(Token {
      kind,
      start: u32::try_from(start).unwrap(),
      length: u16::try_from(self.position - start).unwrap(),
    })
  }
}
impl iter::FusedIterator for Tokeniser<'_> {}

/// A Token of source code, a lexeme of the language
///
/// With the type of token, start position and length of the token in the source code
#[derive(Clone, Copy, Debug, Default)]
pub struct Token {
  /// The type of token
  pub kind: TokenKind,
  /// The byte offset to the start of the token in the source code
  pub start: u32,
  /// The length of the token
  pub length: u16,
}
impl From<Token> for Span {
  fn from(token: Token) -> Self {
    Self {
      start: token.start,
      end: token.start + u32::from(token.length),
    }
  }
}
impl From<&Token> for Span {
  fn from(token: &Token) -> Self {
    Self {
      start: token.start,
      end: token.start + u32::from(token.length),
    }
  }
}

/// The type of a token
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum TokenKind {
  // Brackets
  /// `(`
  LeftParen,
  /// `)`
  RightParen,
  /// `{`
  LeftCurly,
  /// `}`
  RightCurly,
  /// `[`
  LeftSquare,
  /// `]`
  RightSquare,

  // Separators
  /// `,`
  Comma,
  /// `:`
  Colon,
  /// `::`
  ColonColon,
  /// `^`
  Caret,

  // Operators
  /// `-`
  Minus,
  /// `+`
  Plus,
  /// `*`
  Slash,
  /// `*`
  Star,
  /// `%`
  Percent,
  /// `++`
  PlusPlus,
  /// `!`
  Bang,
  /// `and` or `&&`
  And,
  /// `or` or `||`
  Or,

  // Functions
  /// `>>`
  RightRight,
  /// `=>`
  FatRightArrow,

  // Comparators
  /// `!=`
  BangEqual,
  /// `=`
  Equal,
  /// `==`
  EqualEqual,
  /// `>`
  Greater,
  /// `>=`
  GreaterEqual,
  /// `<`
  Less,
  /// `<=`
  LessEqual,

  // Values
  /// A identifier for a variable, a sequence of [a-zA-Z0-9_]
  Identifier,
  /// A number, with possible decimal part and numeric separators
  Number,
  /// A string, any characters between `'`, `"`, or `` ` ``
  String,

  // Format String
  /// The first string section of a format string "\`hello {"
  FormatStringStart,
  /// A string section between expressions "} stuff {"
  FormatStringPart,
  /// The last string section of a format string "} world\`"
  FormatStringEnd,
  /// An unterminated format string end
  FormatStringUnterminated,

  // Keywords
  /// `as`
  As,
  /// `else`
  Else,
  /// `false`
  False,
  /// `from`
  From,
  /// `if`
  If,
  /// `import`
  Import,
  /// `let`
  Let,
  /// `match`
  Match,
  /// `return`
  Return,
  /// `true`
  True,

  // Pattern
  /// `|`
  Pipe,
  /// `->`
  RightArrow,
  /// `..`
  DotDot,

  // Whitespace + Comments
  /// A comment, consisting of `//` then any number of characters before a newline
  Comment,
  /// The end of a line, indicating a `\n`
  EndOfLine,
  /// A token to indicate the end of the file
  EndOfFile,

  // Error
  /// An unknown character, not known to fit in a [`TokenKind`]
  #[default]
  Unknown,
  /// A string where the end of the file has been reached, thus unterminated
  UnterminatedString,
}
impl TokenKind {
  /// Does the token always have the same length?
  #[must_use]
  pub fn has_fixed_length(self) -> bool {
    !matches!(
      self,
      Self::Comment
        | Self::Unknown
        | Self::UnterminatedString
        | Self::Identifier
        | Self::Number
        | Self::String
        | Self::FormatStringStart
        | Self::FormatStringPart
        | Self::FormatStringEnd
        | Self::FormatStringUnterminated
        | Self::And // can be `&&` or `and`
    )
  }

  /// Is the token a keyword?
  #[must_use]
  pub fn is_keyword(self) -> bool {
    matches!(
      self,
      Self::As
        | Self::Else
        | Self::False
        | Self::From
        | Self::If
        | Self::Import
        | Self::Let
        | Self::Match
        | Self::Return
        | Self::True
    )
  }
}
impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      // Brackets
      Self::LeftParen => write!(f, "("),
      Self::RightParen => write!(f, ")"),
      Self::LeftCurly => write!(f, "{{"),
      Self::RightCurly => write!(f, "}}"),
      Self::LeftSquare => write!(f, "["),
      Self::RightSquare => write!(f, "]"),

      // Separators
      Self::Comma => write!(f, ","),
      Self::Colon => write!(f, ":"),
      Self::ColonColon => write!(f, "::"),
      Self::Caret => write!(f, "^"),

      // Operators
      Self::Minus => write!(f, "-"),
      Self::Plus => write!(f, "+"),
      Self::Slash => write!(f, "/"),
      Self::Star => write!(f, "*"),
      Self::Percent => write!(f, "%"),
      Self::PlusPlus => write!(f, "++"),
      Self::Bang => write!(f, "!"),
      Self::Or => write!(f, "or"),
      Self::And => write!(f, "and"),

      // Functions
      Self::RightRight => write!(f, ">>"),
      Self::FatRightArrow => write!(f, "=>"),

      // Equalities
      Self::BangEqual => write!(f, "!="),
      Self::Equal => write!(f, "="),
      Self::EqualEqual => write!(f, "=="),
      Self::Greater => write!(f, ">"),
      Self::GreaterEqual => write!(f, ">="),
      Self::Less => write!(f, "<"),
      Self::LessEqual => write!(f, "<="),

      // With Values
      Self::Identifier => write!(f, "Identifier"),
      Self::Number => write!(f, "Number"),
      Self::String => write!(f, "String"),

      // Format String
      Self::FormatStringStart => write!(f, "Format String Start"),
      Self::FormatStringPart => write!(f, "Format String Part"),
      Self::FormatStringEnd => write!(f, "Format String End"),
      Self::FormatStringUnterminated => write!(f, "Unterminated Format String"),

      // Keywords
      Self::As => write!(f, "as"),
      Self::Else => write!(f, "else"),
      Self::False => write!(f, "false"),
      Self::From => write!(f, "from"),
      Self::If => write!(f, "if"),
      Self::Import => write!(f, "import"),
      Self::Let => write!(f, "let"),
      Self::Match => write!(f, "match"),
      Self::Return => write!(f, "return"),
      Self::True => write!(f, "true"),

      // Pattern
      Self::Pipe => write!(f, "|"),
      Self::RightArrow => write!(f, "->"),
      Self::DotDot => write!(f, ".."),

      // Whitespace + Comments
      Self::Comment => write!(f, "Comment"),
      Self::EndOfLine => write!(f, "New Line"),
      Self::EndOfFile => write!(f, "End of File"),

      // Errors
      Self::Unknown => write!(f, "Unknown Character"),
      Self::UnterminatedString => write!(f, "Unterminated String"),
    }
  }
}

/// Track if a `{}` belongs to a format string or a block
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BlockLocation {
  FormatStringExpression,
  Block,
}
