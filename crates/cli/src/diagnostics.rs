use bang_syntax::{LineIndex, Span};
use owo_colors::{OwoColorize, Style};
use std::fmt;

#[derive(Debug)]
pub enum Severity {
  Error,
  Warning,
}
pub struct Message {
  pub title: String,
  pub body: String,
  pub hint: Option<String>,
  pub severity: Severity,
}
impl Message {
  pub fn error(message: String) -> Self {
    Self {
      title: message,
      body: String::new(),
      hint: None,
      severity: Severity::Error,
    }
  }
  pub fn warning(message: String) -> Self {
    Self {
      title: message,
      body: String::new(),
      hint: None,
      severity: Severity::Warning,
    }
  }
}
impl fmt::Display for Message {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.severity {
      Severity::Error => write!(f, "{}", "✕ Error".bold().red()),
      Severity::Warning => write!(f, "{}", "⚠ Warning".bold().yellow()),
    }?;
    writeln!(f, "{} {}", ":".bold(), &self.title.bold())?;

    if !self.body.is_empty() {
      writeln!(f, "{}", &self.body)?;
    }

    if let Some(hint) = &self.hint {
      writeln!(f, "{} {}", "hint:".italic().cyan(), hint)?;
    }

    Ok(())
  }
}
impl From<&bang_syntax::ParseError> for Message {
  fn from(error: &bang_syntax::ParseError) -> Self {
    Self {
      title: error.title(),
      body: error.message(),
      hint: None,
      severity: Severity::Error,
    }
  }
}
impl From<&bang_interpreter::CompileError> for Message {
  fn from(error: &bang_interpreter::CompileError) -> Self {
    Self {
      title: error.title().to_owned(),
      body: error.message().to_owned(),
      hint: None,
      severity: Severity::Error,
    }
  }
}
impl From<&bang_interpreter::RuntimeError> for Message {
  fn from(error: &bang_interpreter::RuntimeError) -> Self {
    Self {
      title: error.title().to_owned(),
      body: error.message(),
      hint: None,
      severity: Severity::Error,
    }
  }
}
impl From<&bang_linter::LintDiagnostic> for Message {
  fn from(error: &bang_linter::LintDiagnostic) -> Self {
    Self {
      title: error.title.to_owned(),
      body: error.message.to_owned(),
      hint: None,
      severity: Severity::Warning,
    }
  }
}
impl From<&bang_typechecker::TypeError> for Message {
  fn from(error: &bang_typechecker::TypeError) -> Self {
    Self {
      title: error.title().to_string(),
      body: error.message(),
      hint: error.suggestion(),
      severity: if error.is_warning() {
        Severity::Warning
      } else {
        Severity::Error
      },
    }
  }
}

pub struct CodeFrame<'a> {
  title: &'a str,
  source: &'a str,
  span: Span,

  lines: LineIndex,
}
impl<'a> CodeFrame<'a> {
  pub fn new(title: &'a str, source: &'a str, span: Span) -> Self {
    Self {
      title: if title == "-" { "STDIN" } else { title },
      source,
      span,
      lines: LineIndex::from_source(source),
    }
  }
}
impl fmt::Display for CodeFrame<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let start_line = self.lines.line(self.span).max(1);
    let end_line = self.lines.final_line(self.span);

    writeln!(
      f,
      "    {}{}{}{}{}",
      "╭─[".dimmed(),
      self.title,
      ":".dimmed(),
      start_line,
      "]".dimmed()
    )?;

    for line in start_line..=end_line {
      let line_text = self.lines.line_span(line).source_text(self.source);
      write!(f, "{line:>3} {}", "│".dimmed())?;
      if !line_text.is_empty() {
        write!(f, " ")?;
      }
      highlight_source(f, line_text)?;
      if !line_text.ends_with('\n') {
        writeln!(f)?;
      }
    }

    write!(f, "{}", "────╯".dimmed())
  }
}

pub fn highlight_source(output: &mut dyn fmt::Write, source: &str) -> fmt::Result {
  use bang_syntax::{TokenKind, tokenise};

  let mut last = 0;
  for token in tokenise(source) {
    // if there is a gap between tokens, add spaces for the gap
    if token.start != last {
      for _ in 0..(token.start - last) {
        write!(output, " ").unwrap();
      }
    }

    let style = match token.kind {
      TokenKind::Number | TokenKind::True | TokenKind::False => Style::new().blue(),
      TokenKind::String
      | TokenKind::FormatStringStart
      | TokenKind::FormatStringPart
      | TokenKind::FormatStringEnd
      | TokenKind::FormatStringUnterminated
      | TokenKind::UnterminatedString => Style::new().green(),
      TokenKind::As
      | TokenKind::Else
      | TokenKind::From
      | TokenKind::If
      | TokenKind::Import
      | TokenKind::Match
      | TokenKind::Return => Style::new().cyan(),
      TokenKind::Let => Style::new().magenta(),
      TokenKind::Comment => Style::new().dimmed(),
      _ => Style::new(),
    };
    let token_text = Span::from(token).source_text(source);
    write!(output, "{}", token_text.style(style))?;

    last = token.start + u32::from(token.length);
  }

  Ok(())
}
