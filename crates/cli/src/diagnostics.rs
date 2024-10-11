use bang_parser::{LineIndex, Span};
use owo_colors::OwoColorize;
use std::fmt;

#[derive(Debug)]
pub enum Severity {
  Error,
  Warning,
}
pub struct Message {
  pub title: String,
  pub body: String,
  pub severity: Severity,
}
impl Message {
  pub fn error(message: String) -> Self {
    Self {
      title: message,
      body: String::new(),
      severity: Severity::Error,
    }
  }
  pub fn warning(message: String) -> Self {
    Self {
      title: message,
      body: String::new(),
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

    Ok(())
  }
}
impl From<&bang_parser::ParseError> for Message {
  fn from(error: &bang_parser::ParseError) -> Self {
    Self {
      title: error.title(),
      body: error.message(),
      severity: Severity::Error,
    }
  }
}
impl From<&bang_interpreter::CompileError> for Message {
  fn from(error: &bang_interpreter::CompileError) -> Self {
    Self {
      title: error.title().to_owned(),
      body: error.message().to_owned(),
      severity: Severity::Error,
    }
  }
}
impl From<&bang_interpreter::RuntimeError> for Message {
  fn from(error: &bang_interpreter::RuntimeError) -> Self {
    Self {
      title: error.title().to_owned(),
      body: error.message(),
      severity: Severity::Error,
    }
  }
}
impl From<&bang_linter::LintDiagnostic> for Message {
  fn from(error: &bang_linter::LintDiagnostic) -> Self {
    Self {
      title: error.title.to_owned(),
      body: error.message.to_owned(),
      severity: Severity::Warning,
    }
  }
}
impl From<&bang_typechecker::TypeError> for Message {
  fn from(error: &bang_typechecker::TypeError) -> Self {
    Self {
      title: error.title().to_string(),
      body: error.message(),
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
      title,
      source,
      span,
      lines: LineIndex::from_source(source),
    }
  }
}
impl fmt::Display for CodeFrame<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let start_line = self.lines.get_line(self.span).max(1);
    let end_line = self.lines.get_final_line(self.span);

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
      write!(f, "{line:>3} {} {line_text}", "│".dimmed())?;
      if !line_text.ends_with('\n') {
        writeln!(f)?;
      }
    }

    write!(f, "{}", "────╯".dimmed())
  }
}
