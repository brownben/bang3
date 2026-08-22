//! # Zed Extension
//! Runs the Bang language server inside [Zed](https://zed.dev).
//!
//! The extension doesn't bundle Bang, it expects `bang` to be on the `PATH`,
//! or for the path to it to be given in the editor settings.

use zed_extension_api::{
  self as zed, Command, LanguageServerId, Result, Worktree, settings::LspSettings,
};

/// The executable which the language server is a subcommand of
const EXECUTABLE: &str = "bang";
/// The subcommand of the executable which starts the language server
const SUBCOMMAND: &str = "lsp";

/// The Bang extension for Zed
struct BangExtension;

impl zed::Extension for BangExtension {
  fn new() -> Self {
    Self
  }

  fn language_server_command(
    &mut self,
    language_server_id: &LanguageServerId,
    worktree: &Worktree,
  ) -> Result<Command> {
    let settings = LspSettings::for_worktree(language_server_id.as_ref(), worktree)
      .map(|settings| settings.binary)
      .unwrap_or_default();

    let command = match settings.as_ref().and_then(|binary| binary.path.clone()) {
      Some(path) => path,
      None => worktree.which(EXECUTABLE).ok_or_else(|| {
        format!(
          "Could not find `{EXECUTABLE}` on your PATH. \
           Install Bang, or set `lsp.bang.binary.path` in your Zed settings."
        )
      })?,
    };

    let args = (settings.as_ref())
      .and_then(|binary| binary.arguments.clone())
      .unwrap_or_else(|| vec![SUBCOMMAND.to_owned()]);

    let mut env = worktree.shell_env();
    if let Some(extra_env) = settings.and_then(|binary| binary.env) {
      env.extend(extra_env);
    }

    Ok(Command { command, args, env })
  }
}

zed_extension_api::register_extension!(BangExtension);
