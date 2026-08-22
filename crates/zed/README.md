# Bang for Zed

Runs the [Bang language server](../lsp) inside [Zed](https://zed.dev).

### Installation

The extension doesn't bundle Bang, so first install the `bang` executable and
make sure it is on your `PATH`.

The extension needs to be
installed manually. Open the command palette and run
`zed: install dev extension`, then select this folder. Zed compiles the extension itself,
which needs [Rust installed with rustup](https://www.rust-lang.org/tools/install).

### Settings

If `bang` isn't on your `PATH`, point Zed at it:

```json
{
  "lsp": {
    "bang": {
      "binary": {
        "path": "/path/to/bang",
        "arguments": ["lsp"]
      }
    }
  }
}
```

### Syntax Highlighting

There is no Tree-sitter grammar for Bang, so highlighting comes from the
semantic tokens produced by the language server. Zed doesn't request those by
default, so it needs to be turned on:

```json
{
  "languages": {
    "Bang": {
      "semantic_tokens": "full"
    }
  }
}
```

### Development

Zed rebuilds the extension when it is reloaded, but it can be checked with:

```sh
cargo build --target wasm32-wasip2
cargo clippy --target wasm32-wasip2
```
