# Editor Support

This folder contains editor support for Anvyx `.anv` files.

Available integrations:

- `vscode/` for VS Code syntax highlighting and LSP diagnostics
- `nvim/` for Neovim with Tree-sitter

## VS Code

The VS Code extension registers `.anv` files, provides TextMate highlighting, and starts `anvyx lsp` for diagnostics. Hover, completion, formatting, go-to-definition, and semantic tokens are not implemented yet.

Build the compiler binary from the repo root:

```bash
rtk cargo build -p anvyx
```

Build and launch the extension from this repo for one VS Code development session:

```bash
cd editors/vscode
npm install
npm run compile
code --extensionDevelopmentPath="$(pwd)"
```

To keep this checkout installed permanently in VS Code, build it once and symlink it into your VS Code extensions directory:

```bash
cd editors/vscode
npm install
npm run compile
mkdir -p ~/.vscode/extensions
ln -sfn "$(pwd)" ~/.vscode/extensions/anvyx
```

Then reload VS Code. After that, opening any `.anv` file from any workspace loads this extension. Re-run `npm run compile` after changing the extension TypeScript.


For local development, set `anvyx.serverPath` to the absolute path of the compiler binary, for example:

```json
"anvyx.serverPath": "/path/to/anvyx-clean/target/debug/anvyx"
```

Reload the VS Code window after changing `anvyx.serverPath`.

For an installed compiler, run this from the repo root:

```bash
just install
```

Then leave `anvyx.serverPath` as the default `anvyx` if the installed binary is on your `PATH`.

## Neovim

The Neovim support uses Tree-sitter.

You need:

- Neovim 0.9 or newer
- `nvim-treesitter`
- Node.js
- a C compiler

First, build the parser from the repo root:

```bash
cd editors/nvim
npm install
npx tree-sitter generate
```

Then add this to your `init.lua`:

```lua
vim.filetype.add({ extension = { anv = "anvyx" } })

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.anvyx = {
  install_info = {
    url = "/path/to/this/repo/editors/nvim",
    files = { "src/parser.c" },
  },
  filetype = "anvyx",
}
```

Replace `/path/to/this/repo` with the real path to your checkout.

Then install the parser inside Neovim:

```vim
:TSInstall anvyx
```

Next, make the highlight queries available:

```bash
mkdir -p ~/.config/nvim/after/queries/anvyx
ln -sf /path/to/this/repo/editors/nvim/queries/highlights.scm \
  ~/.config/nvim/after/queries/anvyx/highlights.scm
```

Make sure Tree-sitter highlighting is enabled in your Neovim config:

```lua
require("nvim-treesitter.configs").setup({
  highlight = { enable = true },
})
```

After that, open any `.anv` file and highlighting should work.
