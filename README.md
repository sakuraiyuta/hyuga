Hyuga - Yet Another Hy Language Server
======================================

[![PyPI version](https://badge.fury.io/py/hyuga.svg)](https://badge.fury.io/py/hyuga)

Forked from [hy-language-server](https://github.com/rinx/hy-language-server).

**This software is still in the experimental stage!**

Please be aware that specifications may change without prior notice.

## Status

Verified-working Hy version: [1.0.0](https://github.com/hylang/hy/tree/stable)

|Hy version|Hyuga Support|
|----------|-------------|
|0.24      |0.2.1        |
|0.25      |?            |
|0.26      |?            |
|0.27      |?            |
|0.28      |?            |
|1.0.0     |1.0.0        |

We need your help to check the support status for each hy version.

Please report the results [in issues](https://github.com/sakuraiyuta/hyuga/issues/17)!

## Feature

- `textDocument/did{Open,Change}`
- `textDocument/completion`
  - Display a list of all modules installed in your system, including classes/functions in the currently opening source. (Plain Python symbols are also included.)
- `textDocument/definition`
  - Jump to the definition. (Currently, this refers to hy-source only.)
- `textDocument/hover`

## Screenshots

### Completion

![Hyuga sample movie: completion on neovim](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-completion.gif)
![Hyuga sample movie: completion on vscode](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-completion-vscode.gif)

### Jump to definition

![Hyuga sample movie: jump-to-definition on neovim](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-jump-def.gif)
![Hyuga sample movie: jump-to-definition on vscode](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-jump-def-vscode.gif)


## Install

### Regular Global Install

```bash
pip3 install hyuga
```

On Arch based systems, or other systems that use an externally managed Python environment where the above is not possible:
```bash
pipx install hyuga
```
Also, ensure you installed `hy` the same way via `pipx`, not `pacman`/`yay`, as the package has not yet been updated to `1.0.0`, and is currently stuck at `0.29.0-1` for the moment. This likely won't apply in the future as it will inevitably be updated, but `pipx` is generally the safer route here as it pulls it straight from pypi, and doesn't depend on a maintainer to keep it up to date. 

## Setup

### [Neovim(nvim)](https://github.com/neovim/neovim) Lua setup via [lspconfig](https://github.com/neovim/nvim-lspconfig)

Install [vim-hy](https://github.com/hylang/vim-hy) for filetype detection. This is optional, as you can register the filetype yourself, but you'll likely want to run this anyway if you're using Hy with Neovim, as it provides syntax highlighting among other things.

Then define an entry for Hyuga:

```lua
local lspconfig = require("lspconfig")
local lsp_configs = require("lspconfig.configs")

if not lsp_configs.hy then
  lsp_configs.hy = {
    default_config = {
      cmd = { 'hyuga' },
      filetypes = { 'hy' },
      root_dir = function(fname)
        return lspconfig.util.path.dirname(fname)
      end,
    },
    docs = {
      description = "Hyuga language server for the Hy programming language, a Python dialect of LISP"
    }
  }
end
```

And finally, don't forget to run setup! As a reference, here's how I have it set up. I prefer to defer setup (you may need to run `:LspStart` in case it doesn't auto-attach on the first `hy` file that gets opened, but only the first)
```lua
vim.api.nvim_create_autocmd("FileType", {
  once = true,
  pattern = 'hy',
  callback = function(_)
    local ls_entry = lsp_configs.hy
    ls_entry.setup {
      -- Any extended capabilities or custom on_attach functions go here as usual,
      -- e.g. for nvim-cmp other such plugins where you'd extend capabilities:
      -- capabilities = extended_capabilities,
      -- on_attach = custom_on_attach
      -- Can just be left blank, but included for reference. 
    }
  end
})
```

---

### [neovim(nvim)](https://github.com/neovim/neovim) + [vim-lsp](https://github.com/prabirshrestha/vim-lsp) + [vim-lsp-settings](https://github.com/mattn/vim-lsp-settings)

Install [vim-lsp](https://github.com/prabirshrestha/vim-lsp) and [vim-lsp-settings](https://github.com/mattn/vim-lsp-settings), open a `*.hy` file with `filetype=hy`, then run `:LspInstallServer`

---

### [Visual Studio Code(VSCode)](https://code.visualstudio.com)

- Install `hyuga` to your python environment. (Refer to the `Plain install` section for details.)
- In VSCode, open `Extensions` view, search for `hyuga`, and install `Hyuga VSCode Client`.
  - or: visit [Hyuga VSCode Client - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=sakuraiyuta.hyuga-vscode-client) and install.

## Development

### Setup

- Install [poetry](https://github.com/python-poetry/poetry).
- Clone this project: `git clone https://github.com/sakuraiyuta/hyuga.git`
- In project directory, execute `poetry install`.

### Test

```bash
poetry run pytest tests
```

## License

MIT
