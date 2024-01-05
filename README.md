Hyuga - Yet Another Hy Language Server
======================================

[![PyPI version](https://badge.fury.io/py/hyuga.svg)](https://badge.fury.io/py/hyuga)

Forked from [hy-language-server](https://github.com/rinx/hy-language-server).

** This software is still experimental stage! **

Please be aware that specifications may change without prior notice.

## Status

Verified-working Hy version: [0.24.0](https://github.com/hylang/hy/tree/stable)

|Hy version|Hyuga Support|
|----------|-------------|
|0.24      |Yes          |
|0.25      |?            |
|0.26      |?            |
|0.27      |?            |
|0.28      |?            |

We need your help to check the support status for each hy version.

Please report the results [in this issue](https://github.com/sakuraiyuta/hyuga/issues/17)!

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

### plain install

```bash
pip3 install hyuga
```

### [neovim(nvim)](https://github.com/neovim/neovim) + [vim-lsp](https://github.com/prabirshrestha/vim-lsp) + [vim-lsp-settings](https://github.com/mattn/vim-lsp-settings)

**Note:** Currently `vim-lsp-settings` doesn't have installer for Hyuga.
You can test with [my vim-lsp-settings branch](https://github.com/sakuraiyuta/vim-lsp-settings/tree/add-lang/hyuga).

Sample for dein:

```vim
call dein#add('sakuraiyuta/vim-lsp-settings', {'rev': 'add-lang/hyuga'})
```

And open `*.hy` file with `filetype=hy`, then run `:LspInstallServer`

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
