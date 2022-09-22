Hyuga - Yet Another Hy Language Server
======================================

[![PyPI version](https://badge.fury.io/py/hyuga.svg)](https://badge.fury.io/py/hyuga)

Forked from [hy-language-server](https://github.com/rinx/hy-language-server).

Verified-working Hy version: [0.24.0](https://github.com/hylang/hy/tree/stable)

## Feature

- `textDocument/did{Open,Change}`
- `textDocument/completion`
  - Show candidates all modules installed in your system, classes/functions in opening source. (plain Python-symbols included)
- `textDocument/definition`
  - Jump to definition. (currently refered hy-source only)
- `textDocument/hover`

## Screenshots

### Completion

![Hyuga sample movie: completion](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-completion.gif)

### Jump to definition

![Hyuga sample movie: jump-to-definition](https://raw.githubusercontent.com/sakuraiyuta/hyuga/images/hyuga-image-jump-def.gif)


## Install

### plain install

```bash
pip3 install hyuga
```

### [neovim(nvim)](https://github.com/neovim/neovim) + [vim-lsp](https://github.com/prabirshrestha/vim-lsp) + [vim-lsp-settings]()

**Note:** Currently `vim-lsp-settings` doesn't have installer for Hyuga.
You can test with [my vim-lsp-settings branch](https://github.com/sakuraiyuta/vim-lsp-settings/tree/add-lang/hyuga).

Sample for dein:

```vim
call dein#add('sakuraiyuta/vim-lsp-settings', {'rev': 'add-lang/hyuga'})
```

And open `*.hy` file with `filetype=hy`, then run `:LspInstallServer`

### [Visual Studio Code(VSCode)](https://code.visualstudio.com)

TODO: implement

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
