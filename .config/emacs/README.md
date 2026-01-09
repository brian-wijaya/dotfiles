# Doom Emacs Configuration

Doom Emacs user config for Arch Linux / X11.

## Files

- `init.el` - Module configuration (which Doom modules are enabled)
- `config.el` - Custom configuration and settings
- `packages.el` - Additional package declarations
- `WORKFLOW-GUIDE.md` - Comprehensive workflow guide with keybindings (`SPC f w`)

## Quick Start

If using the dotfiles install script, Doom is set up automatically. Otherwise:

```bash
# Install emacs
sudo pacman -S emacs ripgrep fd

# Clone Doom Emacs framework
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Copy user config (this directory) to ~/.config/doom
mkdir -p ~/.config/doom
cp init.el config.el packages.el ~/.config/doom/

# Install Doom
~/.config/emacs/bin/doom install

# Add to PATH (fish)
fish_add_path ~/.config/emacs/bin

# Or for bash/zsh, add to .bashrc/.zshrc:
export PATH="$HOME/.config/emacs/bin:$PATH"
```

## Daemon Mode (Recommended)

For instant startup, run Emacs as a daemon:

```bash
# Start daemon
emacs --daemon

# Open client (instant)
emacsclient -c

# Or with fallback (starts daemon if needed)
emacsclient -c -a ""
```

The i3 config auto-starts the daemon and binds `Super+E` to emacsclient.

## Enabled Modules

**Completion:**
- `vertico` - Modern completion framework
- `corfu` - Inline completion with orderless

**UI:**
- `doom-dashboard` - Startup screen
- `ligatures` - Font ligatures (→, ⇒, etc.)
- `modeline` - Doom modeline
- `ophints` - Operation hints
- `vc-gutter` - Git diff in fringe

**Editor:**
- `evil` - Vim keybindings
- `multiple-cursors` - Multi-cursor editing
- `snippets` - Code snippets

**Tools:**
- `(lsp +eglot)` - Language Server Protocol
- `magit` - Git interface
- `tree-sitter` - Better syntax highlighting
- `vterm` - Terminal emulation

**Languages:**
- `(javascript +lsp +tree-sitter)` - JS/TS
- `(go +lsp +tree-sitter)` - Go
- `(rust +lsp +tree-sitter)` - Rust
- `(python +lsp +tree-sitter)` - Python
- `(cc +lsp +tree-sitter)` - C/C++
- `org` - Org mode
- `markdown` - Markdown
- `web` - HTML/CSS
- `sh` - Shell scripts

## Key Bindings

### General
| Key | Action |
|-----|--------|
| `SPC` | Leader key |
| `SPC f f` | Find file |
| `SPC f r` | Recent files |
| `SPC b b` | Switch buffer |
| `SPC s s` | Search in buffer |
| `SPC s p` | Search in project |
| `C-h/j/k/l` | Navigate windows |

### Code
| Key | Action |
|-----|--------|
| `SPC c d` | Jump to definition |
| `SPC c r` | Find references |
| `SPC c a` | Code actions |
| `SPC c f` | Format buffer |

### Git (Magit)
| Key | Action |
|-----|--------|
| `SPC g g` | Magit status |
| `SPC g b` | Git blame |
| `SPC g l` | Git log |

### Org Mode
| Key | Action |
|-----|--------|
| `SPC o a` | Org agenda |
| `SPC o c` | Org capture |
| `SPC m t` | Toggle TODO |

### Other
| Key | Action |
|-----|--------|
| `SPC f w` | Open workflow guide |
| `SPC h r r` | Reload Doom |
| `SPC q q` | Quit |

## Language Servers

Install LSPs for your languages:

```bash
# JavaScript/TypeScript
sudo pacman -S typescript-language-server

# Python
sudo pacman -S python-lsp-server

# Go
go install golang.org/x/tools/gopls@latest

# Rust
rustup component add rust-analyzer

# C/C++
sudo pacman -S clang
```

## Updating

```bash
# Update Doom packages
doom upgrade

# After changing init.el
doom sync

# After changing config.el (no sync needed)
# Just restart or: SPC h r r
```

## Troubleshooting

**Slow startup?** Use daemon mode (see above).

**Fonts not working?** Install JetBrains Mono Nerd Font:
```bash
sudo pacman -S ttf-jetbrains-mono-nerd
```

**LSP not working?** Check language server is installed and in PATH.

**Tree-sitter grammars missing?** Run `M-x treesit-install-language-grammar`.
