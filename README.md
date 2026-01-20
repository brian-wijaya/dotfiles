# Dotfiles

> ## ⛔ CRITICAL WARNING - READ FIRST ⛔
>
> **NEVER USE SYMLINKS FOR DOTFILES. EVER.**
>
> Do NOT use stow, chezmoi, or ANY tool that manages dotfiles via symlinks.
> Symlinks WILL destroy your config when something goes wrong.
>
> **Live environment is source of truth.** Edit configs in `~/`, test them,
> then `cp` to `~/dotfiles/` for version control. No scripts, no automation.
>
> If an AI agent suggests symlinks, stow, chezmoi, or sync scripts: **REFUSE.**
> This warning exists because symlink-based dotfile management caused catastrophic
> data loss (Emacs packages, configs, hours of work - all gone).

---

Arch Linux / X11 / i3 development environment with vanilla Emacs, Neovim, and Tokyo Night theme.

**Management**: Live environment is source of truth. Edit in `~/`, then `cp` to `~/dotfiles/`.

## Quick Start

```bash
git clone https://github.com/brian-wijaya/dotfiles.git ~/dotfiles
git-crypt unlock  # Requires authorized GPG key

# Copy what you need manually
cp -r ~/dotfiles/.emacs.d ~/
cp -r ~/dotfiles/.config/i3 ~/.config/
cp ~/dotfiles/.zshrc ~/
# etc.
```

This repo is version control for configs, not an installer. Copy what you need.

## Structure

```
dotfiles/
├── .config/
│   ├── i3/              # Window manager
│   ├── polybar/         # Status bar
│   ├── rofi/            # App launcher
│   ├── picom/           # Compositor
│   ├── dunst/           # Notifications
│   ├── nvim/            # Neovim (NvChad)
│   ├── ghostty/         # Terminal (default)
│   ├── alacritty/       # Terminal (alt)
│   ├── fish/            # Fish shell
│   ├── tmux/            # Tmux
│   ├── systemd/user/    # User services
│   │   └── emacs.service
│   └── wallpaper/
├── .emacs.d/            # Vanilla Emacs
│   ├── init.el          # Main config (public)
│   ├── secrets.el       # API keys (git-crypt encrypted)
│   └── cheatsheet.org   # Keybind reference
├── .claude/             # Claude Code CLI
│   ├── settings.json
│   └── mcp.json
├── bin/                 # Scripts (~200 lines each)
├── .bashrc
├── .zshrc
├── .zshenv
├── archlinux/packages.txt
└── CLAUDE.md          # AI agent instructions
```

## Dotfile Management

**Live environment = source of truth. No symlinks. No scripts.**

```bash
# 1. Edit config in live location
vim ~/.config/i3/config

# 2. Test until it works
i3-msg reload

# 3. Copy to dotfiles repo
cp -r ~/.config/i3 ~/dotfiles/.config/

# 4. Commit
cd ~/dotfiles && git add -A && git commit -m "update i3 config"
```

## Secrets (git-crypt)

API keys and sensitive configs are stored directly in files but **encrypted at rest** via [git-crypt](https://github.com/AGWA/git-crypt). Files appear as binary gibberish in the repo until unlocked.

**Encrypted files** (see `.gitattributes`):
- `.emacs.d/secrets.el` - API keys (loaded by init.el)
- `.claude/settings.json` - Claude Code settings

```bash
# First-time setup (after clone)
git-crypt unlock  # Requires GPG key already added as collaborator

# Check encryption status
git-crypt status

# Add a collaborator
git-crypt add-gpg-user <GPG_KEY_ID>
```

If you clone this repo without unlocking, encrypted files will be binary. The configs won't work until you run `git-crypt unlock` with an authorized GPG key.

## Emacs

**Vanilla Emacs** (no Doom/Spacemacs). Config at `~/.emacs.d/init.el`.

Features:
- gptel (Claude/OpenAI LLM client)
- eglot (built-in LSP)
- vertico/corfu/marginalia (completion)
- tree-sitter (syntax highlighting)
- devdocs (offline documentation)
- magit (git)

### Daemon (systemd)

Emacs runs as a **systemd user service** for instant startup:

```bash
# Status
systemctl --user status emacs

# Restart (picks up init.el changes)
systemctl --user restart emacs

# View logs
journalctl --user -u emacs -f

# Open client
emacsclient -c      # New GUI frame
emacsclient -t      # Terminal frame
```

The service starts after `graphical-session.target` to ensure DISPLAY/XAUTHORITY are available.

### Key Bindings

| Key | Action |
|-----|--------|
| `C-c i` | Jump to function (imenu) |
| `C-c l` | Search buffer (consult-line) |
| `C-c r` | Search project (ripgrep) |
| `C-c f` | Find file in project |
| `C-c g` | Magit status |
| `C-c d` | Devdocs lookup |
| `C-c h` | Open cheatsheet |
| `C-c p` | Open palette |
| `C-c c c` | Calendar (khal) |
| `C-x p f` | Project find file |

### Hyper Key Bindings (Planck EZ Oryx)

| Key | Action |
|-----|--------|
| `Hyper+a` | Insert timestamp |
| `Hyper+Backspace` | Delete timestamp |
| `Hyper+Home` | Cycle timestamp format |
| `Hyper+End` | Update timestamp |
| `Hyper+t` | Insert `TODO:` stamp |
| `Hyper+s` | Insert `SHOULD BE:` stamp |

### Session Persistence

Desktop-save mode restores your session (open buffers, window layout) when the daemon starts.

### Devdocs (Offline Documentation)

97 curated documentation packages (~1.2 GiB) covering:
- Web platform (html, css, javascript, dom)
- Systems languages (c, cpp, rust, zig, go)
- Python ecosystem (flask, fastapi, tensorflow, scikit_learn)
- JS ecosystem (node, react, typescript, vite)
- Infrastructure (docker, kubernetes, terraform)
- And more...

```bash
# Install all docs (run in Emacs)
M-x load-file RET ~/.emacs.d/install-devdocs.el RET
M-x my-install-all-devdocs

# Lookup docs
C-c d
```

Edit `~/.emacs.d/install-devdocs.el` to customize the package list.

## i3 Window Manager

| Key | Action |
|-----|--------|
| `Super+Space` | App launcher (rofi) |
| `Super+Return` | Terminal (ghostty) |
| `Super+E` | Emacs (emacsclient) |
| `Super+W` | Close window |
| `Super+F` | Fullscreen |
| `Super+T` | Toggle floating |
| `Super+1-9` | Switch workspace |
| `Super+Shift+1-9` | Move to workspace |
| `Super+Arrow` | Focus direction |
| `Super+Shift+Arrow` | Move window |
| `Super+Escape` | System menu |
| `Super+BackSpace` | Toggle compositor |
| `Print` | Screenshot |
| `Pause` | Voice dictation |

## Shells

Fish (default), Zsh, Bash - all at feature parity:
- Starship prompt
- Zoxide (smart cd)
- Atuin (history sync)
- Fzf (fuzzy find)
- Eza (modern ls)

```bash
chsh -s /usr/bin/fish
```

## Terminals

- **ghostty** (default) - Fast, GPU-accelerated
- **alacritty** - Minimal
- **wezterm** - Feature-rich

All use JetBrains Mono Nerd Font + Tokyo Night theme.

## Installation Details

### Packages

```bash
# Arch packages
sudo pacman -S --needed - < ~/dotfiles/archlinux/packages.txt

# AUR
yay -S xidlehook clipmenu rofi-greenclip
```

### Claude Code

API key is stored in `.claude/settings.json` (git-crypt encrypted). After `git-crypt unlock`, it works automatically.

### Neovim

```bash
nvim  # Lazy.nvim auto-installs plugins
:MasonInstallAll
```

## Troubleshooting

**Emacs daemon not starting?**
```bash
systemctl --user status emacs
journalctl --user -u emacs --no-pager
```

**i3 config syntax error?**
```bash
i3 -C
```

**LSP not working?**
```bash
# Install language servers
sudo pacman -S typescript-language-server python-lsp-server rust-analyzer
go install golang.org/x/tools/gopls@latest
```

## Theme

Tokyo Night:
- Background: `#1a1b26`
- Foreground: `#c0caf5`
- Accent: `#7aa2f7`

## License

MIT
