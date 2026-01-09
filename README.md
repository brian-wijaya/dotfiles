# Dotfiles

Arch Linux / X11 / i3 development environment with Doom Emacs, Neovim, and Tokyo Night theme.

## What's Included

```
dotfiles/
├── .config/
│   ├── i3/              # i3 window manager
│   ├── polybar/         # Status bar
│   ├── rofi/            # App launcher
│   ├── picom/           # Compositor (transparency, animations)
│   ├── dunst/           # Notifications
│   ├── nvim/            # Neovim (NvChad)
│   ├── doom/            # Doom Emacs user config
│   ├── tmux/            # tmux config
│   ├── fish/            # Fish shell
│   ├── alacritty/       # Terminal (reference config)
│   ├── ghostty/         # Terminal (default)
│   ├── wezterm/         # Terminal
│   └── wallpaper/       # Wallpapers
├── bin/                 # Custom scripts
├── .bashrc              # Bash config
├── .zshrc               # Zsh config
├── .zshenv              # Zsh environment
├── archlinux/
│   └── packages.txt     # Package list
└── install.sh           # Setup script
```

## Quick Start

```bash
# Clone dotfiles
git clone https://github.com/YOUR_USERNAME/dotfiles.git ~/dotfiles

# Run install script
cd ~/dotfiles
./install.sh
```

The install script will:
- Backup existing configs to `~/.config_backup/`
- Create symlinks for all configs
- Install Doom Emacs if not present
- Set up Neovim plugins

## Manual Installation

### 1. Install Packages

```bash
# Core packages
sudo pacman -S --needed - < ~/dotfiles/archlinux/packages.txt

# AUR packages (install yay first)
yay -S xidlehook i3lock-color clipmenu satty impala bluetui pulsemixer
```

### 2. Link Configs

```bash
# Create symlinks (or copy)
ln -sf ~/dotfiles/.config/* ~/.config/
ln -sf ~/dotfiles/.bashrc ~/.bashrc
ln -sf ~/dotfiles/.zshrc ~/.zshrc
ln -sf ~/dotfiles/.zshenv ~/.zshenv
ln -sf ~/dotfiles/bin ~/bin
```

### 3. Install Doom Emacs

```bash
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# Add to PATH (fish)
fish_add_path ~/.config/emacs/bin
```

### 4. Initialize Neovim

```bash
nvim  # Lazy.nvim will install plugins automatically
:MasonInstallAll
```

### 5. Set Default Shell

```bash
chsh -s /usr/bin/fish  # or zsh
```

## Key Bindings

### i3 Window Manager

| Key | Action |
|-----|--------|
| `Super` | Modifier key |
| `Super+Space` | App launcher (rofi) |
| `Super+Return` | Terminal (ghostty) |
| `Super+E` | Emacs |
| `Super+W` | Close window |
| `Super+F` | Fullscreen |
| `Super+T` | Toggle floating |
| `Super+1-9` | Switch workspace |
| `Super+Shift+1-9` | Move to workspace |
| `Super+Arrow` | Focus direction |
| `Super+Shift+Arrow` | Move window |
| `Super+K` | Show keybindings |

### Applications

| Key | Action |
|-----|--------|
| `Super+Shift+B` | Browser (Chromium) |
| `Super+Shift+F` | File manager (Nautilus) |
| `Super+Shift+N` | Neovim in terminal |
| `Super+Shift+T` | btop (system monitor) |
| `Super+Shift+M` | Spotify |
| `Super+Shift+O` | Obsidian |

### System

| Key | Action |
|-----|--------|
| `Super+Escape` | System menu |
| `Super+Ctrl+L` | Lock screen |
| `Super+M` | Logout |
| `Super+Shift+R` | Restart i3 |
| `Print` | Screenshot |
| `Super+Ctrl+V` | Clipboard history |

### tmux (prefix: `Ctrl+Space`)

| Key | Action |
|-----|--------|
| `prefix + c` | New window |
| `prefix + \|` | Split vertical |
| `prefix + -` | Split horizontal |
| `prefix + h/j/k/l` | Navigate panes |
| `prefix + x` | Kill pane |

### Doom Emacs

| Key | Action |
|-----|--------|
| `SPC` | Leader key |
| `SPC f f` | Find file |
| `SPC b b` | Switch buffer |
| `SPC s p` | Search in project |
| `SPC g g` | Magit (git) |
| `SPC c d` | Jump to definition |
| `SPC q q` | Quit |

## Terminals

Three terminal configs are included, all with matching Tokyo Night theme:

- **ghostty** (default) - Fast, GPU-accelerated
- **alacritty** - Reference config, minimal
- **wezterm** - Feature-rich, Lua config

Change default in `~/.config/i3/config`:
```
set $term ghostty
```

## Shells

Three shell configs at feature parity:

- **fish** - Default, modern syntax
- **zsh** - Bash-compatible, plugin ecosystem
- **bash** - Fallback, always available

All include: starship prompt, zoxide (cd), atuin (history), fzf (fuzzy find), eza (ls), tmux auto-start.

## Emacs Daemon

Emacs starts as a daemon automatically (via i3 autostart). `Super+E` opens an instant emacsclient.

```bash
# Manual control
emacs --daemon          # Start daemon
emacsclient -c          # Open client
emacsclient -e '(kill-emacs)'  # Stop daemon
```

## Updating

```bash
# Update Doom packages
doom upgrade

# After changing init.el
doom sync

# After changing i3/polybar config
Super+Shift+R  # Restart i3
```

## Troubleshooting

**Fonts not rendering?**
```bash
sudo pacman -S ttf-jetbrains-mono-nerd
fc-cache -fv
```

**Emacs not starting?**
```bash
doom doctor
doom sync
```

**i3 not loading?**
```bash
# Check for config errors
i3 -C
```

**LSP not working in Neovim/Emacs?**
```bash
# Install language servers
sudo pacman -S typescript-language-server python-lsp-server clang
go install golang.org/x/tools/gopls@latest
rustup component add rust-analyzer
```

## Theme

Tokyo Night color scheme throughout:
- Background: `#1a1b26`
- Foreground: `#c0caf5`
- Accent: `#33ccff`

## License

MIT
