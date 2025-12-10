# Dotfiles

Personal development environment for WSL Arch Linux + WezTerm + NvChad.

## What's Included

```
dotfiles/
├── .bashrc              # Shell config (zoxide, fzf, tmux auto-start)
├── .tmux.conf           # tmux config (Ctrl-Space prefix, mouse, vim keys)
├── .wezterm.lua         # WezTerm config (WSL, keybindings, theme)
├── .config/
│   └── nvim/            # NvChad configuration
├── packages.txt         # Arch Linux package list
└── README.md
```

## Prerequisites

- Windows 10/11 with WSL2
- [WezTerm](https://wezfurlong.org/wezterm/installation.html) installed on Windows

## Fresh Install Guide

### 1. Install Arch Linux on WSL

```powershell
# In PowerShell (Admin)
wsl --install -d archlinux
```

### 2. Initial Arch Setup

```bash
# Update system
sudo pacman -Syu

# Install yay (AUR helper)
sudo pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay && makepkg -si
cd .. && rm -rf yay
```

### 3. Install Packages

```bash
# Core packages (essential)
sudo pacman -S neovim tmux fzf ripgrep zoxide git github-cli nodejs npm python-pynvim xclip unzip wget curl

# Nerd Fonts (for terminal icons)
sudo pacman -S ttf-jetbrains-mono-nerd ttf-firacode-nerd ttf-cascadia-code-nerd

# Optional but recommended
sudo pacman -S lazygit bat eza fd htop btop tree
```

Or install everything from the package list:
```bash
# Install all packages (includes many nerd fonts)
sudo pacman -S --needed $(cat packages.txt | awk '{print $1}')
```

### 4. Clone and Apply Dotfiles

```bash
cd ~
git clone https://github.com/retopo001/dotfiles.git

# Backup existing configs
mkdir -p ~/.config_backup
mv ~/.bashrc ~/.config_backup/ 2>/dev/null
mv ~/.tmux.conf ~/.config_backup/ 2>/dev/null
mv ~/.config/nvim ~/.config_backup/ 2>/dev/null

# Create symlinks
ln -sf ~/dotfiles/.bashrc ~/.bashrc
ln -sf ~/dotfiles/.tmux.conf ~/.tmux.conf
ln -sf ~/dotfiles/.config/nvim ~/.config/nvim
```

### 5. WezTerm Config (Windows Side)

Copy `.wezterm.lua` to your Windows home directory:

```bash
cp ~/dotfiles/.wezterm.lua /mnt/c/Users/YOUR_USERNAME/.wezterm.lua
```

Or from PowerShell:
```powershell
Copy-Item "\\wsl$\archlinux\home\YOUR_WSL_USER\dotfiles\.wezterm.lua" "$HOME\.wezterm.lua"
```

### 6. Initialize Neovim

```bash
# First launch will install plugins
nvim

# Inside Neovim, wait for Lazy to finish, then:
:MasonInstallAll
:checkhealth
```

### 7. Authenticate GitHub CLI

```bash
gh auth login
gh auth setup-git
```

## Keybindings

### WezTerm
| Key | Action |
|-----|--------|
| `Ctrl+Shift+T` | New independent bash tab |
| `Ctrl+Shift+N` | New Neovim tab |

### tmux (prefix: `Ctrl+Space`)
| Key | Action |
|-----|--------|
| `prefix + c` | New window |
| `prefix + \|` | Split vertical |
| `prefix + -` | Split horizontal |
| `prefix + h/j/k/l` | Navigate panes |
| `prefix + x` | Kill pane |

### Neovim (NvChad)
| Key | Action |
|-----|--------|
| `Space` | Leader key |
| `Space + f + f` | Find files (Telescope) |
| `Space + f + w` | Live grep |
| `Space + t + h` | Change theme |

## Behavior

- **Open WezTerm** → Auto-starts tmux in `~/signal-assembly-platform`
- **Click + button** → Joins existing tmux session
- **Ctrl+Shift+T** → Independent bash (no tmux)
- **Ctrl+Shift+N** → Opens Neovim directly

## Customization

### Change Default Directory

Edit `.bashrc` line 21 and `.wezterm.lua` cwd paths:
```bash
cd ~/your-project-directory
```

### Change tmux Prefix

Edit `.tmux.conf` line 3:
```bash
set -g prefix C-a  # Change to Ctrl+a
```

## Troubleshooting

### Fonts not rendering
Ensure Nerd Fonts are installed and WezTerm is configured to use them.

### tmux colors wrong
Ensure your terminal supports truecolor. The config sets `tmux-256color`.

### Neovim checkhealth errors
```bash
:checkhealth
```
Install missing providers:
```bash
npm install -g neovim
pip install pynvim
```

## License

MIT
