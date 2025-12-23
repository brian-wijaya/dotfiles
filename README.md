# Dotfiles

Personal development environment for WSL Arch Linux + WezTerm + NvChad.

## What's Included

```
dotfiles/
├── .bashrc                      # Shell config (zoxide, fzf, tmux auto-start)
├── .config/
│   ├── nvim/                    # NvChad configuration
│   ├── tmux/
│   │   └── tmux.conf            # tmux config (Ctrl-Space prefix, mouse, vim keys)
│   └── wezterm/
│       └── wezterm.lua          # WezTerm config (WSL, keybindings, theme)
├── install.sh                   # Automated setup script
├── install-fonts.ps1            # Windows Nerd Font installer
├── packages.txt                 # Arch Linux package list
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

### 4. Clone Dotfiles

```bash
cd ~
git clone https://github.com/retopo001/dotfiles.git
```

### 5. Run Install Script

The install script will:
- Install Go (if missing) for gopls LSP server
- Backup existing configs to `~/.config_backup/`
- Copy all configs to their runtime locations
- Copy WezTerm config to Windows home directory

```bash
cd ~/dotfiles
./install.sh
```

**Note:** The install script automatically copies the WezTerm config to your Windows home directory (`C:\Users\YOUR_USERNAME\.wezterm.lua`) because WezTerm on Windows looks there, not in WSL.

### 6. Install Nerd Fonts (Windows Side)

**Required for icons in terminal and Neovim.**

From PowerShell (run as Administrator for best results):
```powershell
# Option 1: Use the automated script
powershell -ExecutionPolicy Bypass -File "\\wsl$\archlinux\home\YOUR_WSL_USER\dotfiles\install-fonts.ps1"

# Option 2: Use winget (if available)
winget install -e --id CascadiaCode.CascadiaCode-NF
```

The script will automatically download and install multiple Nerd Fonts (CascadiaCode, Hack, FiraCode, JetBrainsMono). After installation, **restart WezTerm completely** for fonts to take effect.

**If you see font warnings:** The config tries multiple font name variations. If fonts still don't work, verify they're installed in Windows Settings → Fonts, or run the install-fonts.ps1 script again.

### 7. Initialize Neovim

```bash
# First launch will install plugins
nvim

# Inside Neovim, wait for Lazy to finish, then:
:MasonInstallAll
:checkhealth
```

### 8. Authenticate GitHub CLI

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

### Fonts not rendering / Font warnings

**If you see font warnings in WezTerm:**

1. **Verify fonts are installed:**
   ```powershell
   # Check if fonts are in Windows Fonts directory
   Get-ChildItem "$env:LOCALAPPDATA\Microsoft\Windows\Fonts" | Where-Object { $_.Name -like '*Cascadia*' }
   ```

2. **Find the exact font name:**
   ```bash
   # In WezTerm, open a new tab and run:
   wezterm ls-fonts --list-system | grep -i cascadia
   ```
   This will show the exact font name to use in the config.

3. **Update the config if needed:**
   - Edit `dotfiles/.config/wezterm/wezterm.lua`
   - Change the font name to match what `wezterm ls-fonts` shows
   - Copy updated config: `cp ~/dotfiles/.config/wezterm/wezterm.lua /mnt/c/Users/YOUR_USERNAME/.wezterm.lua`
   - Restart WezTerm

4. **Re-run font installer:**
   ```powershell
   powershell -ExecutionPolicy Bypass -File "\\wsl$\archlinux\home\YOUR_WSL_USER\dotfiles\install-fonts.ps1"
   ```

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
