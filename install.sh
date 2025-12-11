#!/bin/bash
# Dotfiles installer for Arch Linux WSL

set -e

echo "=== Dotfiles Installer ==="

# Install Go if not present (required for gopls LSP server)
echo "Checking for Go installation..."
if ! command -v go >/dev/null 2>&1; then
  echo "Go not found. Installing Go..."
  
  # Try to detect the environment
  if command -v pacman >/dev/null 2>&1; then
    # Arch Linux - use pacman
    echo "Detected Arch Linux. Installing Go via pacman..."
    sudo pacman -S --noconfirm go
  elif command -v apt >/dev/null 2>&1; then
    # Debian/Ubuntu - use apt
    echo "Detected Debian/Ubuntu. Installing Go via apt..."
    sudo apt update
    sudo apt install -y golang-go
  else
    # Fallback: download Go binary directly
    echo "No package manager found. Downloading Go binary..."
    GO_VERSION="1.23.4"
    GO_ARCH="amd64"
    GO_OS="linux"
    
    if [[ "$(uname -m)" == "aarch64" ]] || [[ "$(uname -m)" == "arm64" ]]; then
      GO_ARCH="arm64"
    fi
    
    GO_TAR="go${GO_VERSION}.${GO_OS}-${GO_ARCH}.tar.gz"
    GO_URL="https://go.dev/dl/${GO_TAR}"
    INSTALL_DIR="$HOME/.local"
    
    echo "Downloading Go ${GO_VERSION}..."
    cd /tmp
    curl -L -o "$GO_TAR" "$GO_URL"
    
    echo "Extracting Go..."
    rm -rf "$INSTALL_DIR/go"
    tar -C "$INSTALL_DIR" -xzf "$GO_TAR"
    
    # Add Go to PATH in .bashrc if not already there
    if ! grep -q "$INSTALL_DIR/go/bin" ~/.bashrc 2>/dev/null; then
      echo "" >> ~/.bashrc
      echo "# Go (for gopls LSP server)" >> ~/.bashrc
      echo "export PATH=\"$INSTALL_DIR/go/bin:\$PATH\"" >> ~/.bashrc
    fi
    
    export PATH="$INSTALL_DIR/go/bin:$PATH"
    echo "Go installed to $INSTALL_DIR/go"
  fi
  
  # Verify installation
  if command -v go >/dev/null 2>&1; then
    echo "✓ Go installed successfully: $(go version)"
  else
    echo "⚠ Warning: Go installation may have failed. Please install manually."
    echo "  Arch: sudo pacman -S go"
    echo "  Debian/Ubuntu: sudo apt install golang-go"
  fi
else
  echo "✓ Go already installed: $(go version)"
fi

# Backup existing configs
echo ""
echo "Backing up existing configs..."
mkdir -p ~/.config_backup
[ -f ~/.bashrc ] && mv ~/.bashrc ~/.config_backup/
[ -f ~/.tmux.conf ] && mv ~/.tmux.conf ~/.config_backup/
[ -f ~/.wezterm.lua ] && mv ~/.wezterm.lua ~/.config_backup/
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config_backup/
[ -d ~/.config/tmux ] && mv ~/.config/tmux ~/.config_backup/
[ -d ~/.config/wezterm ] && mv ~/.config/wezterm ~/.config_backup/

# Create symlinks
echo "Creating symlinks..."
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

# Bash config (must be in home directory)
ln -sf "$DOTFILES_DIR/.bashrc" ~/.bashrc

# Create .config directory structure
mkdir -p ~/.config

# Neovim config (XDG standard)
ln -sf "$DOTFILES_DIR/.config/nvim" ~/.config/nvim

# Tmux config (supports XDG_CONFIG_HOME)
mkdir -p ~/.config/tmux
ln -sf "$DOTFILES_DIR/.config/tmux/tmux.conf" ~/.config/tmux/tmux.conf

# WezTerm config (supports XDG_CONFIG_HOME)
mkdir -p ~/.config/wezterm
ln -sf "$DOTFILES_DIR/.config/wezterm/wezterm.lua" ~/.config/wezterm/wezterm.lua

echo "Symlinks created:"
echo "  ~/.bashrc -> $DOTFILES_DIR/.bashrc"
echo "  ~/.config/nvim -> $DOTFILES_DIR/.config/nvim"
echo "  ~/.config/tmux/tmux.conf -> $DOTFILES_DIR/.config/tmux/tmux.conf"
echo "  ~/.config/wezterm/wezterm.lua -> $DOTFILES_DIR/.config/wezterm/wezterm.lua"

# Copy WezTerm config to Windows home directory (WezTerm on Windows looks there, not in WSL)
echo ""
echo "Copying WezTerm config to Windows home directory..."
# Try to detect Windows username - common usernames to try
WIN_USER=""
for user in wijay $(whoami 2>/dev/null) $(id -un 2>/dev/null); do
  if [ -d "/mnt/c/Users/$user" ]; then
    WIN_USER="$user"
    break
  fi
done

if [ -n "$WIN_USER" ] && [ -d "/mnt/c/Users/$WIN_USER" ]; then
  if cp "$DOTFILES_DIR/.config/wezterm/wezterm.lua" "/mnt/c/Users/$WIN_USER/.wezterm.lua" 2>/dev/null; then
    echo "✓ WezTerm config copied to /mnt/c/Users/$WIN_USER/.wezterm.lua"
  else
    echo "⚠ Could not copy WezTerm config automatically (permission denied?)"
    echo "   Please copy manually:"
    echo "   cp $DOTFILES_DIR/.config/wezterm/wezterm.lua /mnt/c/Users/$WIN_USER/.wezterm.lua"
  fi
else
  echo "⚠ Could not detect Windows username. Please copy WezTerm config manually:"
  echo "   cp $DOTFILES_DIR/.config/wezterm/wezterm.lua /mnt/c/Users/YOUR_USERNAME/.wezterm.lua"
fi

# Check and install Nerd Fonts on Windows (required for WezTerm icons)
echo ""
echo "Checking and installing Nerd Fonts on Windows..."
if [ -n "$WIN_USER" ] && [ -d "/mnt/c/Users/$WIN_USER" ]; then
  # Check if primary font (Cascadia Code NF) is installed
  CASCADIA_INSTALLED=false
  if [ -f "/mnt/c/Users/$WIN_USER/AppData/Local/Microsoft/Windows/Fonts/CascadiaCodeNF-Regular.ttf" ] || \
     [ -f "/mnt/c/Windows/Fonts/CascadiaCodeNF-Regular.ttf" ]; then
    CASCADIA_INSTALLED=true
    echo "✓ Cascadia Code NF already installed"
  fi
  
  # Always run the font installer to ensure all fonts are installed
  if command -v powershell.exe >/dev/null 2>&1; then
    # Convert WSL path to Windows path for PowerShell
    PS_SCRIPT_PATH=$(wslpath -w "$DOTFILES_DIR/install-fonts.ps1" 2>/dev/null)
    
    if [ -z "$PS_SCRIPT_PATH" ]; then
      # Fallback if wslpath doesn't work
      PS_SCRIPT_PATH=$(echo "$DOTFILES_DIR/install-fonts.ps1" | sed 's|/mnt/c|C:|' | sed 's|/|\\|g')
    fi
    
    echo "Installing all Nerd Fonts (CascadiaCode, Hack, FiraCode, JetBrainsMono)..."
    if powershell.exe -ExecutionPolicy Bypass -File "$PS_SCRIPT_PATH" 2>&1; then
      echo "✓ Nerd Fonts installation completed"
    else
      echo "⚠ PowerShell script had issues. Fonts may still be installing..."
      echo "   Check Windows Settings → Fonts to verify"
    fi
  else
    echo "⚠ PowerShell not available. Please install fonts manually:"
    echo "   powershell -ExecutionPolicy Bypass -File $DOTFILES_DIR/install-fonts.ps1"
  fi
else
  echo "⚠ Could not detect Windows user. Please install fonts manually:"
  echo "   powershell -ExecutionPolicy Bypass -File $DOTFILES_DIR/install-fonts.ps1"
fi

echo ""
echo "=== Next Steps ==="
echo "1. Restart WezTerm completely (close all windows) to apply font changes"
echo "2. Launch nvim and wait for plugins to install"
echo "3. Run :MasonInstallAll in Neovim"
echo ""
echo "Done!"
