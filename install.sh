#!/bin/bash
# Dotfiles installer for Arch Linux WSL

set -e

echo "=== Dotfiles Installer ==="

# Backup existing configs
echo "Backing up existing configs..."
mkdir -p ~/.config_backup
[ -f ~/.bashrc ] && mv ~/.bashrc ~/.config_backup/
[ -f ~/.tmux.conf ] && mv ~/.tmux.conf ~/.config_backup/
[ -d ~/.config/nvim ] && mv ~/.config/nvim ~/.config_backup/

# Create symlinks
echo "Creating symlinks..."
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

ln -sf "$DOTFILES_DIR/.bashrc" ~/.bashrc
ln -sf "$DOTFILES_DIR/.tmux.conf" ~/.tmux.conf
mkdir -p ~/.config
ln -sf "$DOTFILES_DIR/.config/nvim" ~/.config/nvim

echo "Symlinks created:"
ls -la ~/.bashrc ~/.tmux.conf ~/.config/nvim

# Remind about WezTerm
echo ""
echo "=== Manual Steps Required ==="
echo "1. Copy WezTerm config to Windows:"
echo "   cp $DOTFILES_DIR/.wezterm.lua /mnt/c/Users/YOUR_USERNAME/.wezterm.lua"
echo ""
echo "2. Launch nvim and wait for plugins to install"
echo "3. Run :MasonInstallAll in Neovim"
echo ""
echo "Done! Restart your terminal to apply changes."
