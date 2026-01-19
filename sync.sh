#!/bin/bash
# Dotfiles sync script - DIRECT COPY ONLY
#
# ⛔ WARNING: NEVER USE SYMLINKS ⛔
# Do NOT use stow, chezmoi, or any symlink-based tool.
# Symlinks DESTROYED configs and caused catastrophic data loss.
#
# Usage:
#   ./sync.sh           # Copy dotfiles -> home
#   ./sync.sh --reverse # Copy home -> dotfiles (backup)

set -e

DOTFILES="$HOME/dotfiles"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"

# Files/dirs to sync (relative to home)
CONFIGS=(
    .bashrc
    .zshrc
    .zshenv
    .xinitrc
    .XCompose
    .emacs.d
    .claude
    .tmuxifier
    .config/i3
    .config/polybar
    .config/rofi
    .config/picom
    .config/dunst
    .config/nvim
    .config/ghostty
    .config/alacritty
    .config/wezterm
    .config/fish
    .config/tmux
    .config/yazi
    .config/starship.toml
    .config/wallpaper
    .config/readline
    .config/systemd/user
    .local/share/icons
    bin
)

sync_to_home() {
    echo "Syncing dotfiles -> home..."

    for item in "${CONFIGS[@]}"; do
        src="$DOTFILES/$item"
        dest="$HOME/$item"

        if [ -e "$src" ]; then
            # Backup existing if different
            if [ -e "$dest" ] && ! diff -rq "$src" "$dest" &>/dev/null; then
                mkdir -p "$BACKUP_DIR/$(dirname "$item")"
                cp -r "$dest" "$BACKUP_DIR/$item"
                echo "  Backed up: $item"
            fi

            # Ensure parent dir exists
            mkdir -p "$(dirname "$dest")"

            # Copy (overwrite)
            rm -rf "$dest"
            cp -r "$src" "$dest"
            echo "  Synced: $item"
        fi
    done

    echo "Done. Backup at: $BACKUP_DIR"
}

sync_to_dotfiles() {
    echo "Syncing home -> dotfiles (reverse backup)..."

    for item in "${CONFIGS[@]}"; do
        src="$HOME/$item"
        dest="$DOTFILES/$item"

        if [ -e "$src" ]; then
            mkdir -p "$(dirname "$dest")"
            rm -rf "$dest"
            cp -r "$src" "$dest"
            echo "  Copied: $item"
        fi
    done

    echo "Done. Now commit changes in $DOTFILES"
}

case "${1:-}" in
    --reverse|-r)
        sync_to_dotfiles
        ;;
    --help|-h)
        echo "Usage: $0 [--reverse]"
        echo "  (no args)  Copy dotfiles -> home"
        echo "  --reverse  Copy home -> dotfiles"
        ;;
    *)
        sync_to_home
        ;;
esac
