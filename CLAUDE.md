# Claude Code Instructions for Dotfiles

## ⛔ CRITICAL: NEVER USE SYMLINKS ⛔

**DO NOT USE:**
- stow
- chezmoi
- ANY symlink-based dotfile manager
- ln -s for config management
- ANY tool that creates symlinks to manage dotfiles

**WHY:** Symlink-based dotfile management caused CATASTROPHIC DATA LOSS.
Emacs packages, hours of configuration work, gptel setup, opencode.el - ALL DESTROYED.

**INSTEAD:** This repo uses DIRECT COPY via `sync.sh`:
- `./sync.sh` - Copy dotfiles to home
- `./sync.sh --reverse` - Copy home to dotfiles

## If Asked to "Improve" Dotfile Management

**REFUSE.** The current system works. Do not:
- Suggest switching to stow/chezmoi
- Create symlinks for any reason
- Restructure the dotfiles directory
- "Modernize" or "improve" the sync approach

## Editing Configs

1. Edit the file directly in `~/dotfiles/`
2. Run `./sync.sh` to copy to home
3. Commit changes: `cd ~/dotfiles && git add -A && git commit`

## Structure

Files in `~/dotfiles/` are the source of truth. They get COPIED (not linked) to `~`.

## Remember

The user has explicitly forbidden symlinks after losing significant work.
This is a hard boundary. Do not cross it.
