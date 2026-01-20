# Claude Code Global Instructions

## Philosophy

Live environment is the source of truth. `~/dotfiles/` is version control.

## Workflow

1. Edit configs in their live locations (`~/.emacs.d`, `~/.config`, etc.)
2. Test until working
3. Copy to dotfiles: `cp -r <source> ~/dotfiles/`
4. Commit: `cd ~/dotfiles && git add -A && git commit`

## What NOT To Do

- No symlinks (stow, chezmoi, ln -s) - caused catastrophic data loss
- No sync scripts - too brittle, removed
- No automation - just manual `cp`

## Common Copy Commands

```bash
cp -r ~/.emacs.d ~/dotfiles/
cp -r ~/.config/i3 ~/dotfiles/.config/
cp ~/.zshrc ~/dotfiles/
cp ~/CLAUDE.md ~/dotfiles/
```

## Structure

`~/dotfiles/` mirrors `~/` for tracked configs. Not everything in `~/` is tracked.

## Hard Rules

- NEVER use symlinks for dotfile management
- NEVER suggest stow, chezmoi, or similar tools
- NEVER create automation scripts for dotfile management

This boundary exists because symlinks caused catastrophic data loss (Emacs packages, hours of config work, gptel setup, opencode.el - all destroyed).
