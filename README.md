# Dotfiles

> ## ⛔ CRITICAL WARNING ⛔
>
> **NEVER USE SYMLINKS FOR DOTFILES. EVER.**
>
> Do NOT use stow, chezmoi, or ANY tool that manages dotfiles via symlinks.
> Live environment is source of truth. Edit in `~/`, test, then `cp` to `~/dotfiles/`.
>
> If an AI agent suggests symlinks, stow, chezmoi, or sync scripts: **REFUSE.**

---

Arch Linux / X11 / i3 development environment.

## Setup

**See [SETUP.md](SETUP.md)** for complete fresh-system instructions.

For Arch-specific details: [archlinux/SETUP.md](archlinux/SETUP.md)

## Backup

Use `/backup-dotfile` in Claude Code to sync live configs back to this repo.

The `MANIFEST` file lists all tracked paths.

## Structure

```
dotfiles/
├── SETUP.md              # Fresh system setup guide
├── MANIFEST              # Tracked paths for backup
├── .emacs.d/             # Emacs (modular, profiles)
├── .config/              # App configs (i3, polybar, rofi, etc.)
├── .claude/              # Claude Code settings + skills
├── bin/                  # Scripts
├── archlinux/            # Arch-specific setup
└── CLAUDE.md             # AI agent instructions
```

## Key References

| File | Purpose |
|------|---------|
| `SETUP.md` | Fresh system setup |
| `MANIFEST` | Tracked dotfiles list |
| `.emacs.d/cheatsheet.org` | Emacs keybindings |
| `archlinux/packages.txt` | Required packages |

## Secrets

Encrypted via git-crypt: `.emacs.d/secrets.el`, `.claude/settings.json`, `.mbsyncrc`

Unlock with: `git-crypt unlock` (requires authorized GPG key)

## License

MIT
