---
name: dotfile-backup
description: Back up tracked dotfiles from live environment to ~/dotfiles/. Use when the user says "back up dotfiles", "sync dotfiles", "save my config", or after making config changes they want to preserve.
---

Back up dotfiles from live environment to ~/dotfiles/ using the MANIFEST.

## Procedure

1. Read `~/dotfiles/MANIFEST` to get the list of tracked paths (skip comments and blank lines).

2. For each path in the MANIFEST:
   a. **Source missing or empty**: SKIP and warn the user. NEVER copy an empty or missing source over an existing backup.
   b. **Identical**: Skip silently.
   c. **New or different**: Copy using `cp -r`.

3. Run `cd ~/dotfiles && git status` to show what changed.

4. Commit all changes: `cd ~/dotfiles && git add -A && git commit` with a descriptive message summarizing what changed.

5. Push to remote: `cd ~/dotfiles && git push -u origin HEAD`.

## Safety rules

- NEVER overwrite a backup with empty content or a missing source
- The MANIFEST is the single source of truth for what is tracked
