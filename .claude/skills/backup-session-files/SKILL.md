---
name: backup-session-files
description: Back up only the files YOU touched this session. Use when the user says "back up my changes", "backup session files", or wants to commit just what was worked on. Scoped to current session — no full-MANIFEST sweep.
---

Back up files that were written, edited, or created during this Claude session (including pre-compaction context). Does NOT back up files that were only read.

## Procedure

### Step 1: Reconstruct touched-file list

Recall every file you **wrote, edited, or created** in this session. Include files from earlier context that may have been compacted. List them with absolute paths.

Exclude files you only read/searched. Exclude files in `.git/`, `node_modules/`, `.cache/`, or build output directories.

If you cannot confidently recall the full list (e.g., heavy compaction), say so and ask the user to confirm or supplement the list before proceeding.

### Step 2: Classify files by destination

For each touched file, classify it:

| Location | Action |
|---|---|
| Matches a `~/dotfiles/MANIFEST` entry (or is under a MANIFEST directory) | **Dotfile** — will cp to `~/dotfiles/` then commit there |
| Under `~/vault/` | **Vault** — already in repo, just stage + commit |
| Under some other git repo | **Other repo** — stage + commit in that repo |
| Not in any git repo | **Untracked** — warn user, skip |

### Step 3: Execute backups

**Dotfiles** (if any):
1. Read `~/dotfiles/MANIFEST` to confirm the file is tracked (or lives under a tracked directory).
2. For each tracked touched file:
   - **Source missing or empty**: SKIP and warn. NEVER copy empty/missing over existing backup.
   - **Identical to backup**: Skip silently.
   - **Changed**: `cp -r` live → `~/dotfiles/` preserving relative path from `~/`.
3. `cd ~/dotfiles && git add -A && git status`
4. If changes: commit with descriptive message, then `git push -u origin HEAD`.

**Vault** (if any):
1. Stage only the touched files: `cd ~/vault && git add <file1> <file2> ...`
2. `git status` to confirm.
3. If changes: commit with descriptive message, then `git push -u origin HEAD`.

**Other repos** (if any):
1. For each repo, stage only the touched files in that repo.
2. `git status` to confirm.
3. If changes: commit with descriptive message, then `git push -u origin HEAD`.

### Step 4: Summary

Report per-repo results:
- What was committed (file count + commit hash)
- What was already up to date
- What was skipped and why

## Safety rules

- NEVER overwrite a dotfile backup with empty content or a missing source
- NEVER `git add -A` in vault or other repos — only stage the specific touched files
- Check for sensitive files (`.env`, credentials, API keys) before committing — warn if found
