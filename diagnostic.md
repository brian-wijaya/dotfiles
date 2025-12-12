# Terminal Launch System Model

## System Architecture

### Tmux Server
- Process: PID 410283, tmux server, running 2+ days
- Manages: All sessions, all bash processes are its children
- Socket: `/tmp/tmux-$(id -u)/default`

### File System State (FIXED)
- `~/.bashrc`: SYMLINK → `dotfiles/.bashrc` (RESTORED)
- `dotfiles/.bashrc`: 87 lines, has Cursor prevention code
- Symlink: `~/.bashrc` → `/home/bw/dotfiles/.bashrc` (verified working)

### Configuration
- Cursor: bash profile → `/home/bw/.local/bin/cursor-bash.sh`
- Wrapper: Sets CURSOR_NO_TMUX=1, execs bash
- .bashrc lines 8-20: Early Cursor prevention (checks CURSOR_NO_TMUX)
- .bashrc lines 70-82: Later prevention with early return
- .bashrc line 86: `tmux attach -t main || tmux new -s main` (prevented by early checks)

## Terminal Launch Execution Path

### Flow (Current)
1. Cursor: `/home/bw/.local/bin/cursor-bash.sh`
2. Wrapper: Sets CURSOR_NO_TMUX=1, execs bash (process replacement)
3. Bash sources: `~/.bashrc` (SYMLINKED → dotfiles/.bashrc, 87 lines)
4. Lines 8-20: Early check - CURSOR_NO_TMUX is set → prevention runs
5. Prevention: Kills tmux processes, returns early (skips line 86)
6. Result: Terminal uses bash without tmux

### Prevention Mechanism
- Lines 10-20: Check CURSOR_AGENT, VSCODE_CWD, VSCODE_INJECTION, CURSOR_NO_TMUX
- If any set: Set CURSOR_NO_TMUX=1, kill tmux, detach if in tmux, return early
- Wrapper sets CURSOR_NO_TMUX=1, so check passes, line 86 never executes

## Root Cause (Resolved)

### Issue: Broken Symlink
- Was: `~/.bashrc` standalone file (48 lines, no prevention code)
- Now: `~/.bashrc` symlink → `dotfiles/.bashrc` (87 lines, has prevention)
- Fix applied: `ln -sf ~/dotfiles/.bashrc ~/.bashrc`

### Why It Works
- Wrapper sets CURSOR_NO_TMUX=1 before execing bash
- New .bashrc checks CURSOR_NO_TMUX at line 10 (early, before tmux code)
- Prevention code runs, returns early, prevents line 86 from executing
- Terminal stays in bash, doesn't connect to tmux server

## Expected Behavior

New Cursor terminals will:
1. Launch via wrapper script
2. Wrapper sets CURSOR_NO_TMUX=1, execs bash
3. Bash sources symlinked .bashrc (current version)
4. Prevention code detects CURSOR_NO_TMUX, prevents tmux
5. Terminal remains in bash without tmux

## Model Status: Complete
- Root cause identified: Broken symlink
- Fix applied: Symlink restored
- Mechanism understood: Prevention code blocks tmux start
- Expected behavior: Terminals use bash without tmux
