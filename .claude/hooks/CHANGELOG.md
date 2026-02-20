# Changelog

All notable changes to the Claude Code hooks system will be documented in this file.

## 2026-02-20

### Added

#### claude-edit-stream (PostToolUse -> Edit, Write)
- Real-time edit streaming to Emacs via SHM IPC (`/dev/shm/claude_edit_stream`)
- JSON-line format: `{"tool":"Edit","file":"/path","old":"...","new":"...","ts":N}`
- Fork-based `emacsclient --eval` invocation with `alarm(3)` timeout
- Post-edit validation: checks `/dev/shm/x11_state` for Emacs visibility, `/dev/shm/emacs_state` for blocking prompts
- Exit code 2 interrupt mechanism -- deterministic model interruption when Emacs needs attention
- Parent exits in <2ms; child handles async notification

#### claude-edit-stream-pre (PreToolUse -> Edit, Write)
- Attention protocol: "eye contact before speaking" -- checks if user can see Emacs before edits land
- Reads `/dev/shm/x11_state` for workspace and visibility state
- If Emacs not visible: sends `notify-send` toast -> moves Emacs to user's workspace via `i3-msg` -> arranges 50/50 tiled layout
- Fallback: `xdotool search --class Emacs` when daemon reports `emacs_wksp=-1`
- Clears stale `I3SOCK`/`I3_SOCKET_PATH` env vars for reliable i3 IPC
- Always exits 0 (never blocks the edit itself)

#### i3-state-writer (persistent daemon)
- Subscribes to i3 workspace + window events via `i3-msg -t subscribe -m`
- Writes atomic state to `/dev/shm/x11_state`: `workspace=N emacs_visible=yes|no emacs_wksp=M focused_class=ClassName`
- Brace-matched JSON parsing for reliable workspace/window detection
- Safety net: if `focused_class == "Emacs"` but `emacs_visible == false`, forces visible=true
- Managed by systemd: `i3-state-writer.service` (user unit)

#### sensor-awareness (PostToolUse -> all tools)
- Extended with multi-SHM fusion: reads `/dev/shm/sensor_ambient`, `/dev/shm/emacs_state`, `/dev/shm/x11_state`
- Composite ambient header injected into every tool response

#### Emacs Integration (claude-edit-stream.el)
- Global minor mode `claude-edit-stream-mode` (lighter ` CES`)
- Cursor-style inline diff visualization: green (added, bg #1a2e1a) / red+strikethrough (removed, bg #2e1a1a)
- Blue header overlay with `[a]ccept [r]eject` controls per change
- Accept (`C-c e a`), Reject (`C-c e r`), Accept All (`C-c e A`), Reject All (`C-c e R`)
- Navigate changes: `C-c e n` / `C-c e p` (Evil: `]e` / `[e`)
- Auto-scroll and full-frame display (`pop-to-buffer-same-window` + `delete-other-windows`)
- Silent buffer revert (no "Reread from disk?" prompts)
- SHM state feedback: writes prompt/changes/buffer state to `/dev/shm/emacs_state`
- Advises `y-or-n-p`, `yes-or-no-p`, `read-from-minibuffer` for prompt detection
- Which-key integration: `C-c e` -> "claude-edit"

#### Hook Registration (settings.json)
- PreToolUse: `claude_edit_stream_pre` with Edit + Write matchers
- PostToolUse: `claude_edit_stream` with Edit + Write matchers, `sensor_awareness` for all tools
- All hooks use compiled C++ binaries, zero shell scripts
