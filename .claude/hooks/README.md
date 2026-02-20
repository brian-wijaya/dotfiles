# Claude Code Hooks

Compiled C++ hooks for Claude Code's lifecycle system. All hooks are raw POSIX -- no heap allocation, no dependencies beyond libc.

## Architecture

```
Claude Code tool call
       |
PreToolUse hooks fire (before execution)
       |
Tool executes (Edit, Write, etc.)
       |
PostToolUse hooks fire (after execution)
       |
SHM state updated -> Emacs notified -> Overlays rendered
```

## Hooks

| Binary | Lifecycle | Triggers | Purpose |
|--------|-----------|----------|---------|
| `claude_edit_stream_pre` | PreToolUse | Edit, Write | Attention protocol -- ensures Emacs is visible before edits |
| `claude_edit_stream` | PostToolUse | Edit, Write | Streams edit data to Emacs via SHM |
| `sensor_awareness` | PostToolUse | All | Fuses ambient state from sensor + Emacs + i3 into tool response |
| `save_session` | Stop, PreCompact | -- | Persists session to PostgreSQL |
| `capture-user-turn` | UserPromptSubmit | -- | Records user turns to PostgreSQL |
| `mosaic_prepare` | UserPromptSubmit | -- | Triggers Mosaic skeleton animation |
| `mosaic_sync` | Stop | -- | Syncs Mosaic state display |
| `enforce_continuation` | Stop | -- | Checkpoint logic for autonomous execution |

## Daemons

| Binary | Managed By | Purpose |
|--------|-----------|---------|
| `i3_state_writer` | systemd (`i3-state-writer.service`) | Subscribes to i3 events, writes workspace/window state to `/dev/shm/x11_state` |

## SHM Files

| Path | Writer | Reader | Format |
|------|--------|--------|--------|
| `/dev/shm/sensor_ambient` | sensor daemon | sensor_awareness | Ambient state vector |
| `/dev/shm/emacs_state` | claude-edit-stream.el | sensor_awareness, claude_edit_stream | `prompt=yes\|no changes=N buffer=X` |
| `/dev/shm/x11_state` | i3_state_writer | sensor_awareness, claude_edit_stream, claude_edit_stream_pre | `workspace=N emacs_visible=yes\|no emacs_wksp=M focused_class=X` |
| `/dev/shm/claude_edit_stream` | claude_edit_stream | claude-edit-stream.el | JSON lines per edit |
| `/dev/shm/mosaic_state` | mosaic_prepare | mosaic_sync | Mosaic viewport state |

## Building

```bash
g++ -O2 -o claude_edit_stream claude_edit_stream.cpp
g++ -O2 -o claude_edit_stream_pre claude_edit_stream_pre.cpp
g++ -O2 -o i3_state_writer i3_state_writer.cpp
g++ -O2 -o sensor_awareness sensor_awareness.cpp
```

## Exit Code Protocol

- Exit 0: success (continue)
- Exit 2: blocking error -- halts Claude and feeds stderr as context (deterministic interrupt)

Used by `claude_edit_stream` when Emacs has a blocking prompt (`prompt=yes`) or is not visible after edit.
