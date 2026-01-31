# Claude Code Global Instructions

MCP config: `/home/bw/.claude.json` (user-level, `mcpServers` key). No project-level `.mcp.json`.

Dotfiles: edit live (`~/.emacs.d`, `~/.config`, etc.) → test → `cp -r <source> ~/dotfiles/` → commit in `~/dotfiles/`.
`~/dotfiles/` mirrors `~/` for tracked configs.
NEVER use symlinks, stow, chezmoi, or sync scripts for dotfile management — caused catastrophic data loss.

<session_preamble>
On first user message of every session, before any other tool call:
1. vault-rag search_sessions(query=user's first message verbatim)
On session end or context compaction:
2. vault-rag save_session(summary, topics, key_facts from session work)
</session_preamble>

<tool_dispatch>
Resolve every tool call through this table. One tool per operation. No deliberation.

x11_desktop:
  arrange_windows → i3_command
  resize_window → i3_command "resize set {w} ppt 0 ppt"
  focus_window → i3_command "[con_id=X] focus"
  query_windows → i3_windows
  query_workspaces → i3_workspaces
  screenshot → x11_screenshot (optionally with window_id)
  send_keys → x11_key
  type_text → x11_type
  click → x11_click
  move_cursor → x11_mouse_move
  get_focused → x11_get_active_window
  start_process → x11 process_start
  notify_persistent → x11 notify

emacs:
  execute_elisp → emacs_elisp
  send_keys → emacs_key
  type_text → emacs_type
  navigate → emacs_navigate
  read_buffer → emacs_buffer_read
  list_buffers → emacs_buffer_list
  list_windows → emacs_window_list
  search → emacs_find
  screenshot → emacs_screenshot (use x11_screenshot if frame capture needed)

browser:
  get_tabs → tabs_context_mcp
  new_tab → tabs_create_mcp
  navigate → navigate
  read_page → read_page (accessibility tree)
  read_text → get_page_text (plain text)
  find_element → find
  fill_form → form_input
  click/type/screenshot → computer (action=left_click|type|screenshot)
  execute_js → javascript_tool
  console → read_console_messages
  network → read_network_requests

filesystem:
  read_file → Read
  edit_file → Edit
  write_file → Write
  search_content → Grep
  search_names → Glob
  run_command → Bash
  privileged_command → sudo (MCP)

somatic:
  timestamp → somatic now/delta
  transient_alert → somatic post_message
  pre_irreversible → somatic check_permission
  user_state → somatic get_snapshot
  typing_rhythm → somatic get_timing
  pointer_state → somatic get_dynamics
  x11_events → somatic get_events
  clipboard_meta → somatic get_latest
  window_geometry → somatic get_layout
  keystrokes → somatic get_keystrokes

knowledge:
  search_vault → vault-rag search_hybrid
  search_sessions → vault-rag search_sessions
  get_document → vault-rag get_document

cross_context:
  pre_irreversible → somatic check_permission, then somatic flash_text (red)
  post_visual_change → get_anomalies + get_events(count=20), then i3_windows, then x11_screenshot
  compound_sequence → BASELINE(get_snapshot + get_events) → SETUP → ACT → VERIFY(get_anomalies + get_events + screenshot, diff against baseline)
</tool_dispatch>

<vault_rag_resource_management>
# Vault-RAG Resource Management

The vault-rag indexing watcher runs continuously with adaptive resource limits.

## Manual Control Commands

- `vault-rag index` - Force aggressive indexing NOW (32GB RAM, 100% CPU)
  Use when stepping away and want indexing to power through backlogs

- `vault-rag auto` - Return to automatic mode (6-hour idle threshold)
  Restores conservative limits and re-enables automatic switching

- `vault-rag status` - Show current mode and resource limits

## Automatic Behavior

**Conservative Mode** (default when user active):
- 8GB RAM limit, 50% CPU quota
- Indexing runs in background without impacting interactive work

**Aggressive Mode** (auto-triggers after 6 hours idle):
- 32GB RAM limit, 100% CPU quota
- Full system resources for overnight indexing
- Auto-restores conservative when user returns (<1 min activity)

**Services:**
- `vault-rag-watcher.service` - File watcher and indexer
- `concierge.service` - Manages auto-switching based on idle time

**Logs:**
- Concierge: `/home/bw/vault/data/logs/concierge.log`
- Watcher: `/home/bw/vault/data/logs/watcher.log`
- Systemd: `journalctl --user -u vault-rag-watcher -f`
</vault_rag_resource_management>

<file_protection_system>
# File Protection System

Multi-layered automatic versioning protects against accidental deletion and overwrites.

## Architecture

**Layer 1: Filesystem Watcher** (automatic, passive)
- `file-version-daemon.service` watches all of $HOME via inotify
- Saves version before MODIFY, DELETE, MOVE operations
- Content-addressed deduplication (same content = one copy)
- Rate-limited (60s) to avoid excessive versions during editing
- 90-day automatic retention

**Layer 2: Shell Integration** (explicit operations)
- `rm` aliased to `trash-put` (moves to trash, not permanent delete)
- `safe_cp` and `safe_mv` functions backup before overwrite
- Recovery aliases: `unrm`, `file-history`, `file-restore`

**Layer 3: Trash** (user-friendly recovery)
- FreeDesktop.org compliant trash at `~/.local/share/Trash/`
- GUI file managers integrate automatically
- Command-line: `trash-put`, `trash-restore`, `trash-list`

## Storage

```
~/.local/share/file-versions/
├── content/               # Content-addressed storage (SHA256 hash filenames)
│   └── a3f5b8c...        # Deduplicated file contents
└── metadata.db           # SQLite index (path, hash, timestamp, operation)

~/.local/share/Trash/
├── files/                # Trashed files
└── info/                 # Deletion metadata
```

## Commands

**Versioning:**
- `file-versions list <path>` - Show all saved versions of a file
- `file-versions restore <path> <id>` - Restore specific version
- `file-versions stats` - Show storage usage and dedup ratio

**Trash:**
- `rm <file>` - Move to trash (safe, reversible)
- `unrm` or `trash-restore` - Restore from trash interactively
- `trash-list` - Show all trashed files
- `trash-empty [days]` - Empty trash (optionally older than N days)
- `trash-size` - Show trash disk usage

**Escape hatches:**
- `rm-real <file>` - Bypass trash, permanent delete (use sparingly)
- `/usr/bin/rm` - Direct system rm

## Recovery Workflow

**Accidental delete:**
1. `trash-list` - Find deleted file
2. `trash-restore` - Restore interactively
3. OR browse `~/.local/share/Trash/files/` directly

**Accidental overwrite:**
1. `file-versions list <path>` - Show versions
2. `file-versions restore <path> <id>` - Restore previous version
3. Current file backed up to `.bak` automatically

**Check what's protected:**
- `file-versions stats` - See how many versions stored
- `journalctl --user -u file-version-daemon -f` - Watch live activity

## Exclusions

Version daemon ignores (performance + privacy):
- `.cache`, `.git`, `node_modules`, `.venv`, `__pycache__`
- `.cargo/registry`, `.npm`, `.local/share/Trash`
- Files >100MB (configurable in `/home/bw/bin/file-version-daemon`)
- Binary artifacts: `*.pyc`, `*.o`, `*.so`

## Maintenance

**Automatic:**
- Daemon purges versions >90 days old daily
- Content deduplication (same file saved once)
- Orphaned content cleanup

**Manual:**
- Check logs: `tail -f ~/vault/data/logs/file-version-daemon.log`
- Restart daemon: `systemctl --user restart file-version-daemon`
- Purge all old versions: Edit RETENTION_DAYS in daemon script

## Services

- `file-version-daemon.service` - Automatic versioning (always running)
- Integrated with existing shell config (auto-loaded)

## Important Notes

1. **Not a backup** - This protects against accidents, not hardware failure. Keep real backups elsewhere.
2. **Performance** - Daemon uses ~20% CPU max, 512MB RAM max, low I/O priority.
3. **Privacy** - All data stays local in `~/.local/share/`.
4. **Deduplication** - Same content stored once, even across different files.
5. **rm is now trash** - Use `rm-real` if you absolutely need permanent deletion.
</file_protection_system>
