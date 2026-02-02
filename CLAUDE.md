# Claude Code Global Instructions

<autonomous_execution>
When working through a committed/approved plan with sequential tasks:
- Proceed automatically through tasks without asking "Ready to proceed?"
- ONLY stop for these 4 conditions:
  1. Actual user input/choice needed for next action
  2. Crossing major Stage boundaries (Stage N → Stage N+1)
  3. Unexpected changes require plan modification
  4. About to execute risky/destructive operation
- User expectation: "unless you need my input, the answer is always yes"

HARD CONSTRAINT - DO NOT STOP for any of these:
- Token usage levels (even at 180K/200K tokens)
- "Substantial progress made"
- "Natural checkpoint" or "good stopping point"
- Desire to provide status update or summary
- Completed multiple tasks/phases within same Stage
- Sub-phases within same task (Task 1.5 Phase 1→Phase 2)
- Multi-hour or multi-day tasks
- Implementation complexity

DEFAULT RULE: If uncertain whether to stop → CONTINUE

Major phase boundaries = Stage 1→Stage 2, NOT Task 1.4→1.5 or Phase 2→Phase 3 within same Stage.
</autonomous_execution>

MCP config: `/home/bw/.claude.json` (user-level, `mcpServers` key). No project-level `.mcp.json`.

Dotfiles: edit live (`~/.emacs.d`, `~/.config`, etc.) → test → `cp -r <source> ~/dotfiles/` → commit in `~/dotfiles/`.
`~/dotfiles/` mirrors `~/` for tracked configs.
NEVER use symlinks, stow, chezmoi, or sync scripts for dotfile management — caused catastrophic data loss.

<session_preamble>
BLOCKING REQUIREMENTS - Execute before any other response:

1. On first user message of every session:
   vault-rag search_sessions(query=user's first message verbatim, recency_bias=0.7)

2. On resumption query ("where were we?", "continue", "what were we working on"):
   → IMMEDIATELY call vault-rag reassemble_loose_ends(lookback_days=10)
   → Present active tasks, loose ends, open questions in structured format
   → Ask user which thread to resume

3. On topic-specific session search (e.g., "sessions about emacs config"):
   → vault-rag search_sessions(query=topic keywords, recency_bias=0.0)

4. On session end or context compaction:
   vault-rag save_session(summary, topics, key_facts from session work)
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
  search_sessions → vault-rag search_sessions (with recency_bias)
  reassemble_context → vault-rag reassemble_loose_ends
  get_document → vault-rag get_document
  track_work_item → vault-rag append_ledger_event

cross_context:
  pre_irreversible → somatic check_permission, then somatic flash_text (red)
  post_visual_change → get_anomalies + get_events(count=20), then i3_windows, then x11_screenshot
  compound_sequence → BASELINE(get_snapshot + get_events) → SETUP → ACT → VERIFY(get_anomalies + get_events + screenshot, diff against baseline)
</tool_dispatch>

<vault_rag_resource_management>
Commands: `vault-rag index` (force now), `vault-rag auto` (restore), `vault-rag status`
Modes: Conservative (8GB/50% CPU, user active) ↔ Aggressive (32GB/100% CPU, 6hr idle)
Services: vault-rag-watcher.service, concierge.service
</vault_rag_resource_management>

<file_protection_system>
3 layers: file-version-daemon (auto-version on modify/delete), trash (rm→trash-put), shell integration
Recovery: `trash-restore` (deleted), `file-versions restore <path> <id>` (overwritten)
Key commands: file-versions {list,restore,stats}, trash-{list,restore,empty,size}, rm-real (permanent)
CRITICAL: `rm` is aliased to trash. Use `rm-real` or `/usr/bin/rm` for permanent deletion.
Exclusions: .cache, .git, node_modules, .venv, >100MB files
Storage: ~/.local/share/{file-versions/,Trash/}, 90-day retention, content-deduplicated
</file_protection_system>
