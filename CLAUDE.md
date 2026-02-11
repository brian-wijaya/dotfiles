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
   kinetic RECALL_search_sessions(query=user's first message verbatim, recency_bias=0.7)

2. On resumption query ("where were we?", "continue", "what were we working on"):
   → IMMEDIATELY call kinetic RECALL_reassemble_loose_ends(lookback_days=10)
   → Present active tasks, loose ends, open questions in structured format
   → Ask user which thread to resume

3. On topic-specific session search (e.g., "sessions about emacs config"):
   → kinetic RECALL_search_sessions(query=topic keywords, recency_bias=0.0)

4. On session end or context compaction:
   kinetic RECALL_save_session(summary, topics, key_facts from session work)
</session_preamble>

<tool_dispatch>
Resolve every tool call through this table. One tool per operation. No deliberation.
All kinetic tools use the "kinetic" MCP server. Three category prefixes: ACT_, SENSE_, RECALL_.

x11_desktop:
  arrange_windows → ACT_arrange_windows (i3 command)
  resize_window → ACT_resize_window (width/height in ppt)
  focus_window → ACT_focus_window (container ID)
  query_layout → SENSE_read_window_layout (i3 tree + workspaces)
  screenshot → SENSE_capture_screen_region (optional window_id/region, returns PNG)
  send_keys → ACT_send_keystroke (xdotool key sequence)
  type_text → ACT_send_text_input (xdotool type with delay)
  click → ACT_send_click (x, y, button)
  move_cursor → ACT_send_mouse_move (x, y)
  get_focused → SENSE_get_focus
  run_command → ACT_execute_command (shell via bash -c)
  run_privileged → ACT_execute_privileged_command (sudo -n bash -c)
  overlay_message → ACT_emit_overlay_message (somatic chat overlay)
  set_attention → ACT_set_attention_target (somatic attention overlay)
  dismiss_overlay → ACT_dismiss_overlay
  process_list → SENSE_read_process_list (pgrep/ps)
  accessibility → SENSE_read_accessibility_tree (AT-SPI, experimental)

emacs:
  execute_elisp → ACT_evaluate_elisp (POST /mcp/eval)
  send_keys → ACT_send_keystroke (xdotool, focus emacs first)
  type_text → ACT_insert_text (POST /mcp/type)
  navigate → ACT_navigate_buffer (file/line/column/buffer)
  read_buffer → SENSE_read_buffer (buffer contents, optional line range)
  list_buffers → ACT_evaluate_elisp("(buffer-list)")
  list_windows → ACT_evaluate_elisp("(window-list)")
  search → ACT_evaluate_elisp with appropriate grep/occur elisp
  screenshot → SENSE_capture_screen_region (pass emacs window_id)

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

somatic:
  timestamp → ACT_now / ACT_delta
  transient_alert → ACT_post_message
  pre_irreversible → SENSE_check_permission
  user_state → SENSE_get_snapshot (14-dim state vector)
  typing_rhythm → SENSE_read_typing_rhythm (or SENSE_get_timing)
  pointer_state → SENSE_read_pointer_state (or SENSE_get_dynamics)
  x11_events → SENSE_read_desktop_events (or SENSE_get_events)
  clipboard_meta → SENSE_read_clipboard_metadata (or SENSE_get_latest)
  window_geometry → SENSE_get_layout
  keystrokes → SENSE_read_input_history (or SENSE_get_keystrokes)
  environment → SENSE_read_environment_snapshot (full fused state)

recall:
  search_vault → RECALL_search_hybrid (FTS5, semantic when P2 ready)
  search_sessions → RECALL_search_sessions (FTS5 + recency_bias)
  list_sessions → RECALL_list_recent_sessions
  get_session → RECALL_retrieve_session (by ID)
  reassemble_context → RECALL_reassemble_loose_ends (cross-session reconstruction)
  get_document → RECALL_retrieve_document (by ID or source_path)
  save_session → RECALL_save_session (summary + topics + key_facts)
  track_work_item → RECALL_append_ledger_event
  index_documents → RECALL_index_documents (requires Qdrant + TEI P2)

cross_context:
  pre_irreversible → SENSE_check_permission, then ACT_flash_text (red)
  post_visual_change → SENSE_get_anomalies + SENSE_get_events(count=20), then SENSE_read_window_layout, then SENSE_capture_screen_region
  compound_sequence → BASELINE(SENSE_get_snapshot + SENSE_get_events) → SETUP → ACT → VERIFY(SENSE_get_anomalies + SENSE_get_events + SENSE_capture_screen_region, diff against baseline)
</tool_dispatch>

<vault_resource_management>
Background indexing: vault-rag-watcher.service (file watcher, still Python)
Resource modes: Conservative (8GB/50% CPU, user active) ↔ Aggressive (32GB/100% CPU, 6hr idle)
Services: vault-rag-watcher.service (resource scaling managed by kinetic VaultResourceScaler)
MCP recall tools: via kinetic (RECALL_search_hybrid, RECALL_index_documents, etc.)
</vault_resource_management>

<file_protection_system>
3 layers: file-version-daemon (auto-version on modify/delete), trash (rm→trash-put), shell integration
Recovery: `trash-restore` (deleted), `file-versions restore <path> <id>` (overwritten)
Key commands: file-versions {list,restore,stats}, trash-{list,restore,empty,size}, rm-real (permanent)
CRITICAL: `rm` is aliased to trash. Use `rm-real` or `/usr/bin/rm` for permanent deletion.
Exclusions: .cache, .git, node_modules, .venv, >100MB files
Storage: ~/.local/share/{file-versions/,Trash/}, 90-day retention, content-deduplicated
</file_protection_system>
