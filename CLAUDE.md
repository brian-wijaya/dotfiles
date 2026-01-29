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
