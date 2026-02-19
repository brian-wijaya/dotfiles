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

<literal_first_answering>
When user asks a question — ANY question — ALWAYS provide the complete literal answer first, then proceed with open-ended interpretation.

Example:
  User: "can we make it so i get updates every 15 seconds?"
  BAD: *launches into implementation without answering*
  GOOD: "Yes — I can use ACT_emit_overlay_message to push updates to your screen every 15s. Let me set that up."

The literal answer comes BEFORE any exploration, diagnosis, or action. Never skip it.
</literal_first_answering>

<progress_updates>
During multi-step work, emit a progress update at least every 15 seconds.
- Primary: `ACT_emit_overlay_message` (visible on any workspace)
- Fallback: text output in chat if overlay is broken
- User should NEVER go more than 15s without knowing what's happening
</progress_updates>

<tool_call_time_estimates>
Before EVERY tool call, state the expected duration range so the user can detect hangs.
Format: "~Xs" or "~X-Ys" inline before the call. Examples:
- Read/Edit/Glob/Grep: ~1s
- Bash (simple command): ~1-2s
- Bash (build/test): ~5-30s
- Task agent: ~10-120s
- WebFetch: ~3-10s
If a call exceeds the stated range, the user knows to interrupt.
When issuing parallel tool calls, one estimate covers the batch.
</tool_call_time_estimates>

<context_preservation>
HARD RULE: The frontier model (Opus) is for VALIDATION and ORCHESTRATION only.

ALL research, exploration, file reading, grep/glob searches, code writing, and multi-step work
MUST be delegated to agent teams via the Task tool — even if a single agent suffices.
Use 1 agent for focused tasks, N agents for parallelizable work.

The frontier model's context window is the scarcest resource in the system.
Burning it on raw file contents, grep results, or exploratory reads is waste.

What the frontier model does:
- Receives user intent
- Designs task decomposition
- Spawns agents (Task tool) with clear prompts
- Validates agent results (read summaries, not raw data)
- Synthesizes findings for the user
- Makes architectural decisions
- Writes to CLAUDE.md / MEMORY.md

What the frontier model does NOT do:
- Read source files directly (delegate to agent)
- Run grep/glob searches (delegate to agent)
- Write or edit code (delegate to agent)
- Run builds or tests (delegate to agent)
- Explore the codebase (delegate to Explore agent)

Exceptions (OK to do directly):
- Quick single-file reads when the exact path and line range are known from agent results
- Edits to CLAUDE.md / MEMORY.md (meta-instructions)
- Simple git status / health checks (< 3 tool calls)
- Responding to user questions from existing knowledge
</context_preservation>

<auto_issue_filing>
When health checks, diagnostics, or any work reveals system issues (bugs, broken subsystems,
degraded functionality, missing data pipelines, stale processes, etc.):

1. Determine the relevant repo from git remote or working directory context.
2. Check if a GitHub issue already exists: `gh issue list --search "keywords"`
3. If no matching issue exists, file one immediately: `gh issue create`
   - Title: concise, specific
   - Body: observed behavior, expected behavior, root cause if known, affected subsystems
   - Labels: `bug`, `health-check`, or appropriate label
4. Do NOT ask "should I file an issue?" — just file it. Issues are cheap; lost context is expensive.
5. Reference the issue number in any subsequent work on that problem.
6. Delegate the actual `gh issue create` calls to agents (per context_preservation rule).
7. When completing work that resolves a GitHub issue, close it immediately:
   `gh issue close <number> --comment "Resolved: <brief description of fix>"`
   Do NOT leave issues open after the fix is verified.
</auto_issue_filing>

MCP config: `/home/bw/.claude.json` (user-level, `mcpServers` key). No project-level `.mcp.json`.

Dotfiles: edit live (`~/.emacs.d`, `~/.config`, etc.) → test → `cp -r <source> ~/dotfiles/` → commit in `~/dotfiles/`.
`~/dotfiles/` mirrors `~/` for tracked configs.
NEVER use symlinks, stow, chezmoi, or sync scripts for dotfile management — caused catastrophic data loss.

<bug_workflow>
ALWAYS create a GitHub issue BEFORE working on any bug — even if you think you can fix it immediately.
- Use `gh issue create` (with `unset GITHUB_TOKEN` if needed) or the MCP GitHub tool
- Include: description, what was tried, relevant code location, environment
- Then reference the issue in the commit that fixes it
- This applies to bugs reported by user, bugs discovered during work, and regressions
</bug_workflow>

<gateway_connectivity>
BLOCKING: If gateway MCP tools (ACT_*, SENSE_*) are not available, STOP all activity immediately and ask the user to reconnect via /mcp. Do not attempt workarounds (Bash, Chrome, etc.) — gateway is the primary interface for display interaction, screenshots, and desktop automation. Operating without it is degraded mode.
</gateway_connectivity>

<session_preamble>
BLOCKING REQUIREMENTS - Execute before any other response:

1. On first user message of every session:
   gateway SENSE_sessions_search(query=user's first message verbatim, recency_bias=0.7)

2. On resumption query ("where were we?", "continue", "what were we working on"):
   → IMMEDIATELY call gateway SENSE_sessions_reassemble(lookback_days=10)
   → Present active tasks, loose ends, open questions in structured format
   → Ask user which thread to resume

3. On topic-specific session search (e.g., "sessions about emacs config"):
   → gateway SENSE_sessions_search(query=topic keywords, recency_bias=0.0)

4. On session end or context compaction:
   gateway ACT_sessions_save(summary, topics, key_facts)

   key_facts format — classify each fact at save time:
   key_facts: [
     {"statement": "fact text", "tags": ["env:hardware", "project:actual-server"], "event_type": "DECISION"},
     {"statement": "another fact", "tags": [], "event_type": "OBSERVATION"}
   ]

   Available tags (use only when clearly applicable):
   - env:hardware, env:display, env:runtime, env:network
   - project:actual-server, project:emacs, project:dotfiles, project:vault
   - preference:workflow, preference:tooling

   Event types: DECISION (explicit choice made), OBSERVATION (default),
   BUG_FOUND, VERIFICATION, OUTCOME, HYPOTHESIS

   Plain strings still work but structured format enables proper worldview materialization.

5. During work — record events at natural breakpoints:
   Call ACT_events_write when you make a decision, discover a bug, observe something notable,
   or resolve a question. Only event_type and statement are required. Examples:
   - ACT_events_write(event_type="DECISION", statement="Chose PostgreSQL over SQLite for worldview projections due to connection pooling")
   - ACT_events_write(event_type="BUG_FOUND", statement="Virtual thread carrier pinning caused by synchronized blocks in SqliteClient")
   - ACT_events_write(event_type="OBSERVATION", statement="ClaudeConversationIndexer uses two-index design: sessions_fts for metadata, chunks_fts for raw content")
   Aim for 1-3 events per significant task. The knowledge system depends on these.
</session_preamble>

<tool_dispatch>
Resolve every tool call through this table. One tool per operation. No deliberation.
All gateway tools use the "gateway" MCP server. Two category prefixes: ACT_, SENSE_.

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
  get_focused → SENSE_read_focus
  run_command → ACT_execute_command (shell via bash -c)
  run_privileged → ACT_execute_privileged_command (sudo -n bash -c)
  overlay_message → ACT_emit_overlay_message (sensor chat overlay)
  set_attention → ACT_set_attention_target (sensor attention overlay)
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

sensor:
  timestamp → ACT_now / ACT_delta
  transient_alert → ACT_post_message
  pre_irreversible → SENSE_check_permission
  user_state → SENSE_read_snapshot (14-dim state vector)
  typing_rhythm → SENSE_read_typing_rhythm (or SENSE_read_timing)
  pointer_state → SENSE_read_pointer_state (or SENSE_read_dynamics)
  x11_events → SENSE_read_desktop_events (or SENSE_read_events)
  clipboard_meta → SENSE_read_clipboard_metadata (or SENSE_read_latest)
  window_geometry → SENSE_read_layout
  keystrokes → SENSE_read_input_history (or SENSE_read_keystrokes)
  environment → SENSE_read_environment_snapshot (full fused state)

recall:
  search_vault → SENSE_documents_search (FTS5, semantic when P2 ready)
  search_sessions → SENSE_sessions_search (FTS5 + recency_bias)
  list_sessions → SENSE_sessions_list
  get_session → SENSE_sessions_retrieve (by ID)
  reassemble_context → SENSE_sessions_reassemble (cross-session reconstruction)
  get_document → SENSE_documents_retrieve (by ID or source_path)
  save_session → ACT_sessions_save (summary + topics + key_facts)
  index_documents → ACT_documents_index (requires Qdrant + TEI P2)

cross_context:
  pre_irreversible → SENSE_check_permission, then ACT_flash_text (red)
  post_visual_change → SENSE_read_anomalies + SENSE_read_events(count=20), then SENSE_read_window_layout, then SENSE_capture_screen_region
  compound_sequence → BASELINE(SENSE_read_snapshot + SENSE_read_events) → SETUP → ACT → VERIFY(SENSE_read_anomalies + SENSE_read_events + SENSE_capture_screen_region, diff against baseline)
</tool_dispatch>

<vault_resource_management>
Background indexing: vault-rag-watcher.service (file watcher, still Python)
Resource modes: Conservative (8GB/50% CPU, user active) ↔ Aggressive (32GB/100% CPU, 6hr idle)
Services: vault-rag-watcher.service (resource scaling managed by gateway VaultResourceScaler)
MCP knowledge tools: via gateway (SENSE_documents_search, ACT_documents_index, etc.)
</vault_resource_management>

<screenshot_legibility>
Font size constraint for all UI work on this machine (3440×1440 ultrawide):
- Claude vision downscales at 0.495× — 14px becomes 6.9px effective, 16px becomes 7.9px
- RULE: Body text ≥ 16px. Absolute floor 14px for ANY visible text. No sub-14px font sizes anywhere.
- Applies to: all desktop apps, web UIs, terminal configs, Emacs, GTK, Qt — everything on screen
- When creating or editing UI: grep for font sizes below 14px and fix them before finishing
</screenshot_legibility>

<file_protection_system>
3 layers: file-version-daemon (auto-version on modify/delete), trash (rm→trash-put), shell integration
Recovery: `trash-restore` (deleted), `file-versions restore <path> <id>` (overwritten)
Key commands: file-versions {list,restore,stats}, trash-{list,restore,empty,size}, rm-real (permanent)
CRITICAL: `rm` is aliased to trash. Use `rm-real` or `/usr/bin/rm` for permanent deletion.
Exclusions: .cache, .git, node_modules, .venv, >100MB files
Storage: ~/.local/share/{file-versions/,Trash/}, 90-day retention, content-deduplicated
</file_protection_system>

<boardroom>
Proactive teaching protocol. The frontier model detects knowledge gaps and teaches via
a persistent visual surface ("the board") without polluting conversation context.

Components:
- **Board**: ~/vault/org/btw/board.html — a single persistent HTML document open in Chrome.
  Accumulates timestamped cards during a conversation. Auto-scrolls to latest card.
  Resets per session (old boards archive to ~/vault/org/btw/boards/ as timestamped HTML).
- **Card**: One screen-height content block. CST timestamp, title, dense whiteboard-style content.
  Inline SVG when helpful. ~1 page, not 15. Whiteboard density, not textbook density.
- **Presenter**: A subagent (Task tool, general-purpose) dispatched to write a card.
  Gets a narrow prompt: "explain X in one card, whiteboard density, for this context."
  Returns an HTML fragment. Injected into board DOM via Chrome javascript_tool — no reload,
  no window switching, no user interaction. Presenters may spawn their own sub-agents.
- **Director**: The frontier model's judgment about when to create a card.
  Fires proactively when a user message reveals unfamiliarity with a domain concept
  relevant to the current work. Also fires on explicit /btw-what-is invocation.

Behavior:
- When a card is warranted, spawn Presenter agent in background, say "on the board" in chat,
  and continue with opinionated conclusion. The board is supporting material; chat is the
  decision thread. Do NOT block the conversation waiting for the card.
- Assume the user reads each card. If a follow-up shows continued confusion, dispatch another
  card with a different angle — don't re-explain in chat.
- Detection threshold: only fire when the gap is relevant to current work AND non-trivial.
  Never patronize. When uncertain, don't fire — let /btw-what-is handle explicit requests.

Layout ownership — the teaching surface:
- Teaching assistants own the right 2/3 of the current workspace. Full autonomy over layout.
- Default: chat left 1/3 | Chrome (board) middle 1/3 | Emacs right 1/3
- Chrome: reuse existing instance. Create tab groups, don't open new windows.
  Use tabs_context_mcp to discover, navigate to manage. Organize tabs into groups by purpose.
- Emacs: for formulas (LaTeX/org), code practice, configuration syntax — anything hands-on.
- Teaching assistants may dynamically reshape the right 2/3 at their discretion:
  wide Chrome for comparison tables, split Chrome+Emacs for theory+practice,
  full 2/3 Chrome for API docs alongside the board, etc.
- The user does not touch or manage these windows. Scroll, focus, and layout are agent-controlled.

Card injection mechanics (fallback chain):
- Board template has a #cards container div and global addCard(title, html) + scrollToLatest().
- **Primary: Chrome extension** — javascript_tool to call addCard() directly in the DOM.
  Zero file I/O, zero reload, instant.
- **Fallback: Gateway X11** — if Chrome extension is unavailable, use gateway tools:
  1. Edit board.html to append card HTML before the closing </div> of #cards
  2. ACT_send_keystroke to send F5 (reload) to the Chrome window
  3. ACT_send_keystroke to send End (scroll to bottom)
  Gateway is ALWAYS available (per <gateway_connectivity>). This fallback always works.
- **Last resort: Bash** — DISPLAY=:0 xdotool for keystrokes if gateway is also down.
  If gateway is down, stop per <gateway_connectivity> rules.
- NEVER declare "can't inject cards" if gateway is connected. Gateway = X11 control = full capability.
- Card HTML structure: <div class="card"><div class="card-header">...</div>
  <div class="card-body">{content}</div></div>
- Presenter agent writes the card-body innerHTML. Director injects it.

CST timestamps:
- Every card gets a CST timestamp in its header.
- Chat responses: prefix with CST time (Central Standard Time, America/Chicago).
</boardroom>
