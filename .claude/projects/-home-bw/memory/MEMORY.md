# Memory

## Operational model — context preservation
- **Frontier model (Opus) = orchestrator only**. ALL research, code, exploration delegated to agents via Task tool.
- Even single-agent tasks use Task tool — never burn frontier context on raw file reads, greps, or code edits.
- Frontier does: task decomposition, agent spawning, result validation, synthesis, architectural decisions, meta-instruction edits.
- Full rule in `~/CLAUDE.md` `<context_preservation>` section.

## Approved vocabulary — source of truth
- **File**: `~/actual/docs/approved-vocabulary.md` (Status: Locked)
- **Naming convention**: descriptive names, no branded internals
- **sensor** (C++ daemon, was "somatic"), **gateway** (Java MCP server, was "kinetic"), **memory** (recall service, greenfield)
- **Two prefixes only**: `ACT_` (modifies state), `SENSE_` (reads state). RECALL_ prefix retired.
- Old names (`somatic`, `kinetic`) FULLY retired. Only 2 legacy references remain: data normalization in ClaudeConversationIndexer and BootstrapEvents.
- JDK 25. Plain CSS (no Tailwind).

## Gateway project status
- **Repo**: `~/actual/` (SignalAssembly/actual). Single repo — all prior repos deleted.
- **Production**: `actual-gateway.service` (systemd user). Config: `~/.config/actual/gateway.toml`. MCP: `"gateway"` in `~/.claude.json`. Build: `~/actual/build/libs/actual-1.0.0.jar`.
- **31 tools** after pruning (was 106). Includes ACT_send_notification (ntfy, config-gated).
- **REST API for debugging**: GET /api/tools, GET/POST /api/tool/{name} on port 8372.
- **VaultWatcher**: background pipeline — DocumentIndexer, ClaudeConversationIndexer, EmailIndexer. All virtual threads. FTS5-only by default, adds embeddings if Qdrant+TEI available.
- **FTS5 query sanitization**: all MATCH queries quote each token. Fixed in SqliteClient + Python vault-rag db/sqlite.py.
- **Display isolation** (ADR-007): Xvfb :99 + i3 + per-display sensor via `SENSOR_SHM_SUFFIX`. Auto-attaches xpra viewer to workspace 9.
- **HTTP daemon mode**: Streamable HTTP via Jetty 12.1.5 on `127.0.0.1:8372`. Multi-session. Config: `server.transport = "http"`.
- **Protocol version shim**: `ProtocolVersionFilter.java` patches `2024-11-05` → `2025-11-25`. Remove when SDK updates.
- **Null JSON Schema kills tool loading**: any `null` property value in `properties` causes Claude Code to silently reject entire `tools/list` response.
- **Sensor binary**: `sudo cp builddir/src/adapters/mcp/sensor /usr/local/bin/sensor` after rebuilding.
- **Host display routing**: `display_id: "host"` routes to real X11 (:0) via `DisplayEnvironment`.
- **maim --noopengl required on :99**: maim's OpenGL capture conflicts with xpra's composite manager.
- **Session lifecycle fix** (2026-02-20): Fixed 878-session memory leak (209MB→3GB). 5 changes: idle timeout 24h→5min, 20-session hard cap with oldest-idle eviction, shared ScheduledExecutorService (was per-session), KeepAlive 120s re-enabled, NMT enabled. ADR in actual/docs/ADR/. Fresh baseline: 366MB committed.
- **OTel agent v2.25.0**: Upgraded from v2.11.0 (broken on JDK 25). Jar at `~/actual/lib/opentelemetry-javaagent.jar`. gRPC exporter (port 4317).
- **Observability stack** (Docker): Prometheus, Grafana, Tempo, Loki, OTel Collector. All three pipelines operational (metrics→Prometheus, traces→Tempo, logs→Loki). JFR enabled (`-XX:StartFlightRecording`).
- **Resource allocation** (2026-02-20): Liberal limits — gateway: -Xmx8g, MemoryMax=32G, MemoryHigh=24G, CPUQuota=400%, TasksMax=4096. Docker: Prometheus 4g, Grafana 2g, Tempo 4g, Loki 4g, OTel Collector 2g. Machine: 128GB RAM, 48 cores (Threadripper PRO 5965WX).
- **All hooks now C++**: save_session (libpq, 21ms), capture-user-turn (libpq, 29ms), sensor_awareness (SHM, 3ms). SQLite completely eliminated from hook chain.
- **SQLite→Postgres migration complete**: 478 sessions + 873 turns migrated. vault.db no longer written to by any active hook.
- **Naming**: All "actual-server" references renamed to "actual" (config path, systemd, source code, dashboards).

## Architecture decisions

### ADR-034: PostgreSQL replaces SQLite (accepted 2026-02-17)
- SQLite is transitional; PostgreSQL is the target for worldview projections.
- **Four-store pipeline**: Kafka → Neo4j → PostgreSQL → Qdrant.
- Resolves sqlite-jdbc carrier pinning and write contention issues.

### Virtual thread carrier pinning (2026-02-15, updated 2026-02-20)
- **synchronized blocks**: pin carrier threads (pool size = CPU count). All explicit `synchronized` replaced with `ReentrantLock` in SqliteClient + SensorClient.
- **Process I/O**: `FileInputStream.readBytes` holds `ProcessPipeInputStream` monitor, pinning carriers. All process stream readers must use `Thread.ofPlatform().daemon()`, NOT `Thread.ofVirtual()`.
- sqlite-jdbc still has internal `synchronized` — resolved by ADR-034.

### gRPC architecture
- Gateway uses MCP over HTTP. Sensor hot path is shared memory + raw Unix sockets.
- gRPC planned ONLY as edge adapter: ML consumers and inter-device RPC over Tailscale.

### Kafka consumer contract
- Handlers are `Predicate<>` (return boolean), not `Consumer<>` (void). Return false = don't commit batch. At-least-once semantics.

## CI & Testing
- **GitHub Actions**: self-hosted runner "threadripper" at `~/github-actions-runner/`.
- **Gateway tests**: JUnit 5 + YAML story runner (StoryRunner.java). 31/31 tools covered, 36 dynamic tests.
- **Mosaic tests**: 55 Rust unit tests (cargo test), 14 Vitest frontend tests with JUnit XML reporter.
- **Benchmarks**: JMH (tool dispatch, circuit breaker, error classifier, JSON). Baselines at `~/vault/data/benchmarks/gateway/`.
- **Bencher**: self-hosted regression detection (API port 61016, console port 3001).
- **just**: command runner with justfiles in `~/actual/` and `~/actual-mosaic/`.

## Actual Mosaic (2026-02-20)
- **Status**: v0.1.0 running. Standalone Tauri 2.x app at `~/actual-mosaic/`.
- **Concept**: Agent's visual communication channel. "Like talking with hands." Terminal = voice, mosaic = gestures. NOT a decision tool — agent controls, human watches.
- **Stack**: Tauri 2.x (Rust + axum MCP on 127.0.0.1:8373) + vanilla TS/CSS frontend. JSON-RPC 2.0.
- **MCP config**: `"actual-mosaic": { "type": "http", "url": "http://127.0.0.1:8373/mcp" }` in `~/.claude.json`.
- **Tools**: `add_slide` (5 types: statement/emphasis/diagram/note/rich, 6 entrances, html param, session/turn IDs), `clear`, `prepare` (wind-up skeleton), `heartbeat` (viewport + slide count).
- **Three-phase turn cycle**: Wind-up (UserPromptSubmit hook → prepare → skeleton) → Chat lands → Voila (add_slide → dissolve).
- **SHM IPC**: `/dev/shm/mosaic_state`, `mosaic_session_id`, `mosaic_turn_id`. Hooks write, Rust reads as fallback.
- **Hooks** (all 6 are compiled C++ binaries, zero shell scripts):
  - `mosaic_prepare` (UserPromptSubmit) — triggers Mosaic skeleton only (no longer writes timestamps/IDs)
  - `mosaic_sync` (Stop) — reads SHM files written by capture-user-turn, displays T{N}/timestamp/IDs in terminal
  - `capture-user-turn` (UserPromptSubmit) — C++ with libpq, generates turn_id UUID, writes timestamp/session_id/turn_id to SHM + Postgres (session clock owner)
  - `save_session` (Stop, PreCompact) — C++ with libpq, UPSERT to Postgres
  - `sensor_awareness` (PostToolUse) — C++ SHM read, <3ms
  - `enforce_continuation` (Stop) — C++ checkpoint logic
- **Timestamp format**: `Fri Feb 20, 2026 - 06:57:10:137 CST` (millisecond precision).
- **Focus prevention**: i3 `no_focus` + Tauri `"focus": false`.
- **WebKit fix**: `WEBKIT_DISABLE_DMABUF_RENDERER=1` on Arch Linux.
- **Docs**: README.md, CHANGELOG.md, PRD.md, glossary.md, 3 ADRs in `docs/ADR/`.

## Actual authorial extraction
- **Repo**: `~/actual/`. 9 feature files, PRD, implementation plan, ADRs, approved vocabulary.
- **Feature files**: desktop-embodiment (5 DS), agent-cognition (3 DS), knowledge-architecture (6 DS), telemetry (4 DS), agent-coordination (3 DS), voice-integration (2 DS), ambient-intelligence (3+1 DS), multi-device (3 DS), reflective-processing (2+6 DS)
- **User Work Surface**: Tauri app (Rust+WebView). 16 knowledge states, 48 visual configs. Feature file written.
- **Still open**: AMB-DS4, UWS DS-5 (visual encoding), UWS DS-8 (rendering architecture)

## Screenshot legibility / font size rule
- **Screen**: 3440x1440 ultrawide. Claude vision downscales at 0.495x.
- **Font mapping**: 12px→5.9px (invisible), 14px→6.9px (threshold), 16px→7.9px (readable), 20px→9.9px (comfortable)
- **Rule**: Body text >=16px, absolute floor 14px for any UI text. No exceptions.

## Bug workflow
- **Issue first, fix second**: ALWAYS create a GitHub issue before working on any bug. Reference the issue in the fix commit.
- Gateway issues #26/#27 (session timeout/memory leak) — root cause was 878-session accumulation, fixed 2026-02-20.

## Gateway connectivity protocol
- If gateway MCP tools (ACT_*, SENSE_*) are unavailable, STOP and ask user to reconnect via `/mcp`. No workarounds.

## Communication protocols
- **Literal-first answering**: Always give a complete literal answer first, then proceed with action.
- **Progress updates**: `ACT_emit_overlay_message` at least every 15 seconds during multi-step work.
- **Show-me default**: After visible UI changes, auto-screenshot :99 and open on user's display. Don't ask.
- **Attention prerequisite**: Detect user's workspace via `SENSE_read_window_layout(format=workspaces)` before visual communication.

## Desktop app testing on :99
- Use actual app on :99, not Chromium. Kill :0 instance first (Tauri single-instance).
- Display height: `user_height - polybar_height` (3440x1400). Config: `display.panel_height = 40`.
- `display_id` param uses string "99" (no colon).

## Dotfile workflow
- Edit live → test → `cp -r <source> ~/dotfiles/` → commit in `~/dotfiles/`
- NEVER use symlinks/stow/chezmoi

## Emacs notes
- Niche Emacs debugging notes (which-key C subrs, YASnippet, x11_key timing) in `~/.claude/projects/-home-bw/memory/emacs-notes.md`.
