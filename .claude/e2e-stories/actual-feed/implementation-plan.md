# Implementation Plan
## Actual Feed — V1 Desktop Release

### Meta

- **Constraint**: Solo developer + Claude agents. No calendar dates — phases are dependency-ordered, not time-boxed.
- **V1 ships after Phase 4.** Phases 5-8 are V1.1+.
- **Critical path**: Phase 1 → Phase 2 → Phase 3 → Phase 4 → Ship.
- **All task IDs reference**: stories (US-xxx), enforced constraints (EC-xxx), opinionated constraints (OC-xxx), and ADRs.

### Key Documents

- **[Project Tree](project-tree.md)** — Canonical directory layout, module boundaries, naming conventions. **Authoritative for all paths.**
- **[Vocabulary](vocabulary.md)** — Locked glossary of every primitive, enum, and concept. **Authoritative for all naming.**
- **[Extraction Prompt](extraction-prompt.md)** — The Stage 2 LLM prompt template, JSON schema, and signal calibration guidance
- **[Wireframes](wireframes.md)** — ASCII wireframes for Feed, Item Detail, Settings, and Onboarding screens
- **[PRD](PRD.md)** — Product requirements, stories, constraints

### Dependency Graph

```
Phase 0 (Distribution & CI) ─── parallel with Phase 1
Phase 1 (Skeleton)
  └── Phase 2 (Engine)
        ├── Phase 3 (Desktop MVP)
        │     └── Phase 4 (Desktop Complete) → V1 SHIP
        ├── Phase 5 (Android) ─────────────────────────┐
        ├── Phase 6 (Cloud & Sync) ────────────────────┼─→ V1.1+
        ├── Phase 7 (iOS) ─── depends on Phase 6 too ──┘
        └── Phase 8 (Emacs) ── can start after Phase 3
```

---

## Phase 0: Distribution & CI

**Depends on**: Nothing. Runs in parallel with Phase 1.
**Output**: Reproducible cross-platform builds, native packaging, code signing, auto-update infrastructure.
**Rationale**: Building distribution from day one prevents the "works on my machine" trap. Every phase gate produces shippable artifacts.

### Task 0.1: GitHub Actions CI matrix

**What**: Cross-platform build matrix producing testable artifacts on every push.

**Where**: `.github/workflows/ci.yml`

**Matrix**:

| Target | Runner | Toolchain | Output |
|---|---|---|---|
| macOS arm64 | `macos-14` (M1) | Xcode 15, Rust stable, JDK 21, pnpm | `.dmg` |
| macOS x86_64 | `macos-13` | Xcode 15, Rust stable, JDK 21, pnpm | `.dmg` |
| Linux x86_64 | `ubuntu-22.04` | gcc, Rust stable, JDK 21, pnpm | `.AppImage`, `.deb`, `.rpm` |
| Windows x86_64 | `windows-2022` | MSVC, Rust stable, JDK 21, pnpm | `.msi` (NSIS) |

**Steps per target**:
1. Checkout + cache (Gradle, Cargo, pnpm, llama-server binaries)
2. `./gradlew :shared:jvmTest` — engine tests
3. `pnpm install && pnpm build` — frontend build
4. `pnpm tauri build` — produces platform-specific installer
5. Upload artifact (installer + debug symbols)

**Native binary bundling**:
- llama-server, whisper-server: pre-built per platform, stored in Git LFS or downloaded in CI
- Bundled via Tauri `bundle.resources` configuration:
  ```toml
  [bundle]
  resources = ["binaries/llama-server*", "binaries/whisper-server*"]
  ```
- macOS: additionally use `macOS.frameworks` for system-level dylibs if needed
- Windows: bundled in `resources/` directory alongside `.exe`
- Linux: bundled in AppImage, or `/usr/share/actual-feed/bin/` for .deb/.rpm

**Key decisions**:
- Tauri 2.x `tauri build` handles all packaging natively (DMG/NSIS/AppImage/deb/rpm)
- No Electron, no custom packaging scripts
- Debug symbols uploaded separately for crash symbolication
- Artifact retention: 30 days for branches, permanent for tags

**Acceptance**:
- [ ] Push to main → all 4 targets build green
- [ ] Each target produces a working installer
- [ ] llama-server and whisper-server binaries present in installed app
- [ ] Build completes in < 30 minutes per target

---

### Task 0.2: Code signing

**What**: Sign all release binaries for OS trust and auto-update integrity.

**Where**: `.github/workflows/release.yml`, Tauri config

**Per platform**:

| Platform | Method | Cost | Notes |
|---|---|---|---|
| macOS | Apple Developer ID (hardened runtime + notarization) | $99/yr | Tauri handles `codesign` + `xcnotary` automatically. Apple Developer account required. Secrets: `APPLE_CERTIFICATE`, `APPLE_CERTIFICATE_PASSWORD`, `APPLE_ID`, `APPLE_TEAM_ID`. |
| Windows | Azure Trusted Signing (formerly Azure Code Signing) | $120/yr ($9.99/mo) | EV equivalent without hardware token. Secrets: `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`. |
| Linux | GPG signature on .AppImage + .deb/.rpm | $0 | Sign with project GPG key. Users can verify manually. |

**Acceptance**:
- [ ] macOS DMG installs without Gatekeeper warning
- [ ] Windows MSI installs without SmartScreen warning
- [ ] Linux packages have verifiable GPG signatures
- [ ] All secrets stored as GitHub encrypted secrets, never in repo

---

### Task 0.3: Auto-update infrastructure

**What**: Ed25519-signed update delivery via Tauri's built-in updater.

**Where**: `src-tauri/tauri.conf.json` (updater config), GitHub Releases

**Behavior**:
1. On app start (after 60s delay): check `GET https://releases.actualfeed.app/latest.json`
2. Response: `{ "version": "1.0.1", "platforms": { "darwin-aarch64": { "url": "...", "signature": "..." } } }`
3. Signature: Ed25519 (Tauri generates keypair, public key embedded in app, private key in CI secrets)
4. Download in background, zero UI disruption
5. Tray icon badge when ready → "Restart to Update"
6. Crash rollback: Tauri stores previous version; 3 crashes in 30s → automatic revert (EC-154)

**Hosting**: GitHub Releases as primary. `latest.json` served from a static endpoint (GitHub Pages or Cloudflare R2). No custom server required for V1.

**Key decisions**:
- Ed25519 over RSA (smaller signatures, faster verification)
- Public key in `tauri.conf.json`, private key as `TAURI_SIGNING_PRIVATE_KEY` CI secret
- `latest.json` endpoint updated by release workflow after artifact upload
- No delta updates for V1 (full binary replacement). Delta is a V1.1 optimization.

**Acceptance**:
- [ ] Release workflow produces signed installers + `latest.json`
- [ ] App detects new version, downloads silently
- [ ] Update signature verification passes
- [ ] Crash rollback triggers after 3 rapid crashes (EC-154)
- [ ] Manual rollback possible from settings

---

### Phase 0 Definition of Done

- [ ] All 4 platform targets build green in CI
- [ ] Installers signed and notarized (macOS) / trusted (Windows)
- [ ] Auto-update endpoint live, signature verification works
- [ ] Native binaries (llama-server, whisper-server) bundled in all platforms
- [ ] Release workflow: tag → build → sign → upload → update `latest.json`

---

## Phase 1: Skeleton

**Depends on**: Nothing.
**Output**: Data flows from RSS feed to SQLite to stdout.
**Stories**: US-001, US-040, US-041, US-042. **Constraints**: EC-040, EC-041, EC-044, OC-040, OC-043.

### Task 1.1: Gradle KMP project scaffold

**What**: Initialize the Gradle project with Kotlin Multiplatform structure. Shared engine module (`shared/`) with `commonMain`, `jvmMain`, `iosMain` source sets. Desktop module (`desktop/`). Build produces a runnable JVM fat-jar.

**Where**: Project root `actual-feed/`

```
actual-feed/
  build.gradle.kts              # Root build
  settings.gradle.kts           # Module includes
  shared/
    build.gradle.kts            # KMP shared module
    src/
      commonMain/kotlin/        # Platform-agnostic engine
      jvmMain/kotlin/           # JVM-specific (sqlite-jdbc, JNI)
      iosMain/kotlin/           # Stubbed (Phase 7)
  desktop/
    build.gradle.kts            # JVM application
    src/main/kotlin/            # CLI entry point
```

**Key decisions**:
- Kotlin 2.1.x, Gradle 8.x, Java 21+ target (ADR-013)
- `kotlinx.serialization` for JSON (compile-time, no reflection)
- `kotlin.coroutines` for async (structured concurrency)
- SQLDelight plugin configured for JVM + iOS targets

**Acceptance**:
- [ ] `./gradlew :desktop:run` starts a JVM process and exits cleanly
- [ ] `./gradlew :shared:jvmTest` runs and passes with 0 tests (scaffold only)
- [ ] Kotlin version, Java target, and Gradle wrapper committed

**ADRs**: ADR-013

---

### Task 1.2: SQLDelight schema — core tables

**What**: Define the complete SQLite schema as `.sq` files. SQLDelight generates type-safe Kotlin data classes and query functions. All 5 core tables + profiles + FTS5.

**Where**: `shared/src/commonMain/sqldelight/com/actualfeed/db/`

**Tables** (from story 005):

| File | Tables | Key columns |
|---|---|---|
| `FeedItems.sq` | `feed_items` | id (UUIDv7), source_url (UNIQUE), source_platform, pipeline_state, all 13 feature signals, extraction_model_id |
| `UserInteractions.sq` | `user_interactions` | profile_id, item_id, interaction_type (enum), created_at |
| `BudgetLedger.sq` | `budget_ledger` | profile_id, content_type, seconds_consumed, device_id, period_start |
| `ObliteratedSources.sq` | `obliterated_sources` | profile_id, source_platform, author_id, source_domain, obliterated_at, restored_at |
| `ExaltedSources.sq` | `exalted_sources` | profile_id, source_platform, author_id, source_domain, exalted_at, restored_at |
| `Profiles.sq` | `profiles` | id, name, is_owner, pin_hash, maturity_threshold, maturity_overrides_json, active_preset_id |
| `Fts.sq` | `feed_items_fts` | FTS5 virtual table (title, summary, transcript, topics_text, entities_text) |
| `SignalHistograms.sq` | `signal_histograms` | model_id, signal_name, bin_index (0-99), count |
| `Tripwires.sq` | `tripwires` | id, profile_id, condition_text, severity, keywords_json, cooldown_hours, enabled |
| `TripwireFires.sq` | `tripwire_fires` | tripwire_id, item_id, confidence, fired_at |
| `TuningPresets.sq` | `tuning_presets` | id, profile_id, name, is_shipped, weights_json, taste_slider, serendipity |
| `CustomFeeds.sq` | `custom_feeds` | id, profile_id, name, filter_json, preset_id |
| `SearchHistory.sq` | `search_history` | profile_id, query_text, searched_at |
| `PipelineLog.sq` | `pipeline_log` | item_id, stage, status, error_code, error_message, started_at, completed_at |
| `PipelineStats.sq` | `pipeline_stats` | date, stage, items_processed, items_failed, avg_duration_ms |

**Key decisions**:
- UUIDv7 generated in shared Kotlin code (OC-043)
- All timestamps as Unix epoch integers (portable across platforms)
- `pipeline_state` as TEXT enum: raw, transcribing, extracting, enriched, failed
- `interaction_type` as TEXT enum: thumbs_up, thumbs_down, obliterate, exalt, skip, play, pause, more_like_this, less_like_this, already_seen, often_revisit, not_relevant_anymore, misleading_spam
- Indexes defined in `.sq` files alongside table definitions

**Acceptance**:
- [ ] `./gradlew :shared:generateSqlDelightInterface` succeeds
- [ ] Generated Kotlin data classes compile
- [ ] Schema migration 001 creates all tables
- [ ] FTS5 virtual table creation succeeds on sqlite-jdbc
- [ ] All indexes created without error

**Stories**: US-040, US-041, US-042, US-043. **Constraints**: EC-040, EC-044, OC-040, OC-043.

---

### Task 1.3: FeedItem domain model

**What**: Kotlin data classes for the FeedItem domain model in `commonMain`. Includes the 13-signal feature vector, pipeline state enum, modality enum, platform enum. Mapping between SQLDelight-generated classes and domain model.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/model/`

**Classes**:
- `FeedItem` — full enriched item
- `FeedItemSummary` — lightweight projection for feed list (id, title, author, source, score, pipeline_state, thumbnail_url)
- `FeatureVector` — 13 named signals, all `Float` in [0,1]
- `Enums`: `PipelineState`, `Modality`, `SourcePlatform`, `InteractionType`
- `Profile`, `TuningPreset`, `Tripwire`, `CustomFeed`

**Acceptance**:
- [ ] Domain classes compile in `commonMain`
- [ ] Round-trip test: insert FeedItem via SQLDelight → query → map to domain → verify all fields

**Stories**: US-040.

---

### Task 1.4: RSS source adapter (Stage 1)

**What**: First source adapter. Fetches RSS/Atom feeds, parses entries, normalizes into FeedItem schema, inserts into SQLite. Pure I/O — no ML.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/adapter/RssAdapter.kt` (interface + common logic), `shared/src/jvmMain/kotlin/com/actualfeed/adapter/RssAdapterJvm.kt` (HTTP via Ktor Client)

**Behavior**:
1. Accept feed URL list from config
2. Fetch each feed (Ktor Client, timeout 30s, retry once)
3. Parse RSS 2.0 / Atom 1.0 (ROME library on JVM)
4. Normalize: title, author, published_at, source_url (dedup key), modality=text, content_length_tokens
5. Insert into feed_items with pipeline_state=raw
6. Skip duplicates (source_url UNIQUE constraint, ON CONFLICT IGNORE)
7. Surface errors per ADR-011 (error_code, error_message)

**Acceptance**:
- [ ] Adapter fetches a real RSS feed (e.g., HN RSS) and inserts items
- [ ] Duplicate items are silently skipped (idempotent)
- [ ] Network errors produce structured error with reason code
- [ ] Items have pipeline_state=raw after insert
- [ ] Unit test with mock HTTP response

**Stories**: US-001. **Constraints**: EC-001 (no API keys), EC-004 (no silent degradation).

---

### Task 1.5: CLI verification tool

**What**: Command-line tool that queries feed_items and dumps JSON to stdout. Verifies the full data path: RSS → parse → normalize → insert → query → serialize.

**Where**: `server/src/main/kotlin/com/actualfeed/cli/Main.kt`

**Commands**:
- `actual-feed crawl` — run all configured adapters once
- `actual-feed list` — dump feed_items as JSON (id, title, source, pipeline_state)
- `actual-feed show <id>` — dump full FeedItem as JSON
- `actual-feed stats` — item count by pipeline_state, by source_platform

**Acceptance**:
- [ ] `actual-feed crawl` + `actual-feed list` shows RSS items
- [ ] `actual-feed stats` shows correct counts
- [ ] Output is valid JSON parseable by `jq`

---

### Task 1.6: Phase 1 tests

**What**: Unit tests for schema integrity, data model round-trips, RSS parsing, and dedup.

**Where**: `shared/src/jvmTest/kotlin/com/actualfeed/`

**Tests**:
- Schema: all tables created, all indexes exist, FTS5 operational
- FeedItem: insert → query → domain mapping preserves all fields
- RSS adapter: parse valid RSS, parse Atom, handle malformed XML gracefully
- Dedup: second insert of same source_url is a no-op
- UUIDv7: generated IDs are time-sortable and unique

**Acceptance**:
- [ ] `./gradlew :shared:jvmTest` — all green
- [ ] Coverage: all data model classes, all adapter error paths

---

### Phase 1 Definition of Done

- [ ] Gradle KMP project builds on macOS, Linux, Windows
- [ ] SQLDelight schema generates Kotlin, all tables created
- [ ] RSS adapter inserts items into SQLite
- [ ] CLI tool verifies end-to-end data flow
- [ ] All tests pass
- [ ] Git tag: `phase-1-skeleton`

---

## Phase 2: Engine

**Depends on**: Phase 1.
**Output**: Raw items enter, enriched items with scores exit.
**Stories**: US-002, US-003, US-004, US-020–025, US-040–045, US-050–055, US-120–126, US-150–157. **Constraints**: EC-020–023, EC-040–044, EC-050–053, EC-120–123, EC-150–154, OC-020–022, OC-040–043, OC-050–053.

### Task 2.1: llama-server HTTP client

**What**: HTTP client for llama-server (llama.cpp's built-in HTTP server). Manages server process lifecycle (start, stop, health check, restart on crash). Sends extraction prompts, receives structured JSON via `json_schema` constraint.

**Where**: `shared/src/jvmMain/kotlin/com/actualfeed/llm/LlamaServerClient.kt`, `shared/src/jvmMain/kotlin/com/actualfeed/llm/LlamaServerProcess.kt`

**Architecture decision**: HTTP server mode over JNI. Rationale:
- **Process isolation**: LLM crash cannot take down the host JVM. OOM kills the server process, not the app.
- **Structured JSON output**: llama-server's `/v1/chat/completions` supports `response_format: { type: "json_schema", json_schema: {...} }` natively. Guarantees valid JSON against schema without post-hoc parsing.
- **Always current**: llama-server binary updated independently of app releases. No JNI recompilation on llama.cpp upstream changes.
- **Debuggable**: `curl` against localhost for manual testing. Server logs separate from app logs.
- **Fallback path**: java-llama.cpp (kherud) exists as JNI fallback if HTTP proves untenable, but HTTP is the primary recommendation.

**Server lifecycle**:
1. On app start: spawn `llama-server --model <path> --port <auto> --ctx-size 32768 --n-gpu-layers 99 --threads <physical_cores - 2> --host 127.0.0.1`
2. Health check: `GET /health` every 5s. Three consecutive failures → restart.
3. On crash: log error, wait 30s, restart (max 3 restarts before `LLM_SERVER_FAILED`)
4. On app shutdown: `SIGTERM` → wait 5s → `SIGKILL`
5. Port selection: ephemeral port, written to `~/.actual-feed/llama-server.port`

**Key decisions**:
- Bundle llama-server binary per platform (macOS arm64/x86_64, Linux x86_64, Windows x86_64) via Tauri `bundle.resources`
- Model loading: server validates GGUF on startup, reports via `/health` endpoint
- Inference: `POST /v1/chat/completions` with `response_format.json_schema` set to extraction schema
- Context window: 32K tokens (Qwen3-30B-A3B)
- Timeout: 120s per inference call, configurable via HTTP client timeout
- Concurrency: 1 slot (sequential extraction). Server config: `--parallel 1`

**Acceptance**:
- [ ] Server process starts on app launch, `/health` returns `{"status":"ok"}`
- [ ] `POST /v1/chat/completions` with extraction prompt returns valid JSON matching schema
- [ ] Server OOM on 8GB machine → process dies → client detects → `LLM_OOM` error code (EC-150)
- [ ] Model file not found → server fails to start → `LLM_MODEL_NOT_LOADED` error code
- [ ] Server crash → client detects via health check → restart after 30s (story 016)
- [ ] App shutdown → server process terminated cleanly

**ADRs**: ADR-006, ADR-012.

---

### Task 2.2: whisper-server HTTP client

**What**: HTTP client for whisper-server (whisper.cpp's built-in HTTP server). Same process lifecycle pattern as Task 2.1. Sends audio files, receives transcript text with timestamps.

**Where**: `shared/src/jvmMain/kotlin/com/actualfeed/transcription/WhisperServerClient.kt`, `shared/src/jvmMain/kotlin/com/actualfeed/transcription/WhisperServerProcess.kt`

**Architecture decision**: HTTP server mode, same rationale as Task 2.1. whisper-server provides OpenAI-compatible `POST /v1/audio/transcriptions` endpoint. Process isolation is even more important here — whisper FFmpeg decoding has historically segfaulted on malformed audio.

**Server lifecycle**:
1. On first transcription request (lazy start): spawn `whisper-server --model <path> --port <auto> --host 127.0.0.1`
2. Health check: `GET /health` every 10s while active. Idle timeout: stop server after 5 minutes with no requests.
3. On crash: log error, wait 10s, restart (max 3 restarts before `TRANSCRIPTION_SERVER_FAILED`)
4. On app shutdown: `SIGTERM` → wait 3s → `SIGKILL`

**Key decisions**:
- Bundle whisper-server binary per platform via Tauri `bundle.resources`
- Lazy start: whisper-server only runs when transcription is needed (saves ~2GB RAM when idle)
- Model: Whisper Large-v3 Turbo (distil-whisper for resource-constrained machines)
- API: `POST /v1/audio/transcriptions` with `model=<loaded>`, `response_format=verbose_json` (includes segment timestamps)
- Supported input: mp3, m4a, wav, ogg, flac, webm (whisper-server handles FFmpeg decoding internally)
- Timeout: 5 minutes per file (configurable via HTTP client timeout)

**Acceptance**:
- [ ] Server starts lazily on first transcription request
- [ ] `POST /v1/audio/transcriptions` with audio file returns text + segment timestamps
- [ ] Unsupported format produces structured error from server → mapped to `TRANSCRIPTION_FORMAT_UNSUPPORTED`
- [ ] Timeout on very long file → HTTP timeout → `TRANSCRIPTION_TIMEOUT`
- [ ] Server crashes on malformed audio → client detects → restart → `TRANSCRIPTION_DECODE_ERROR`
- [ ] Server stops after 5 minutes idle (saves memory)

**ADRs**: ADR-012.

---

### Task 2.3: ONNX Runtime integration (embeddings)

**What**: Load nomic-embed-text-v1.5 (137M params) via ONNX Runtime Java. Produce 768-dim embeddings for novelty/repetition detection.

**Where**: `shared/src/jvmMain/kotlin/com/actualfeed/embedding/EmbeddingService.kt`

**Behavior**:
1. Load ONNX model file
2. Accept text (title + summary), return Float[768]
3. Batch embedding: process multiple items in one call
4. Cosine similarity function for novelty/repetition computation
5. Store embeddings in `feed_items.embed_data` (JSON-encoded or BLOB)

**Acceptance**:
- [ ] Embed a text, get 768-dim vector
- [ ] Cosine similarity between identical texts = 1.0
- [ ] Cosine similarity between unrelated texts < 0.5
- [ ] Batch of 50 items embeds in < 10s on desktop CPU

**Stories**: US-044.

---

### Task 2.4: Stage 2 extraction pipeline

**What**: The core enrichment pipeline. For each item with `pipeline_state=raw`: transcribe (if audio/video) → extract features via LLM → compute post-LLM signals → update to `enriched`. Orchestrates tasks 2.1-2.3.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/pipeline/ExtractionPipeline.kt`

**Extraction prompt**: See [extraction-prompt.md](extraction-prompt.md) for the complete prompt template, JSON schema, signal descriptions, and token budget.

**Pipeline per item**:
1. **Pre-check**: Skip if already enriched. Skip if obliterated source.
2. **Transcription** (audio/video only): Call whisper-server → store transcript. Set `pipeline_state=transcribing`.
3. **LLM extraction**: Build prompt per [extraction-prompt.md](extraction-prompt.md) with item content + user topics + active tripwire conditions. Send to llama-server `POST /v1/chat/completions` with `response_format.json_schema`. Parse structured JSON: summary, entities, claims, sources, topics, 8 feature signals, maturity score + flags, tripwire evaluations. Set `pipeline_state=extracting`.
4. **Post-LLM signals**:
   - `freshness`: `1.0 / (1.0 + hours_since_published / 168.0)` (half-life: 1 week)
   - `novelty`: `1.0 - max_cosine_similarity(item_embedding, recent_100_embeddings)`
   - `repetition`: inverse of novelty across same-source items
   - `source_authority`: lookup from user config (exalted = 1.0, default = 0.5, configurable per-source)
   - `modality_preference`: lookup from user config
5. **Percentile normalization**: Look up each signal in `signal_histograms`, compute percentile. Insert raw value into histogram. Skip if < 100 items (cold start). Content maturity exempt (ADR-015).
6. **Tripwire evaluation**: For each tripwire match with confidence >= threshold, insert into `tripwire_fires`.
7. **State update**: Set `pipeline_state=enriched`, store all extracted data.
8. **Error handling**: On any failure, set `pipeline_state=failed`, store error_code + error_message. Item remains visible with Stage 1 data (EC-150).

**Key decisions**:
- Pipeline runs on Kotlin coroutines (one coroutine per item, bounded dispatcher)
- Concurrency: 1 LLM inference at a time (model is single-threaded), N transcriptions in parallel
- Retry: exponential backoff for retryable errors, no retry during resource pressure (EC-152)
- Idempotent: same content + same model = same output (EC-041)

**Acceptance**:
- [ ] Raw RSS item → enriched with all 13 signals populated
- [ ] Audio item → transcribed → enriched
- [ ] Failed extraction → pipeline_state=failed with error_code
- [ ] Obliterated source items skipped
- [ ] Percentile normalization activates after 100 items
- [ ] Tripwire fire recorded when confidence >= threshold
- [ ] Pipeline log entries created for each stage transition

**Stories**: US-040–045, US-050–055, US-150–157. **Constraints**: EC-040–044, EC-050–053, EC-150–154. **ADRs**: ADR-006, ADR-010, ADR-011, ADR-015.

---

### Task 2.5: Stage 3 Layer 1 — Explicit formula scoring

**What**: Weighted linear combination of 12 ranking features. Hard pre-filters (obliterate, maturity) execute before scoring.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/ranking/FormulaScorer.kt`

**Behavior**:
1. Load active preset weights (12 weights, one per scoring signal)
2. Pre-filter: exclude obliterated sources, exclude items above maturity threshold for active profile
3. For each surviving item: `formula_score = Σ(weight_i × normalized_signal_i)`
4. Normalize to [0, 1]
5. Return sorted list with scores and per-signal contributions (for score breakdown)

**Acceptance**:
- [ ] Items from obliterated sources never appear in output
- [ ] Items above maturity threshold for child profiles never appear
- [ ] Negative weights on hype_score push high-hype items down
- [ ] Score breakdown shows each signal's contribution
- [ ] Different presets produce different orderings

**Stories**: US-020–022. **Constraints**: EC-020–023. **ADRs**: ADR-003.

---

### Task 2.6: Additional adapters (HN, Web Articles, Podcasts)

**What**: Three more Stage 1 adapters to reach the V1 set of 5 free sources.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/adapter/`

| Adapter | Source | Method | Notes |
|---|---|---|---|
| `HackerNewsAdapter` | Algolia HN API | HTTP GET, JSON parse | Top/best/new stories. No auth. Rate limit: respectful (1 req/sec). |
| `WebArticleAdapter` | Any URL | HTTP + readability4j | readability4j 1.0.8 (Kotlin, 80KB, Mozilla Readability.js port). Fork and update to Readability.js 0.6.0 in Phase 2. Title, author, text content, published date. |
| `PodcastAdapter` | RSS with enclosures | Same as RSS adapter | Detect `<enclosure>` tags. Set modality=audio. Extract duration from enclosure or itunes:duration. |

**Acceptance**:
- [ ] HN adapter fetches top stories, inserts as feed_items
- [ ] Web article adapter extracts readable text from a URL
- [ ] Podcast adapter detects audio enclosures, sets modality=audio, extracts duration
- [ ] All adapters produce structured errors on failure (EC-004)

**Stories**: US-001. **Constraints**: EC-001.

---

### Task 2.7: FTS5 search implementation

**What**: Full-text search across 5 columns with BM25 ranking, snippet extraction, and structured filters.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/search/SearchService.kt`

**Behavior**:
1. Accept query string, sanitize (escape FTS5 operators — EC-121)
2. Execute FTS5 MATCH with BM25 weights (title=10, summary=5, transcript=1, entities=3, topics=3)
3. JOIN with feed_items for structured filters (platform, modality, date range, score range)
4. Return ranked results with highlighted snippets
5. Search history: insert query into `search_history`
6. FTS5 content sync: trigger rebuild on enrichment update

**Acceptance**:
- [ ] Search "database" returns items mentioning databases, ranked by BM25
- [ ] Snippets show `<mark>` around matched terms
- [ ] Porter stemming: "indexing" matches "index"
- [ ] Search latency < 100ms for 500K items
- [ ] SQL injection via FTS5 operators sanitized (EC-121)
- [ ] Filters (platform, modality, date) narrow results correctly

**Stories**: US-120–126. **Constraints**: EC-120–123. **ADRs**: ADR-001.

---

### Task 2.8: Content maturity extraction

**What**: Ensure every item gets a maturity score regardless of parental controls state. This is part of the Stage 2 prompt but deserves separate validation.

**Where**: Part of extraction prompt in Task 2.4. Validation logic in `shared/src/commonMain/kotlin/com/actualfeed/maturity/MaturityFilter.kt`.

**Behavior**:
1. LLM prompt always includes maturity assessment block (EC-101)
2. Parse: overall score [0,1] + category flags (violence, sexual, language, substances, graphic, mature_themes) + reason text
3. Content maturity uses RAW scores (not percentile-normalized — ADR-015 exception)
4. Maturity filter: for active profile, exclude items where `content_maturity > profile.maturity_threshold` OR any category score exceeds category override threshold
5. Conservative bias: prompt instructs LLM to err on the side of higher maturity scores for ambiguous content (EC-104)

**Acceptance**:
- [ ] Every enriched item has content_maturity and maturity_flags_json populated
- [ ] Child profile with threshold=0.3: no items with maturity > 0.3 visible
- [ ] Category overrides work: violence=0.2 filters more aggressively for violence
- [ ] Raw scores used (not percentile-normalized)
- [ ] Ambiguous content scores higher rather than lower

**Stories**: US-100–106. **Constraints**: EC-100–104. **ADRs**: ADR-015 (exception clause).

---

### Task 2.9: Error recovery infrastructure

**What**: Error taxonomy, pipeline logging, retry mechanisms, resource pressure detection.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/pipeline/ErrorRecovery.kt`, `shared/src/commonMain/kotlin/com/actualfeed/pipeline/PipelineLogger.kt`

**Behavior**:
1. Error taxonomy: 27 reason codes across 5 categories (network, LLM, transcription, embedding, storage) — from story 016
2. Pipeline log: append-only, per-item-per-stage entries with timing
3. Retry: exponential backoff [10s, 60s, 300s] for retryable errors, max 3 attempts
4. Resource pressure: pause pipeline when available memory < 10% of total RAM (EC-152)
5. Graceful degradation: failed items retain Stage 1 data, show "Analysis failed" badge (OC-150)
6. Pipeline stats: daily aggregates materialized for trending
7. Log GC: delete entries older than 30 days (EC-153)

**Acceptance**:
- [ ] Every failure produces a structured error (code + message)
- [ ] Retryable errors retry with backoff
- [ ] Resource pressure pauses pipeline, does not burn retry attempts
- [ ] Pipeline log grows per-stage, garbage-collected at 30 days
- [ ] Failed items visible in feed with Stage 1 data

**Stories**: US-150–157. **Constraints**: EC-150–154, OC-150–152. **ADRs**: ADR-011.

---

### Phase 2 Definition of Done

- [ ] RSS item → transcription (if audio) → LLM extraction → 13 signals → formula score
- [ ] All 5 adapters (RSS, HN, Web Articles, Podcasts, YouTube deferred to Phase 3) operational
- [ ] FTS5 search returns ranked, highlighted results
- [ ] Maturity filtering works for child profiles
- [ ] Tripwires fire on matching content
- [ ] Percentile normalization activates after 100 items
- [ ] Error recovery: failures surfaced, retried, gracefully degraded
- [ ] CLI: `actual-feed crawl && actual-feed list` shows enriched, scored items
- [ ] All tests pass
- [ ] Git tag: `phase-2-engine`

---

## Phase 3: Desktop MVP

**Depends on**: Phase 2.
**Output**: First shippable product. Usable feed in a desktop window.
**Stories**: US-010–018, US-030–036, US-050–055, US-130–138. **Constraints**: EC-010–015, EC-030–034, EC-050–053, EC-130–133.

### Task 3.1: Tauri 2.x project scaffold

**What**: Initialize Tauri desktop shell. System webview renders SolidJS frontend. Tauri manages window lifecycle, system tray, auto-update, single-instance.

**Where**: `desktop/src-tauri/` (Rust glue), `desktop/src/` (SolidJS + TypeScript)

**Setup**:
- Tauri 2.x with single-instance plugin
- SolidJS + TypeScript + Vite
- Plain CSS with custom properties (no framework, no component library)
- Window: 1200x800 default, resizable, remember position/size
- System tray icon with context menu (show/hide, quit)

**Acceptance**:
- [ ] `pnpm tauri dev` opens a desktop window with SolidJS content
- [ ] System tray icon appears
- [ ] Window remembers position/size across restarts
- [ ] Single instance: second launch focuses existing window

**Stories**: US-130–132. **Constraints**: EC-130, EC-131. **ADRs**: ADR-013.

---

### Task 3.2: JSON-RPC 2.0 server (Ktor)

**What**: The Kotlin/JVM backend serves JSON-RPC over Unix domain socket (Linux/macOS) and HTTP localhost (Windows). Same API consumed by Tauri and (later) Emacs.

**Where**: `server/src/main/kotlin/com/actualfeed/server/JsonRpcServer.kt`

**API methods**:

| Method | Params | Returns | Story |
|---|---|---|---|
| `feed.getItems` | profile_id, preset_id, offset, limit | FeedItemSummary[] | US-010 |
| `feed.getItem` | item_id | FeedItem (full) | US-011 |
| `feed.getScoreBreakdown` | item_id, preset_id | ScoreBreakdown | US-012 |
| `feed.interact` | item_id, interaction_type, profile_id | void | US-013 |
| `search.query` | query, filters, limit | SearchResult[] | US-120 |
| `search.findSimilar` | item_id, limit | FeedItemSummary[] | US-125 |
| `tripwires.list` | profile_id | Tripwire[] | US-050 |
| `tripwires.create` | condition_text, severity, keywords | Tripwire | US-051 |
| `tripwires.update` | tripwire_id, fields | Tripwire | US-051 |
| `tripwires.delete` | tripwire_id | void | US-051 |
| `tripwires.fires` | profile_id, since | TripwireFire[] | US-052 |
| `pipeline.status` | — | PipelineStatus | US-150 |
| `pipeline.retryItem` | item_id | void | US-153 |
| `pipeline.retryAll` | — | void | US-153 |
| `settings.getConfig` | — | Config | US-130 |
| `settings.updateConfig` | fields | Config | US-130 |
| `adapters.list` | — | Adapter[] | US-001 |
| `adapters.toggleSource` | source_id, enabled | void | US-001 |
| `profiles.list` | — | Profile[] | US-100 |
| `profiles.create` | name, maturity_threshold, pin | Profile | US-100 |
| `profiles.switchTo` | profile_id, pin | void | US-101 |
| `presets.list` | profile_id | Preset[] | US-140 |
| `presets.create` | name, weights | Preset | US-141 |
| `presets.update` | preset_id, weights | Preset | US-141 |
| `budgets.getStatus` | profile_id | BudgetStatus | US-030 |
| `budgets.configure` | profile_id, limits | void | US-031 |
| `autoplay.getQueue` | profile_id, mode | FeedItemSummary[] | US-110 |
| `autoplay.control` | action (play/pause/skip/thumbs) | void | US-111 |

**Key decisions**:
- Ktor Server with Unix domain socket transport (Ktor UDS support via ktor-server-cio)
- HTTP localhost fallback for Windows
- JSON-RPC 2.0 spec: id, method, params, result/error
- Authentication: none (local machine only, PID-validated socket)
- Error responses: JSON-RPC error codes map to pipeline error codes

**Acceptance**:
- [ ] Server starts and listens on Unix domain socket
- [ ] `feed.getItems` returns enriched, scored items
- [ ] `feed.interact` with thumbs_up inserts interaction record
- [ ] `search.query` returns BM25-ranked results
- [ ] Invalid method returns JSON-RPC -32601 error
- [ ] Tauri frontend connects and renders feed

**Constraints**: EC-070 (same API for all clients).

---

### Task 3.3: Feed rendering (SolidJS)

**What**: Main feed view. Infinite scroll with virtualized list. Rich cards showing summary, source, author, timestamp, score indicator, thumbnail.

**Where**: `desktop/src/components/Feed.tsx`, `desktop/src/components/FeedCard.tsx`

**Components**:
- `<Feed>`: Virtualized list (solid-virtual), fetches pages via JSON-RPC
- `<FeedCard>`: title, summary (2-4 lines), source icon + name, author, relative timestamp, score bar, thumbnail, pipeline_state badge
- `<ScoreBreakdown>`: click/hover overlay showing per-signal contributions
- `<InteractionBar>`: thumbs up/down, obliterate/exalt gesture target, kebab menu
- `<KebabMenu>`: "More like this", "Less like this", "Already seen", "Often revisit", "Not relevant", "Misleading/spam", "Why recommended?"
- Obliterate animation: shake → crack → shatter → collapse (CSS + JS)
- Exalt badge: star/crown icon on exalted source items
- Failed item badge: "Analysis failed" with reason and retry button

**Acceptance**:
- [ ] Feed loads and scrolls smoothly with 1000+ items
- [ ] Score breakdown shows on click
- [ ] Thumbs up/down records interaction
- [ ] Rapid thumbs-down within 2000ms triggers obliterate UI
- [ ] Rapid thumbs-up within 2000ms triggers exalt UI
- [ ] Obliterate animation plays, source removed from feed
- [ ] Kebab menu options all record interactions
- [ ] Failed items show badge with retry button
- [ ] "Why recommended?" shows score breakdown

**Stories**: US-010–018, US-015b, US-015c. **Constraints**: EC-010–015.

---

### Task 3.4: YouTube adapter + platform embedding

**What**: YouTube adapter (yt-dlp) for Stage 1, plus YouTube IFrame Player API embedding in the desktop UI.

**Where**: `shared/src/jvmMain/kotlin/com/actualfeed/adapter/YoutubeAdapter.kt`, `desktop/src/components/YoutubeEmbed.tsx`

**Adapter behavior**:
1. Accept YouTube channel/playlist URLs from config
2. Shell out to yt-dlp: `yt-dlp --dump-json --flat-playlist <url>`
3. Parse JSON: title, description, uploader, upload_date, duration, thumbnail_url, video_id
4. For transcripts: `yt-dlp --write-auto-sub --sub-lang en --skip-download <url>` → parse .vtt
5. Normalize into FeedItem schema, modality=video
6. Fallback: if yt-dlp fails, try YouTube oEmbed for basic metadata

**Embed behavior**:
1. IFrame Player API in Tauri webview
2. `player.pauseVideo()` for budget enforcement (no overlay — EC-030)
3. Native YouTube controls and branding preserved
4. Pause-then-collapse on budget exceeded

**Acceptance**:
- [ ] YouTube adapter fetches video metadata and transcripts via yt-dlp
- [ ] YouTube embed plays video in desktop window
- [ ] Budget enforcement pauses playback programmatically
- [ ] No overlay on YouTube player at any time (EC-030)
- [ ] yt-dlp failure produces structured error with reason code

**Stories**: US-001, US-030–033. **Constraints**: EC-001, EC-030–034. **ADRs**: ADR-004.

---

### Task 3.5: Tripwire UI

**What**: CRUD interface for tripwires. Fire notifications. Fire history view.

**Where**: `desktop/src/components/Tripwires.tsx`, `desktop/src/components/TripwireForm.tsx`

**Behavior**:
- List active tripwires with enabled/disabled toggle
- Create: text input for condition, severity selector, optional keywords, cooldown
- Edit/delete existing tripwires
- Fire notifications: OS desktop notification (Tauri notification plugin) with tripwire name, item title, confidence
- Fire history: chronological list of tripwire fires with item links
- Shipped tripwire packs: importable from a menu

**Acceptance**:
- [ ] Create a tripwire "Any CVE affecting Spring Framework"
- [ ] Tripwire fires on matching item during extraction
- [ ] Desktop notification appears with tripwire name and item title
- [ ] Fire history shows all past fires
- [ ] Tripwire can be disabled/re-enabled

**Stories**: US-050–055. **Constraints**: EC-050–053, OC-050–053.

---

### Task 3.6: Settings UI (basic)

**What**: Settings panel for source management, topic declaration, model selection, and crawl schedule.

**Where**: `desktop/src/components/Settings.tsx`, subcomponents for each section

**Sections**:
- **Sources**: Grid of available adapters. Toggle enabled/disabled. Add RSS feed URLs. Compute budget bar updates live as sources toggle.
- **Topics**: Text input for interests. Suggested from sources. Seeds topic_relevance.
- **Model**: Three tiers (Recommended / Balanced / Lightweight). Auto-detected by RAM. Model download status. Current model info.
- **Crawl**: Schedule selector (every 15min, 30min, 1h, 2h, manual only).
- **Obliterated sources**: List with restore option.
- **Exalted sources**: List with un-exalt option.

**Acceptance**:
- [ ] Toggle a source on/off, feed updates accordingly
- [ ] Add an RSS feed URL, items appear after crawl
- [ ] Change crawl schedule, next crawl respects new interval
- [ ] Obliterated sources visible and restorable
- [ ] Exalted sources visible and reversible

**Stories**: US-130–138. **Constraints**: EC-130–133, OC-130–133.

---

### Task 3.7: Web article + podcast embedding

**What**: Platform embedding for web articles (inline Readability render) and podcasts (HTML5 audio player with budget tracking).

**Where**: `desktop/src/components/ArticleEmbed.tsx`, `desktop/src/components/PodcastPlayer.tsx`

**Acceptance**:
- [ ] Web article renders in clean Readability format within the app
- [ ] Podcast plays audio with play/pause/seek controls
- [ ] Podcast playback time tracked in budget_ledger
- [ ] Budget enforcement pauses podcast playback

**Stories**: US-030–036. **ADRs**: ADR-004.

---

### Phase 3 Definition of Done

- [ ] Desktop window opens showing a ranked feed from 5 sources
- [ ] Items are interactive: thumbs up/down, obliterate, exalt, kebab menu
- [ ] YouTube videos play with budget enforcement
- [ ] Web articles render inline
- [ ] Podcasts play with budget tracking
- [ ] Tripwires: create, fire, notify
- [ ] Search: query, filter, highlighted results
- [ ] Settings: sources, topics, model, crawl schedule
- [ ] Score breakdown for every item
- [ ] Failed items visible with retry
- [ ] Git tag: `phase-3-desktop-mvp`

---

## Phase 4: Desktop Complete (V1 Ship)

**Depends on**: Phase 3.
**Output**: Feature-complete desktop product.
**Stories**: All remaining US not covered by Phases 1-3. **Constraints**: All remaining EC/OC not covered by Phases 1-3.

### Task 4.1: Stage 3 Layer 2 — Neural taste model

**What**: Small MLP trained on user interactions. Discovers nonlinear preferences. ONNX Runtime inference, DJL training.

**Where**: `shared/src/jvmMain/kotlin/com/actualfeed/ranking/NeuralTasteModel.kt`

**Architecture**:
- Input: 12 normalized feature signals
- Hidden: 2 layers, 64 units each, ReLU
- Output: 1 unit, sigmoid → [0, 1] preference score
- Training: mini-batch SGD from interaction log (thumbs_up=1, thumbs_down=0, skip=0.3)
- Online learning: retrain incrementally on each new batch of 10+ interactions
- Export: ONNX format (~1MB)
- Reset: user can clear model and start fresh

**Acceptance**:
- [ ] After 50 thumbs up/down interactions, model produces differentiated scores
- [ ] Taste slider at 0%: pure formula. At 100%: pure neural. At 50%: blended.
- [ ] Model retrains in < 5s on 1000 interactions
- [ ] Model reset clears weights, feed reverts to formula-only
- [ ] Score breakdown shows neural adjustment direction and magnitude

**Stories**: US-023–024. **Constraints**: OC-021. **ADRs**: ADR-003.

---

### Task 4.2: Stage 3 Layer 3 — Thompson sampling exploration

**What**: Probabilistic exploration layer. Surfaces items from outside the top-N to prevent filter bubbles.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/ranking/ExplorationSampler.kt`

**Behavior**:
1. For each item, maintain a Beta distribution from interactions (α = positive, β = negative)
2. Sample from each distribution → exploration score
3. Serendipity slider controls blend: `final = (1 - s) × [taste × neural + (1-taste) × formula] + s × exploration`
4. Exploration items get a visual indicator in the feed ("New territory" badge)
5. Max serendipity: 50%

**Acceptance**:
- [ ] At serendipity=0%: feed is deterministic (formula + neural only)
- [ ] At serendipity=50%: roughly half the items are exploration picks
- [ ] Exploration items have visual indicator
- [ ] Interacting positively with exploration items updates their Beta distribution

**Stories**: US-025. **ADRs**: ADR-003.

---

### Task 4.3: Tuning presets

**What**: 6 shipped defaults + user CRUD. Per-feed assignment. Export/import.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/preset/PresetService.kt`, `desktop/src/components/PresetEditor.tsx`

**Shipped presets** (from story 015):

| Preset | topic_relevance | credibility | novelty | hype_score | vagueness | taste_slider | serendipity |
|---|---|---|---|---|---|---|---|
| deliberate | 0.8 | 0.9 | 0.3 | -0.7 | -0.6 | 0.20 | 0.05 |
| discovery | 0.3 | 0.5 | 0.9 | -0.2 | -0.1 | 0.60 | 0.40 |
| balanced | 0.6 | 0.7 | 0.5 | -0.4 | -0.3 | 0.40 | 0.15 |
| deep_research | 0.9 | 0.95 | 0.4 | -0.9 | -0.8 | 0.30 | 0.05 |
| breaking_news | 0.5 | 0.4 | 0.1 | 0.0 | 0.0 | 0.50 | 0.20 |
| background_audio | 0.4 | 0.6 | 0.6 | -0.5 | -0.4 | 0.50 | 0.25 |

**Behavior**:
- Shipped presets are read-only (is_shipped=1). Attempting modification returns error (EC-140).
- User can clone a shipped preset to create editable copy
- Presets assignable per custom feed
- Export as JSON, import from JSON
- Live preview: changing weights re-ranks feed in real-time (< 2s for 1000 items)

**Acceptance**:
- [ ] All 6 shipped presets available on first launch
- [ ] Shipped presets cannot be modified (EC-140)
- [ ] Clone → edit → save creates user preset
- [ ] Switching preset re-ranks feed immediately
- [ ] Export/import round-trips correctly
- [ ] Live preview shows re-ranked feed within 2s

**Stories**: US-140–147. **Constraints**: EC-140–142, OC-140–143. **ADRs**: ADR-003.

---

### Task 4.4: Content budgets

**What**: Per-content-type time limits with soft/hard modes and pause-then-collapse enforcement.

**Where**: `shared/src/commonMain/kotlin/com/actualfeed/budget/BudgetService.kt`, `desktop/src/components/BudgetBar.tsx`

**Behavior**:
- Budget types: video_minutes, audio_minutes, social_minutes, article_count
- Periods: daily (default), weekly, rolling_24h
- Soft budget (default): warning at 80%, dismissable pause at 100%
- Hard budget (PIN-locked): non-dismissable pause at 100%
- Day-of-week profiles: weekday vs weekend budgets
- Enforcement: `player.pauseVideo()` → collapse embed → show budget UI in vacated space
- Budget bar: colored segments per content type, current consumption, time until reset

**Acceptance**:
- [ ] Set video budget to 60 minutes. At 48min (80%): warning badge
- [ ] At 60min: video pauses, embed collapses, budget UI shows
- [ ] Soft budget: user can dismiss and continue
- [ ] Hard budget: requires PIN to override
- [ ] Budget bar shows accurate consumption per type
- [ ] Cross-type: audio budget independent of video budget

**Stories**: US-030–036. **Constraints**: EC-030–034, OC-030–032. **ADRs**: ADR-005.

---

### Task 4.5: Profiles + parental controls UI

**What**: Multi-profile support with maturity filtering, PIN protection, filtered content review.

**Where**: `desktop/src/components/ProfileSwitcher.tsx`, `desktop/src/components/ParentalControls.tsx`

**Behavior**:
- Owner profile created during onboarding
- Create child profile: name, avatar, maturity threshold slider (labeled: Young children 0.2, Teens 0.5, Mature teens 0.7, Unfiltered 1.0)
- Per-category overrides under "Advanced"
- Profile switching: PIN entry for owner profile access
- Child profiles: identical UI, no "kids mode" aesthetic
- Filtered content review: parent can see what was filtered for each child
- Whitelist: parent can approve specific items for a child
- Model swap: child profiles freeze maturity scores until parent confirms new model
- Weekly digest: borderline items (within 0.1 of threshold) surfaced to parent

**Acceptance**:
- [ ] Create child profile with threshold=0.3
- [ ] Child profile feed shows no items with maturity > 0.3
- [ ] Parent can review filtered items
- [ ] Parent can whitelist specific items
- [ ] Profile switch requires PIN
- [ ] Model swap freezes child maturity scores until parent confirms

**Stories**: US-100–106. **Constraints**: EC-100–104, OC-100–103.

---

### Task 4.6: Autoplay / audio feed

**What**: Four playback modes with TTS, queue management, hardware controls, budget enforcement during playback.

**Where**: `desktop/src/components/AutoplayPlayer.tsx`, `shared/src/jvmMain/kotlin/com/actualfeed/autoplay/AutoplayService.kt`

**Modes**:
- **Scan**: Piper TTS reads summaries. 15-20 items per 10 minutes.
- **Deep**: Full original content (video/audio plays natively, articles TTS'd)
- **Background**: Short items (< 5min) play full, long items get TTS summary
- **Project-focused**: Filter queue to active custom feed/preset

**TTS**: Piper (ONNX, MIT license, ~50MB per voice). Local-first.

**Hardware controls**: Media key mapping via Tauri:
- Play/pause → single press
- Skip → double press
- Thumbs up → triple press / long press

**Acceptance**:
- [ ] Scan mode: TTS reads summaries sequentially
- [ ] Deep mode: YouTube video plays, then next item
- [ ] Background mode: short podcast plays full, long article gets TTS summary
- [ ] Hardware media keys control playback
- [ ] Budget enforcement pauses autoplay when budget exceeded
- [ ] Tripwire fire interrupts autoplay with notification

**Stories**: US-110–115. **Constraints**: EC-110–112, OC-110–112.

---

### Task 4.7: Onboarding flow

**What**: 4-6 screen first-run experience.

**Where**: `desktop/src/components/Onboarding.tsx` (multi-step wizard)

**Screens**:
1. **Activation**: Email + password sign-in (or skip for local-only evaluation)
2. **Sources**: Grid with checkboxes. 5 free sources. Compute budget bar live.
3. **Topics**: Text input for interests. Suggestions from selected sources.
4. **Model**: Auto-detected by RAM. Three tiers shown. Download progress.
5. **Initial Crawl**: Live feed building — items stream in, animate from raw to enriched
6. **Optional**: Budgets, tripwires, presets. "Set up later" dismisses.

**Bundled fallback**: Qwen2.5-3B Q4 (~2GB) for immediate enrichment while recommended model downloads.

**Acceptance**:
- [ ] Fresh install → onboarding flow opens
- [ ] Source selection updates compute budget bar
- [ ] Model auto-selected based on RAM
- [ ] Initial crawl shows live item processing
- [ ] First usable feed within 5 minutes on 100 Mbps (EC-090)
- [ ] "Skip" works on every step except activation
- [ ] Second-device setup: sign in → sync → done (skips onboarding)

**Stories**: US-090–095. **Constraints**: EC-090–093, OC-090–093.

---

### Task 4.8: Custom feeds

**What**: User-created filtered views by modality, source, or topic. Each with its own preset.

**Where**: `desktop/src/components/CustomFeedEditor.tsx`

**Behavior**:
- Create feed with name + filters (modality, source platform, topic tags)
- Assign a tuning preset
- Max 20 custom feeds per profile
- Sidebar navigation between main feed and custom feeds
- Each custom feed shows item count and last update

**Acceptance**:
- [ ] Create "Videos Only" feed → shows only video items
- [ ] Create "YouTube + Podcasts" feed → shows items from those sources
- [ ] Assign "deep_research" preset to a custom feed → different ranking
- [ ] 20 custom feed limit enforced

**Stories**: US-145–147. **Constraints**: OC-140.

---

### Task 4.9: Compute budget bar + pipeline dashboard

**What**: Real-time visualization of pipeline health.

**Where**: `desktop/src/components/ComputeBudgetBar.tsx`, `desktop/src/components/PipelineDashboard.tsx`

**Budget bar**:
- Horizontal bar with colored segments: crawling (blue), transcription (green), summarization (orange), ranking (purple), storage (gray)
- Percentages per segment
- Updates live as sources toggled or model changed
- Recalibrates on startup based on device capabilities

**Dashboard** (accessible from status chip on main feed):
- Per-stage queue depth
- Processing rates (items/hour)
- Error count with breakdown by error code
- Model health (tok/s, memory usage)
- Retry queue
- Pipeline log (last 100 events)

**Acceptance**:
- [ ] Budget bar shows proportional segments
- [ ] Toggling a source updates bar in real-time
- [ ] Dashboard shows queue depth, processing rates, error counts
- [ ] Retry button on dashboard retries all failed items
- [ ] Status chip shows queue depth and error count

**Stories**: US-150–157.

---

### Task 4.10: Desktop keyboard shortcuts + command palette

**What**: Full keyboard navigation. Command palette (Ctrl+K / Cmd+K).

**Where**: `desktop/src/components/CommandPalette.tsx`, keyboard shortcut registration in Tauri

**Key bindings**:
- `j/k` or `↑/↓`: navigate feed items
- `Enter`: open item detail
- `Escape`: back to feed
- `s`: toggle score breakdown
- `t`: thumbs up, `T`: thumbs down
- `/`: focus search
- `Ctrl+K` / `Cmd+K`: command palette
- `1-6`: switch tuning preset
- `b`: toggle budget bar
- `p`: pipeline dashboard
- `Ctrl+,` / `Cmd+,`: settings

**Command palette**: fuzzy search across all actions, items, sources, presets.

**Acceptance**:
- [ ] Navigate entire app without mouse
- [ ] Command palette finds actions, items, settings
- [ ] All keyboard shortcuts work

**Stories**: US-135–137. **Constraints**: EC-132.

---

### Task 4.11: Auto-update + crash rollback

**What**: Background update downloads, restart prompt, crash rollback.

**Where**: Tauri updater plugin configuration

**Behavior**:
- Check for updates on startup (silent)
- Download in background, no UI disruption
- Tray icon badge when update ready
- "Restart to Update" when user is ready
- Crash rollback: 3 crashes within 30s → restore previous version (EC-154)
- Release notes shown before restart

**Acceptance**:
- [ ] Update downloads without interrupting workflow
- [ ] Tray badge appears when update ready
- [ ] Crash rollback activates after 3 rapid crashes
- [ ] Previous version restored with user notification

**Stories**: US-138. **Constraints**: EC-133, EC-154.

---

### Phase 4 Definition of Done

- [ ] All 103 V1 user stories satisfied
- [ ] All 63 enforced constraints testable and passing
- [ ] All 57 opinionated constraints implemented
- [ ] Neural taste model trains from interactions and differentiates items
- [ ] Thompson sampling exploration with serendipity slider
- [ ] 6 shipped presets available, user presets CRUD works
- [ ] Content budgets: soft + hard, pause-then-collapse enforcement
- [ ] Profiles: owner + child, maturity filtering, PIN, filtered review
- [ ] Autoplay: 4 modes, TTS, hardware controls
- [ ] Onboarding: 4-6 screens, fallback model, first feed < 5 min
- [ ] Custom feeds with per-feed presets
- [ ] Pipeline dashboard + compute budget bar
- [ ] Full keyboard navigation + command palette
- [ ] Auto-update + crash rollback
- [ ] Obliterate animation + exalt badge
- [ ] Kebab menu: all 7 tune-my-recommendations options
- [ ] Cross-platform: macOS, Linux, Windows builds pass
- [ ] Performance: feed scrolls at 60fps, search < 100ms, preset switch < 2s
- [ ] Git tag: `v1.0.0`

---

## Post-V1 Phases (V1.1+)

### Phase 5: Android
**Depends on**: Phase 2 (shared engine). Parallelizable with Phases 4, 6.

| Task | What | Key |
|---|---|---|
| 5.1 | Android project scaffold (Jetpack Compose) | Shared engine as Android library |
| 5.2 | Mobile-tier models (Phi-4 mini / Qwen2.5-3B) | Battery-aware scheduling |
| 5.3 | Moonshine ASR integration | ONNX Runtime Mobile, charging only |
| 5.4 | Mobile embedding model (all-MiniLM-L6-v2, 22M) | Reduced memory footprint |
| 5.5 | Feed + detail + search UI (Compose) | Material 3, mobile gestures |
| 5.6 | Budget enforcement via Android media APIs | MediaSession, notification controls |
| 5.7 | Profile support + maturity filtering | Same shared engine code |
| 5.8 | Notification system for tripwire fires | Android notification channels |

**Stories**: US-002. **ADRs**: ADR-012, ADR-013.

---

### Phase 6: Cloud & Sync
**Depends on**: Phase 2 (shared engine). Parallelizable with Phase 5.

| Task | What | Key |
|---|---|---|
| 6.1 | Auth service (Ktor server) | JWT, session management, 5-device limit |
| 6.2 | Sync relay | Encrypted change event store (S3-compatible), pull-then-push |
| 6.3 | Per-table conflict resolution | feed_items: extraction_version wins, interactions: append-only, obliterated: LWW, presets: sync_version + conflict log |
| 6.4 | Cloud crawler | Same shared engine, virtual device in sync protocol, 24/7 |
| 6.5 | Subscription infrastructure | Stripe, à la carte source pricing |
| 6.6 | Paid adapters: Twitter/X, Reddit | X API v2 ($100/mo amortized), Reddit paid tier |
| 6.7 | Premium tripwires (continuous evaluation) | Server-side, always-on |
| 6.8 | Subscription lapse handling | Local-only fallback, 180-day retention |
| 6.9 | Desktop client sync integration | Sync UI in settings, conflict log viewer |

**Stories**: US-060–065. **Constraints**: EC-060–063, OC-060–063. **ADRs**: ADR-002, ADR-007.

---

### Phase 7: iOS
**Depends on**: Phase 2 (shared engine), Phase 6 (sync). Parallelizable with Phase 5.

| Task | What | Key |
|---|---|---|
| 7.1 | iOS project scaffold (SwiftUI) | Import shared engine as native .framework |
| 7.2 | Kotlin/Native compilation + cinterop | llama.cpp, whisper.cpp via C FFI |
| 7.3 | ONNX Runtime iOS | Embeddings + neural taste inference |
| 7.4 | Feed + detail + search + settings UI | SwiftUI, iOS gestures |
| 7.5 | Critical Alert entitlement | Tripwire notifications |
| 7.6 | App Store deployment | Review, TestFlight |

**Stories**: US-003. **ADRs**: ADR-012, ADR-013.

---

### Phase 8: Emacs
**Depends on**: Phase 2 (engine) + JSON-RPC server from Phase 3.

| Task | What | Key |
|---|---|---|
| 8.1 | actual-feed.el scaffold | JSON-RPC 2.0 client, UDS/HTTP transport |
| 8.2 | Feed buffer | Ranked items, text properties, keyboard nav |
| 8.3 | Item detail buffer | Full extraction: summary, entities, claims, features |
| 8.4 | Search buffer | FTS5 query, highlighted snippets |
| 8.5 | Tripwire buffer | CRUD, fire history |
| 8.6 | Budget buffer | ASCII progress bars |
| 8.7 | Pipeline buffer | Status, model info, re-extraction trigger |
| 8.8 | Org-mode integration | Capture templates, `[[actual-feed:id]]` links, agenda TODOs from tripwire fires, tag inheritance |
| 8.9 | mpv.el integration | Media playback, budget tracking via IPC |
| 8.10 | Evil keybindings | evil-define-key for all buffer types |
| 8.11 | consult/embark integration | Completing-read search, contextual actions |

**Stories**: US-070–075. **Constraints**: EC-070–072, OC-070–072. **ADRs**: ADR-009.

---

## Agent Execution Strategy

### Principles

1. **File ownership is exclusive.** Every source file has exactly one owner agent at any given time. No two agents write to the same file concurrently. Ownership is assigned at task granularity, not negotiated at runtime.

2. **Interface-first.** Before parallel agents begin coding, the lead writes or approves the interface contracts they depend on: trait/interface signatures, data class shapes, enum values. This happens in a brief sequential "contract phase" at the start of each parallelizable wave.

3. **Shared files have a single owner.** Files touched by multiple agents (`Enums.kt`, `.sq` schema files, `FeatureVector.kt`) are owned by one designated agent per phase. Other agents declare what they need added; the owner agent makes the edit.

4. **Phase gates are hard.** No agent from Phase N+1 spawns until Phase N's gate criteria pass: all tests green, all acceptance criteria checked, lead reviews the diff.

5. **Tests live with the code they test.** The agent that writes `FooService.kt` writes `FooServiceTest.kt`. Tests are not a separate task assigned to a separate agent.

---

### Phase 0: Distribution & CI

**Topology**: 1 agent (solo)
**Reason**: Small scope (3 tasks), all CI config files. No code conflicts possible.

| Agent | Tasks | Owns |
|---|---|---|
| `ci-agent` | 0.1, 0.2, 0.3 | `.github/workflows/*`, `desktop/src-tauri/tauri.conf.json` (updater section only) |

**Runs in parallel with Phase 1.**

---

### Phase 1: Skeleton

**Topology**: 2 agents, mostly sequential with one parallel window
**Reason**: Each task produces the input for the next. The dependency chain is tight.

**Wave 1 (sequential, lead agent):**

| Agent | Tasks | Owns |
|---|---|---|
| `skeleton-lead` | 1.1, 1.2, 1.3 | `build.gradle.kts`, `settings.gradle.kts`, `gradle.properties`, `shared/build.gradle.kts`, `shared/src/commonMain/sqldelight/**`, `shared/src/commonMain/kotlin/com/actualfeed/model/**`, `shared/src/jvmMain/kotlin/com/actualfeed/db/**` |

Task 1.1 → 1.2 → 1.3 are sequential. Schema must exist before domain model. Domain model must exist before adapters.

**Wave 2 (parallel, after 1.3 completes):**

| Agent | Tasks | Owns |
|---|---|---|
| `skeleton-lead` | 1.5 (CLI), 1.6 (tests) | `server/**`, `shared/src/jvmTest/**` |
| `adapter-agent` | 1.4 (RSS adapter) | `shared/src/commonMain/kotlin/com/actualfeed/adapter/**`, `shared/src/jvmMain/kotlin/com/actualfeed/adapter/**` |

**Interface contract** (written by lead before wave 2):
```kotlin
// shared/src/commonMain/kotlin/com/actualfeed/adapter/SourceAdapter.kt
interface SourceAdapter {
    val platform: SourcePlatform
    suspend fun crawl(): List<RawItem>
}
```
`adapter-agent` implements this interface. Lead writes CLI that calls it.

**Shared file owner**: `skeleton-lead` owns `Enums.kt` and all `.sq` files. `adapter-agent` requests enum additions via message.

**Phase gate**:
- [ ] `./gradlew :shared:jvmTest` all green
- [ ] `actual-feed crawl && actual-feed list` produces JSON output
- [ ] `actual-feed stats` shows correct counts
- [ ] Lead reviews full diff

---

### Phase 2: Engine

**Topology**: 4 agents, two waves
**Reason**: Phase 2 has the most parallelism opportunity. Tasks 2.1-2.3 are independent I/O bridges. Tasks 2.5-2.7 are independent engine components. Task 2.4 (extraction pipeline) orchestrates everything and must go second.

**Wave 1 (parallel, all independent):**

| Agent | Tasks | Owns |
|---|---|---|
| `llm-agent` | 2.1 (llama-server client) | `shared/src/jvmMain/kotlin/com/actualfeed/llm/**` |
| `media-agent` | 2.2 (whisper-server client), 2.3 (ONNX embeddings) | `shared/src/jvmMain/kotlin/com/actualfeed/transcription/**`, `shared/src/jvmMain/kotlin/com/actualfeed/embedding/**` |
| `scoring-agent` | 2.5 (formula scorer), 2.7 (FTS5 search) | `shared/src/commonMain/kotlin/com/actualfeed/ranking/FormulaScorer.kt`, `shared/src/commonMain/kotlin/com/actualfeed/ranking/Blender.kt`, `shared/src/commonMain/kotlin/com/actualfeed/search/**` |
| `adapter-agent` | 2.6 (HN, Web Article, Podcast adapters) | `shared/src/commonMain/kotlin/com/actualfeed/adapter/HackerNewsAdapter.kt`, `WebArticleAdapter.kt`, `PodcastAdapter.kt`, `shared/src/jvmMain/kotlin/com/actualfeed/adapter/HackerNewsAdapterJvm.kt`, `WebArticleAdapterJvm.kt`, `PodcastAdapterJvm.kt` |

**Interface contracts** (written by lead before wave 1):
```kotlin
// LLM bridge interface — llm-agent implements
interface LlmClient {
    suspend fun extract(prompt: String, jsonSchema: String): ExtractionResult
    fun isHealthy(): Boolean
}

// Transcription interface — media-agent implements
interface TranscriptionClient {
    suspend fun transcribe(audioPath: Path): TranscriptionResult
    fun isHealthy(): Boolean
}

// Embedding interface — media-agent implements
interface EmbeddingService {
    suspend fun embed(text: String): FloatArray  // 768-dim
    suspend fun batchEmbed(texts: List<String>): List<FloatArray>
    fun cosineSimilarity(a: FloatArray, b: FloatArray): Float
}
```

**Wave 2 (after wave 1 completes):**

| Agent | Tasks | Owns |
|---|---|---|
| `pipeline-agent` | 2.4 (extraction pipeline), 2.8 (maturity validation), 2.9 (error recovery) | `shared/src/commonMain/kotlin/com/actualfeed/pipeline/**`, `shared/src/commonMain/kotlin/com/actualfeed/maturity/**` |

`pipeline-agent` orchestrates the interfaces that wave 1 agents implemented. This is the most complex single-agent task in the project — it wires together LLM, transcription, embeddings, normalization, tripwires, and error recovery.

**Shared file owner**: Phase 2 lead owns `Enums.kt`, `FeatureVector.kt`, all `.sq` files, and `shared/src/commonMain/kotlin/com/actualfeed/pipeline/ExtractionResult.kt` (the data class all agents produce/consume).

**Phase gate**:
- [ ] `./gradlew :shared:jvmTest` all green
- [ ] CLI: raw item → transcription (if audio) → extraction → 13 signals → formula score
- [ ] FTS5 search returns ranked results
- [ ] Maturity filtering works
- [ ] Error recovery: failures surfaced, retried, degraded gracefully
- [ ] Lead reviews full diff

---

### Phase 3: Desktop MVP

**Topology**: 4 agents, two waves
**Reason**: Tauri scaffold and JSON-RPC server must exist before UI agents can connect. Once the API is live, UI work parallelizes cleanly — each component directory is an isolated ownership boundary.

**Wave 1 (sequential):**

| Agent | Tasks | Owns |
|---|---|---|
| `server-agent` | 3.1 (Tauri scaffold), 3.2 (JSON-RPC server) | `desktop/src-tauri/**`, `server/src/main/kotlin/com/actualfeed/server/**`, `desktop/src/rpc.ts`, `desktop/src/stores/**`, `desktop/src/App.tsx`, `desktop/src/index.tsx`, `desktop/src/styles/globals.css`, `desktop/package.json`, `desktop/vite.config.ts`, `desktop/tsconfig.json`, `desktop/index.html` |

`server-agent` gets the skeleton running: Tauri window opens, JVM backend starts, JSON-RPC responds, SolidJS renders a placeholder feed from real data.

**Wave 2 (parallel, after wave 1):**

| Agent | Tasks | Owns |
|---|---|---|
| `feed-ui-agent` | 3.3 (feed rendering) | `desktop/src/components/feed/**`, `desktop/src/components/layout/**` |
| `embed-ui-agent` | 3.4 (YouTube adapter + embed), 3.7 (article + podcast embed) | `desktop/src/components/detail/**`, `shared/src/*/kotlin/com/actualfeed/adapter/YoutubeAdapter*.kt` |
| `feature-ui-agent` | 3.5 (tripwire UI), 3.6 (settings UI) | `desktop/src/components/tripwires/**`, `desktop/src/components/settings/**` |

**Interface contract** (written by `server-agent` before wave 2):
- `desktop/src/rpc.ts` — typed JSON-RPC client with method signatures matching the 26 API methods
- `desktop/src/stores/*.ts` — reactive stores that UI agents import and consume
- `desktop/src/styles/globals.css` — CSS custom properties (colors, spacing, typography)

UI agents import `rpc.ts` and stores. They do NOT modify them — they call methods and read signals.

**Shared file owner**: `server-agent` owns `rpc.ts`, all stores, `App.tsx`, `globals.css`. UI agents own their component directories exclusively.

**Phase gate**:
- [ ] Desktop window shows ranked feed from 5 sources
- [ ] All interactions record correctly
- [ ] YouTube/article/podcast embeds play
- [ ] Tripwires: create, fire, notify
- [ ] Search works
- [ ] Settings functional
- [ ] Lead reviews full diff

---

### Phase 4: Desktop Complete

**Topology**: 5 agents, two waves
**Reason**: Phase 4 has the most tasks (11) and many are independent. Neural model, presets, budgets, profiles, autoplay, and onboarding have almost zero file overlap.

**Wave 1 (parallel, all independent):**

| Agent | Tasks | Owns |
|---|---|---|
| `ml-agent` | 4.1 (neural taste model), 4.2 (Thompson sampling) | `shared/src/commonMain/kotlin/com/actualfeed/ranking/NeuralTasteModel.kt`, `ExplorationSampler.kt`, `shared/src/jvmMain/kotlin/com/actualfeed/ranking/NeuralTasteTrainer.kt` |
| `budget-agent` | 4.4 (budgets), 4.5 (profiles + parental controls) | `shared/src/commonMain/kotlin/com/actualfeed/budget/**`, `desktop/src/components/profiles/**`, `desktop/src/components/settings/BudgetsTab.tsx`, `ProfilesTab.tsx` |
| `preset-agent` | 4.3 (tuning presets), 4.8 (custom feeds) | `shared/src/commonMain/kotlin/com/actualfeed/preset/**`, `desktop/src/components/presets/**`, `desktop/src/components/custom-feeds/**` |
| `autoplay-agent` | 4.6 (autoplay / audio feed) | `shared/src/commonMain/kotlin/com/actualfeed/autoplay/**`, `shared/src/jvmMain/kotlin/com/actualfeed/autoplay/**`, `desktop/src/components/autoplay/**` |
| `onboard-agent` | 4.7 (onboarding flow) | `desktop/src/components/onboarding/**` |

**Wave 2 (after wave 1, sequential):**

| Agent | Tasks | Owns |
|---|---|---|
| `polish-agent` | 4.9 (pipeline dashboard), 4.10 (keyboard shortcuts + command palette), 4.11 (auto-update + crash rollback) | `desktop/src/components/pipeline/**`, `desktop/src/components/layout/CommandPalette.tsx`, `desktop/src-tauri/` (updater config) |

`polish-agent` handles the integration tasks that touch multiple subsystems (dashboard reads from pipeline, command palette indexes all actions, auto-update touches Tauri config).

**Shared file owner**: Phase 4 lead owns `Enums.kt`, `Blender.kt` (blend formula now includes neural + exploration), `server/src/main/kotlin/com/actualfeed/server/Methods.kt` (new RPC methods for budgets, presets, autoplay, profiles). Agents that need new RPC methods message the lead, who adds them to `Methods.kt`.

**Phase gate**:
- [ ] All 103 user stories satisfied
- [ ] All 63 enforced constraints passing (`test_EC_xxx`)
- [ ] Neural model differentiates items after 50 interactions
- [ ] Full onboarding → daily use scenario works end-to-end
- [ ] Cross-platform builds pass (macOS, Linux, Windows)
- [ ] Performance: feed 60fps, search <100ms, preset switch <2s
- [ ] Lead reviews full diff
- [ ] Tag: `v1.0.0`

---

### Phases 5-8: Post-V1

**Phase 5 (Android)**: 2 agents. One for Jetpack Compose UI, one for mobile model integration (Phi-4, Moonshine). Low overlap with desktop code — shared engine compiles to Android library.

**Phase 6 (Cloud & Sync)**: 3 agents. Auth/subscription (server-side Ktor), sync relay (S3 + conflict resolution), client integration. Mostly new code, minimal overlap with V1 codebase.

**Phase 7 (iOS)**: 2 agents. SwiftUI frontend, Kotlin/Native integration. Depends on Phase 6 for sync.

**Phase 8 (Emacs)**: 1 agent. Single-file `actual-feed.el`. No overlap with anything. Can start after Phase 3 (needs JSON-RPC server).

---

### Shared File Ownership Protocol

These files are touched by multiple tasks across phases. Exactly one agent owns each at any given time. Other agents request changes via message to the owner.

| File | Phase 1 Owner | Phase 2 Owner | Phase 3 Owner | Phase 4 Owner |
|---|---|---|---|---|
| `shared/.../model/Enums.kt` | `skeleton-lead` | phase 2 lead | `server-agent` | phase 4 lead |
| `shared/.../model/FeatureVector.kt` | `skeleton-lead` | phase 2 lead | (stable) | (stable) |
| `shared/.../sqldelight/**/*.sq` | `skeleton-lead` | phase 2 lead | `server-agent` | phase 4 lead |
| `server/.../server/Methods.kt` | — | — | `server-agent` | phase 4 lead |
| `desktop/src/rpc.ts` | — | — | `server-agent` | phase 4 lead |
| `desktop/src/stores/*.ts` | — | — | `server-agent` | phase 4 lead |
| `desktop/src/styles/globals.css` | — | — | `server-agent` | phase 4 lead |
| `desktop/src/App.tsx` | — | — | `server-agent` | `polish-agent` |
| `build.gradle.kts` (root) | `skeleton-lead` | (stable) | (stable) | (stable) |
| `settings.gradle.kts` | `skeleton-lead` | (stable) | (stable) | (stable) |

**Protocol for requesting a change to a shared file:**
1. Agent sends message to owner: "I need `InteractionType.OFTEN_REVISIT` added to `Enums.kt`"
2. Owner makes the edit, commits, confirms
3. Requesting agent pulls and continues

This is slower than direct access but eliminates merge conflicts on files with complex interdependencies.

---

### Agent Naming Convention

Agent names are `kebab-case`, scoped to their phase and role. The team lead for each phase is `phase-N-lead` or has a descriptive name.

| Phase | Lead | Teammates |
|---|---|---|
| 0 | `ci-agent` (solo) | — |
| 1 | `skeleton-lead` | `adapter-agent` |
| 2 | `engine-lead` | `llm-agent`, `media-agent`, `scoring-agent`, `adapter-agent`, `pipeline-agent` |
| 3 | `server-agent` (acts as lead) | `feed-ui-agent`, `embed-ui-agent`, `feature-ui-agent` |
| 4 | `desktop-lead` | `ml-agent`, `budget-agent`, `preset-agent`, `autoplay-agent`, `onboard-agent`, `polish-agent` |

---

### Concurrency Summary

| Phase | Max concurrent agents | Total agent-tasks | Critical path |
|---|---|---|---|
| 0 | 1 | 3 | CI → sign → auto-update |
| 1 | 2 | 6 | scaffold → schema → model → adapter + CLI |
| 2 | 4 → 1 | 9 | contracts → wave 1 (4 parallel) → wave 2 (pipeline) |
| 3 | 3 → 1 | 7 | scaffold + server → wave 2 (3 parallel UI agents) |
| 4 | 5 → 1 | 11 | wave 1 (5 parallel) → wave 2 (polish) |
| **V1 total** | — | **36 tasks** | Phases 0-4 sequential, internal parallelism per phase |

---

## Testing Strategy

### Per-Phase Testing

| Phase | Unit Tests | Integration Tests | E2E Tests |
|---|---|---|---|
| 1 | Schema, model, adapter parsing | RSS → SQLite → query round-trip | CLI crawl + list |
| 2 | LLM output parsing, scoring math, FTS5 queries | Full pipeline: raw → enriched → scored | CLI with real model |
| 3 | JSON-RPC method handlers, UI component tests | Frontend ↔ backend via JSON-RPC | Desktop app: crawl → view → interact |
| 4 | Neural model training, budget math, preset validation | Multi-profile scoring, autoplay queue | Full onboarding → daily use scenario |

### Continuous Validation

- **YAML story validation**: `python3 -c "import yaml; yaml.safe_load(open(f))"` on every story file change
- **Schema migration**: SQLDelight `verifyMigrations` task in CI
- **Cross-platform build**: macOS, Linux, Windows matrix in CI (GitHub Actions)
- **Performance regression**: search latency, feed scroll FPS, extraction throughput benchmarked per phase

### Acceptance Test Mapping

Every enforced constraint (EC-xxx) maps to at least one automated test. The test name follows `test_EC_xxx_description`. This creates a traceable chain: story → constraint → test → code.

---

## Document Traceability Matrix

| Phase | Stories Satisfied | ADRs Implemented | Story Files |
|---|---|---|---|
| 1 | US-001, US-040–042 | ADR-001, ADR-013 | 001, 005 |
| 2 | US-002–004, US-020–025, US-040–045, US-050–055, US-100–106, US-120–126, US-150–157 | ADR-003, ADR-006, ADR-010, ADR-011, ADR-012, ADR-015 | 001, 003, 005, 006, 011, 013, 016 |
| 3 | US-010–018, US-015b, US-015c, US-030–036, US-050–055, US-130–138 | ADR-004, ADR-009, ADR-013 | 002, 004, 006, 014 |
| 4 | US-023–025, US-030–036, US-090–095, US-100–106, US-110–115, US-140–147, US-150–157 | ADR-003, ADR-005 | 003, 004, 010, 011, 012, 015, 016 |
| 5 | US-002 | ADR-012, ADR-013 | 001, 012 |
| 6 | US-060–065 | ADR-002, ADR-007 | 007 |
| 7 | US-003 | ADR-012, ADR-013 | 001 |
| 8 | US-070–075 | ADR-009 | 008 |
