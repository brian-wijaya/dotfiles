# Project Tree — Actual Feed

**This document is the canonical authority for directory layout, file naming, and module boundaries.** Where the implementation plan references a path that conflicts with this tree, this tree wins. Implementation agents must use these exact paths.

Last updated: 2026-02-11

---

## Root

```
actual-feed/
├── build.gradle.kts                          # Root Gradle build (KMP plugin, shared config)
├── settings.gradle.kts                       # Module includes: shared, server
├── gradle.properties                         # Kotlin/Gradle version pins
├── gradlew / gradlew.bat                     # Gradle wrapper
├── .gitignore
├── .gitattributes                            # LFS tracking for binaries/
├── LICENSE
│
├── shared/                                   # ◆ KMP shared engine module
├── server/                                   # ◆ JVM backend application (JSON-RPC + CLI)
├── desktop/                                  # ◆ Tauri 2.x + SolidJS desktop shell
├── binaries/                                 # ◆ Pre-built native binaries (Git LFS)
├── models/                                   # ◆ Model files (gitignored, user-downloaded)
├── docs/                                     # ◆ All documentation
└── .github/                                  # ◆ CI/CD workflows
```

---

## shared/ — KMP Shared Engine

Platform-agnostic feed engine. Compiles to JVM (desktop, Android, server) and Kotlin/Native (iOS). Contains all business logic, data model, pipeline orchestration, scoring, and database access. Zero UI code.

```
shared/
├── build.gradle.kts                          # KMP module: jvm + iosArm64 + iosX64 targets
│                                             # Plugins: sqldelight, kotlinx-serialization
└── src/
    ├── commonMain/
    │   ├── kotlin/com/actualfeed/
    │   │   ├── model/                        # Domain model (pure data classes)
    │   │   │   ├── FeedItem.kt               # Full enriched item
    │   │   │   ├── FeedItemSummary.kt        # Lightweight projection for feed list
    │   │   │   ├── FeatureVector.kt          # 13 named signals, all Float [0,1]
    │   │   │   ├── ScoreBreakdown.kt         # Per-signal contribution + totals
    │   │   │   ├── Profile.kt
    │   │   │   ├── TuningPreset.kt
    │   │   │   ├── Tripwire.kt
    │   │   │   ├── TripwireFire.kt
    │   │   │   ├── CustomFeed.kt
    │   │   │   ├── BudgetStatus.kt
    │   │   │   ├── PipelineStatus.kt
    │   │   │   └── Enums.kt                  # PipelineState, Modality, SourcePlatform,
    │   │   │                                 # InteractionType, ErrorCode, BudgetPeriod,
    │   │   │                                 # BudgetMode, MaturityCategory, ClaimType,
    │   │   │                                 # SourceType, EntityType, AutoplayMode
    │   │   │
    │   │   ├── adapter/                      # Stage 1 source adapters (interfaces + common logic)
    │   │   │   ├── SourceAdapter.kt          # Interface: suspend fun crawl(): List<RawItem>
    │   │   │   ├── RssAdapter.kt
    │   │   │   ├── HackerNewsAdapter.kt
    │   │   │   ├── WebArticleAdapter.kt
    │   │   │   ├── PodcastAdapter.kt
    │   │   │   └── YoutubeAdapter.kt
    │   │   │
    │   │   ├── pipeline/                     # Stage 2 extraction orchestration
    │   │   │   ├── ExtractionPipeline.kt     # Per-item: transcribe → extract → enrich → score
    │   │   │   ├── ExtractionPrompt.kt       # Prompt template builder (see docs/extraction-prompt.md)
    │   │   │   ├── ExtractionResult.kt       # Parsed LLM output (summary, entities, claims, signals)
    │   │   │   ├── PostLlmSignals.kt         # freshness, novelty, source_authority, modality_preference
    │   │   │   ├── PercentileNormalizer.kt   # 100-bin histogram lookup + insert
    │   │   │   ├── ErrorRecovery.kt          # Retry logic, backoff, resource pressure detection
    │   │   │   └── PipelineLogger.kt         # Append-only per-stage log entries
    │   │   │
    │   │   ├── ranking/                      # Stage 3 scoring
    │   │   │   ├── FormulaScorer.kt          # Layer 1: weighted linear combination
    │   │   │   ├── NeuralTasteModel.kt       # Layer 2: MLP inference (expect/actual)
    │   │   │   ├── ExplorationSampler.kt     # Layer 3: Thompson sampling
    │   │   │   └── Blender.kt                # Combines layers per blend formula
    │   │   │
    │   │   ├── search/                       # FTS5 search
    │   │   │   └── SearchService.kt          # Query, sanitize, BM25 rank, snippet extract
    │   │   │
    │   │   ├── maturity/                     # Content maturity filtering
    │   │   │   └── MaturityFilter.kt         # Threshold + per-category override logic
    │   │   │
    │   │   ├── budget/                       # Content budget enforcement
    │   │   │   └── BudgetService.kt          # Soft/hard, cross-device aggregation
    │   │   │
    │   │   ├── preset/                       # Tuning presets
    │   │   │   └── PresetService.kt          # CRUD, shipped defaults, export/import
    │   │   │
    │   │   ├── autoplay/                     # Autoplay queue management
    │   │   │   └── AutoplayService.kt        # 4 modes, queue building, budget integration
    │   │   │
    │   │   └── sync/                         # Sync protocol (V1.1+, stubbed in V1)
    │   │       ├── SyncEngine.kt
    │   │       └── ConflictResolver.kt
    │   │
    │   └── sqldelight/com/actualfeed/db/     # SQLDelight schema (.sq files)
    │       ├── FeedItems.sq                  # feed_items: core content table
    │       ├── UserInteractions.sq           # user_interactions: feedback log
    │       ├── BudgetLedger.sq               # budget_ledger: playback tracking
    │       ├── ObliteratedSources.sq         # obliterated_sources: hard pre-filter
    │       ├── ExaltedSources.sq             # exalted_sources: source authority boost
    │       ├── Profiles.sq                   # profiles: multi-profile support
    │       ├── Fts.sq                        # feed_items_fts: FTS5 virtual table
    │       ├── SignalHistograms.sq            # signal_histograms: percentile normalization
    │       ├── Tripwires.sq                  # tripwires: event conditions
    │       ├── TripwireFires.sq              # tripwire_fires: firing history
    │       ├── TuningPresets.sq              # tuning_presets: ranking configurations
    │       ├── CustomFeeds.sq                # custom_feeds: filtered views
    │       ├── SearchHistory.sq              # search_history: query log
    │       ├── PipelineLog.sq                # pipeline_log: per-item-per-stage status
    │       └── PipelineStats.sq              # pipeline_stats: daily aggregates
    │
    ├── jvmMain/kotlin/com/actualfeed/
    │   ├── adapter/                          # JVM-specific adapter implementations
    │   │   ├── RssAdapterJvm.kt              # HTTP via Ktor Client, XML via ROME
    │   │   ├── HackerNewsAdapterJvm.kt       # HTTP via Ktor Client, JSON parse
    │   │   ├── WebArticleAdapterJvm.kt       # HTTP + readability4j extraction
    │   │   ├── PodcastAdapterJvm.kt          # Same as RSS + enclosure detection
    │   │   └── YoutubeAdapterJvm.kt          # Shell out to yt-dlp
    │   │
    │   ├── llm/                              # llama-server HTTP client
    │   │   ├── LlamaServerClient.kt          # POST /v1/chat/completions, schema enforcement
    │   │   └── LlamaServerProcess.kt         # Lifecycle: spawn, health check, restart, kill
    │   │
    │   ├── transcription/                    # whisper-server HTTP client
    │   │   ├── WhisperServerClient.kt        # POST /v1/audio/transcriptions
    │   │   └── WhisperServerProcess.kt       # Lifecycle: lazy start, idle shutdown
    │   │
    │   ├── embedding/                        # ONNX Runtime embeddings
    │   │   └── EmbeddingService.kt           # nomic-embed-text-v1.5, 768-dim
    │   │
    │   ├── ranking/                          # JVM-specific: DJL training for neural model
    │   │   └── NeuralTasteTrainer.kt         # Mini-batch SGD, ONNX export
    │   │
    │   └── db/                               # JVM SQLite driver config
    │       └── DatabaseFactory.kt            # sqlite-jdbc driver, WAL mode, pragma config
    │
    ├── iosMain/kotlin/com/actualfeed/        # Stubbed for Phase 7
    │   └── db/
    │       └── DatabaseFactory.kt            # Native SQLite driver
    │
    └── jvmTest/kotlin/com/actualfeed/        # Engine tests (JVM)
        ├── model/                            # Round-trip tests
        ├── adapter/                          # Parser tests with mock HTTP
        ├── pipeline/                         # Extraction + normalization tests
        ├── ranking/                          # Scoring math tests
        ├── search/                           # FTS5 query tests
        ├── maturity/                         # Filter threshold tests
        └── schema/                           # Table creation, migration, index tests
```

---

## server/ — JVM Backend Application

Entry point for the desktop backend process. Depends on `shared/`. Produces a fat-jar. Launched by Tauri as a sidecar process.

```
server/
├── build.gradle.kts                          # JVM application, shadow-jar plugin
│                                             # Dependencies: shared, ktor-server-cio, ktor-server-websockets
└── src/
    ├── main/kotlin/com/actualfeed/
    │   ├── Main.kt                           # Entry point: parse args, start server or CLI
    │   ├── cli/                              # CLI commands
    │   │   ├── CrawlCommand.kt               # actual-feed crawl
    │   │   ├── ListCommand.kt                # actual-feed list
    │   │   ├── ShowCommand.kt                # actual-feed show <id>
    │   │   └── StatsCommand.kt               # actual-feed stats
    │   └── server/                           # JSON-RPC 2.0 server
    │       ├── JsonRpcServer.kt              # Ktor CIO, UDS (Linux/macOS) + HTTP localhost (Windows)
    │       ├── JsonRpcDispatcher.kt          # Method routing, param validation
    │       ├── Methods.kt                    # 26 API method implementations
    │       └── SidecarLifecycle.kt           # llama-server + whisper-server process management
    └── test/kotlin/com/actualfeed/
        ├── cli/                              # CLI output tests
        └── server/                           # JSON-RPC method tests
```

---

## desktop/ — Tauri Desktop Shell

System webview wrapping SolidJS frontend. Communicates with the JVM backend (`server/`) via JSON-RPC over Unix domain socket. Contains zero business logic — pure presentation.

```
desktop/
├── package.json                              # SolidJS + Vite
├── pnpm-lock.yaml
├── vite.config.ts
├── tsconfig.json
├── index.html                                # Vite entry
│
├── src-tauri/                                # Tauri Rust glue (minimal)
│   ├── tauri.conf.json                       # Window config, bundle config, updater config
│   ├── Cargo.toml
│   ├── Cargo.lock
│   ├── capabilities/                         # Tauri permission capabilities
│   ├── icons/                                # App icons (all sizes)
│   └── src/
│       └── main.rs                           # Sidecar launch (JVM fat-jar), single-instance,
│                                             # system tray, window position persistence
│
├── src/                                      # SolidJS + TypeScript frontend
│   ├── index.tsx                             # Mount point
│   ├── App.tsx                               # Root layout: sidebar + content area
│   ├── rpc.ts                                # JSON-RPC 2.0 client (UDS/HTTP transport)
│   ├── stores/                               # SolidJS stores (reactive state)
│   │   ├── feed.ts                           # Feed items, pagination, active preset
│   │   ├── profile.ts                        # Active profile, switching
│   │   ├── pipeline.ts                       # Pipeline status, queue depth
│   │   ├── budget.ts                         # Budget consumption, limits
│   │   └── settings.ts                       # Config, sources, topics
│   │
│   ├── components/                           # UI components
│   │   ├── layout/
│   │   │   ├── Sidebar.tsx                   # Custom feeds, pipeline chip, budget bars, profile
│   │   │   ├── Toolbar.tsx                   # Search, preset selector, action buttons
│   │   │   └── CommandPalette.tsx            # Ctrl+K / Cmd+K fuzzy action search
│   │   │
│   │   ├── feed/
│   │   │   ├── Feed.tsx                      # Virtualized list (solid-virtual)
│   │   │   ├── FeedCard.tsx                  # Card: thumb, title, summary, score, interactions
│   │   │   ├── ScoreBreakdown.tsx            # Per-signal contribution overlay
│   │   │   ├── InteractionBar.tsx            # Thumbs, obliterate/exalt gesture target
│   │   │   └── KebabMenu.tsx                 # More like this, less like this, etc.
│   │   │
│   │   ├── detail/
│   │   │   ├── ItemDetail.tsx                # Full extraction: summary, entities, claims, sources
│   │   │   ├── YoutubeEmbed.tsx              # IFrame Player API, budget enforcement
│   │   │   ├── ArticleEmbed.tsx              # Readability-rendered inline
│   │   │   └── PodcastPlayer.tsx             # HTML5 audio, budget tracking
│   │   │
│   │   ├── tripwires/
│   │   │   ├── TripwireList.tsx              # Active tripwires with toggle
│   │   │   ├── TripwireForm.tsx              # Create/edit: condition, severity, keywords
│   │   │   └── TripwireHistory.tsx           # Chronological fire list
│   │   │
│   │   ├── settings/
│   │   │   ├── Settings.tsx                  # Tabbed container
│   │   │   ├── SourcesTab.tsx                # Source grid, RSS/YT management, compute budget
│   │   │   ├── TopicsTab.tsx                 # Interest declaration
│   │   │   ├── ModelTab.tsx                  # Tier selection, download progress
│   │   │   ├── ScheduleTab.tsx               # Crawl interval
│   │   │   ├── BudgetsTab.tsx                # Per-type limits, soft/hard, day-of-week
│   │   │   ├── ProfilesTab.tsx               # Create/edit profiles, maturity config
│   │   │   └── ManageSourcesTab.tsx          # Obliterated/exalted source lists
│   │   │
│   │   ├── autoplay/
│   │   │   └── AutoplayPlayer.tsx            # Mode selector, queue, hardware controls
│   │   │
│   │   ├── pipeline/
│   │   │   ├── ComputeBudgetBar.tsx          # Horizontal segmented bar
│   │   │   └── PipelineDashboard.tsx         # Queue depth, rates, errors, model health
│   │   │
│   │   ├── presets/
│   │   │   └── PresetEditor.tsx              # Weight sliders, live preview, export/import
│   │   │
│   │   ├── custom-feeds/
│   │   │   └── CustomFeedEditor.tsx          # Filter builder, preset assignment
│   │   │
│   │   ├── onboarding/
│   │   │   └── Onboarding.tsx                # 6-screen wizard (see wireframes.md)
│   │   │
│   │   ├── profiles/
│   │   │   ├── ProfileSwitcher.tsx           # Dropdown + PIN entry
│   │   │   └── ParentalControls.tsx          # Maturity config, filtered review, whitelist
│   │   │
│   │   └── shared/                           # Reusable primitives
│   │       ├── BudgetBar.tsx                 # Single budget progress bar
│   │       ├── SignalBar.tsx                 # Single signal indicator bar
│   │       └── Badge.tsx                     # Exalt star, exploration badge, failed badge
│   │
│   └── styles/
│       └── globals.css                       # CSS custom properties, base reset, shared styles
│
└── public/                                   # Static assets
    └── favicon.svg
```

---

## binaries/ — Pre-built Native Binaries

Git LFS tracked. Downloaded in CI from upstream releases. Bundled into installers via Tauri `bundle.resources`.

```
binaries/
├── llama-server-darwin-aarch64               # macOS Apple Silicon
├── llama-server-darwin-x86_64                # macOS Intel
├── llama-server-linux-x86_64                 # Linux
├── llama-server-windows-x86_64.exe           # Windows
├── whisper-server-darwin-aarch64
├── whisper-server-darwin-x86_64
├── whisper-server-linux-x86_64
├── whisper-server-windows-x86_64.exe
└── README.md                                 # Version pins, build instructions, checksums
```

---

## models/ — Gitignored

Not checked into the repo. The `models/` directory in the repo root exists only as a `.gitignore` entry. Model files live at runtime in `<data_dir>/models/` (see Runtime Paths above). The fallback model (`qwen2.5-3b-q4.gguf`) is bundled inside the installer at the install directory path.

---

## docs/ — Documentation

```
docs/
├── PRD.md                                    # Product Requirements Document (v0.4)
├── implementation-plan.md                    # 9-phase task-level plan
├── extraction-prompt.md                      # Stage 2 LLM prompt, JSON schema, token budget
├── wireframes.md                             # ASCII wireframes for 4 core screens
├── project-tree.md                           # THIS FILE
├── vocabulary.md                             # Locked vocabulary / glossary
│
├── ADR/                                      # Architecture Decision Records
│   ├── ADR-001-local-first-sqlite-storage.md
│   ├── ADR-002-jetbrains-licensing-model.md
│   ├── ADR-003-three-layer-scoring.md
│   ├── ADR-004-platform-embedding-via-official-apis.md
│   ├── ADR-005-per-content-type-budgets.md
│   ├── ADR-006-single-call-llm-extraction.md
│   ├── ADR-007-zero-knowledge-encrypted-sync.md
│   ├── ADR-009-emacs-first-class-client.md
│   ├── ADR-010-natural-language-tripwires.md
│   ├── ADR-011-no-silent-degradation.md
│   ├── ADR-012-honest-platform-asymmetry.md
│   ├── ADR-013-kotlin-multiplatform-toolchain.md
│   ├── ADR-014-v1-mvp-scope-and-implementation-plan.md
│   └── ADR-015-feature-signal-calibration.md
│
└── e2e-stories/                              # End-to-end story files (YAML)
    ├── 001-architecture-tiers.yaml
    ├── 002-core-experience.yaml
    ├── 003-ranking-pipeline.yaml
    ├── 004-budget-and-embedding.yaml
    ├── 005-feeditem-schema-and-extraction.yaml
    ├── 006-tripwires-and-events.yaml
    ├── 007-sync-protocol.yaml
    ├── 008-emacs-client.yaml
    ├── 010-onboarding-first-run.yaml
    ├── 011-parental-controls-and-maturity-filtering.yaml
    ├── 012-autoplay-and-audio-feed.yaml
    ├── 013-search-and-discovery.yaml
    ├── 014-desktop-application-shell.yaml
    ├── 015-tuning-presets-and-custom-feeds.yaml
    └── 016-error-recovery-and-pipeline-transparency.yaml
```

---

## .github/ — CI/CD

```
.github/
└── workflows/
    ├── ci.yml                                # Build matrix: macOS arm64/x86_64, Linux x86_64, Windows x86_64
    └── release.yml                           # Tag → build → sign → upload → update latest.json
```

---

## Runtime Paths

Not in the repo. Created at runtime on the user's machine. The **data directory** holds all user state. The **install directory** holds the application binary, sidecar processes, and bundled resources.

### Data Directory

| Platform | Path |
|---|---|
| macOS | `~/Library/Application Support/com.actualfeed.app/` |
| Linux | `~/.local/share/actual-feed/` (XDG_DATA_HOME) |
| Windows | `%APPDATA%\ActualFeed\` |

Resolved at runtime via Tauri's `app_data_dir()` API. Code must never hardcode paths — always use the platform resolver.

```
<data_dir>/
├── feed.db                                   # SQLite database (WAL mode)
├── feed.db-wal                               # WAL file
├── feed.db-shm                               # Shared memory
├── config.json                               # User configuration
├── llama-server.port                         # Ephemeral port for current llama-server instance
├── whisper-server.port                       # Ephemeral port for current whisper-server instance
├── models/                                   # Downloaded model files
│   ├── qwen3-30b-a3b-q4.gguf                # Primary LLM (~18 GB)
│   ├── qwen2.5-3b-q4.gguf                   # Fallback LLM (~2 GB, bundled in installer)
│   ├── whisper-large-v3-turbo.bin            # Transcription model
│   ├── nomic-embed-text-v1.5.onnx           # Embedding model (137M, 768-dim)
│   ├── neural-taste-v*.onnx                  # User's trained taste model (~1 MB)
│   └── piper-en-*.onnx                       # TTS voice (~50 MB per voice)
├── thumbnails/                               # Cached thumbnails
└── update/                                   # Auto-update staging area
    └── previous/                             # Previous version for crash rollback
```

### Log Directory

| Platform | Path |
|---|---|
| macOS | `~/Library/Logs/com.actualfeed.app/` |
| Linux | `~/.local/state/actual-feed/logs/` (XDG_STATE_HOME) |
| Windows | `%APPDATA%\ActualFeed\logs\` |

Resolved at runtime via Tauri's `app_log_dir()` API.

```
<log_dir>/
├── server.log                                # JVM backend log
├── llama-server.log                          # LLM server stdout/stderr
└── whisper-server.log                        # Transcription server stdout/stderr
```

### Cache Directory

| Platform | Path |
|---|---|
| macOS | `~/Library/Caches/com.actualfeed.app/` |
| Linux | `~/.cache/actual-feed/` (XDG_CACHE_HOME) |
| Windows | `%LOCALAPPDATA%\ActualFeed\cache\` |

Resolved at runtime via Tauri's `app_cache_dir()` API. Deletable without data loss.

### Install Directory

| Platform | Path |
|---|---|
| macOS | `/Applications/Actual Feed.app/Contents/` |
| Linux (AppImage) | Self-contained, mounts at runtime |
| Linux (deb/rpm) | `/usr/share/actual-feed/` |
| Windows | `C:\Program Files\Actual Feed\` |

Bundled resources (sidecar binaries, fallback model) are inside the install directory:

| Platform | Sidecar binaries | Bundled model |
|---|---|---|
| macOS | `Contents/Resources/binaries/llama-server`, `whisper-server` | `Contents/Resources/models/qwen2.5-3b-q4.gguf` |
| Linux (deb) | `/usr/share/actual-feed/binaries/llama-server`, `whisper-server` | `/usr/share/actual-feed/models/qwen2.5-3b-q4.gguf` |
| Linux (AppImage) | `<mount>/usr/share/actual-feed/binaries/` | `<mount>/usr/share/actual-feed/models/` |
| Windows | `C:\Program Files\Actual Feed\binaries\llama-server.exe`, `whisper-server.exe` | `C:\Program Files\Actual Feed\models\qwen2.5-3b-q4.gguf` |

---

## Naming Conventions

| Scope | Convention | Example |
|---|---|---|
| Kotlin files | PascalCase | `ExtractionPipeline.kt` |
| Kotlin packages | lowercase dotted | `com.actualfeed.pipeline` |
| SQLDelight files | PascalCase | `FeedItems.sq` |
| SQLite tables | snake_case | `feed_items` |
| SQLite columns | snake_case | `pipeline_state` |
| TypeScript/SolidJS files | PascalCase for components, camelCase for modules | `FeedCard.tsx`, `rpc.ts` |
| Component directories | kebab-case | `custom-feeds/` |
| CSS | Plain CSS with custom properties | BEM-style or scoped per component |
| JSON-RPC methods | dot-namespaced camelCase | `feed.getItems` |
| Error codes | SCREAMING_SNAKE_CASE | `LLM_MODEL_NOT_LOADED` |
| Feature signals | snake_case | `topic_relevance` |
| Pipeline states | snake_case | `pipeline_state=extracting` |
| Interaction types | snake_case | `thumbs_up` |
| Enum values | snake_case | `breaking_news` |
| Binary names | kebab-case with platform suffix | `llama-server-darwin-aarch64` |
| Gradle modules | lowercase | `shared`, `server`, `desktop` |

---

## Module Dependency Graph

```
shared (KMP)
  ↑
server (JVM) ── depends on shared via Gradle
  ↑
desktop (Tauri) ── launches server fat-jar as sidecar process
                   communicates via JSON-RPC over UDS
```

No circular dependencies. `shared` depends on nothing in the project. `server` depends on `shared`. `desktop` depends on neither at compile time — it launches `server` as a child process at runtime.

---

## Post-V1 Modules (Not Created Until Needed)

```
actual-feed/
├── android/                                  # Phase 5: Jetpack Compose app
│   ├── app/
│   │   └── src/main/kotlin/com/actualfeed/android/
│   └── build.gradle.kts
│
├── ios/                                      # Phase 7: SwiftUI app
│   ├── ActualFeed/
│   │   └── Sources/
│   └── ActualFeed.xcodeproj
│
└── emacs/                                    # Phase 8: Emacs client
    └── actual-feed.el                        # Single-file package
```
