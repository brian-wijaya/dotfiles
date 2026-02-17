# Product Requirements Document (PRD)
## Actual Feed

### Status
Draft v0.4

### Owner
Brian

### Last Updated
2026-02-11

### Tagline
*It's your algorithm. It's your life. It's your feed.*

---

## 1. Purpose

Actual Feed is a **user-controlled, local-first, infinite awareness system** that aggregates, ranks, and presents information from the web across text, audio, and video.

The core problem it solves is **loss of user agency over information selection** caused by proprietary, engagement-optimized feeds. Existing platforms do not allow users to define negative selection pressure, ranking logic, or long-term provenance memory.

Actual Feed restores user control over:
- what is surfaced
- why it is surfaced
- how it is ordered
- how it evolves over time

The system is always-on and infinite, but **selection logic is owned by the user**, not the platform. The feed can be as healthy or unhealthy as the user configures — no moral guardrails on adult self-configuration. Parental controls (story 011) are parent-imposed constraints on child profiles, not system-imposed morality.

---

## 2. Non-Goals

Actual Feed is **not**:
- a social network
- a publishing platform
- a content host
- an ad-supported product
- a time-limiting or "anti-usage" app
- a replacement for the underlying platforms (X, YouTube, Reddit, etc.)

It does not attempt to:
- eliminate hype globally
- moderate or censor content
- judge correctness or truth automatically
- enforce morality or "healthy usage" patterns

---

## 3. Target Users

Primary users:
- senior engineers
- founders
- researchers
- analysts
- technically literate professionals
- Emacs / terminal-centric users
- **parents** — controlling what their family consumes

User characteristics:
- already paying for professional tools
- sensitive to cognitive overload
- long-horizon projects
- preference for transparency over polish
- distrust of opaque algorithms
- may be configuring feeds for children (age-appropriate filtering is a core capability, not a feature)

---

## 4. Core Principles

1. **User-owned ranking**
   - Ranking logic is inspectable and configurable.
   - Negative weights are first-class.
   - Three scoring layers: explicit formula, learned neural taste, exploration/serendipity.
   - Score breakdown available for every item on demand.

2. **Infinite by design**
   - The feed never ends.
   - Control comes from selection quality, not artificial limits.
   - Optional content budgets (per-content-type) are user-imposed, never system-imposed.

3. **Local-first**
   - Core functionality works offline.
   - User data belongs to the user.
   - SQLite database file is the user's data — portable, exportable, inspectable.
   - No API keys required from end users.

4. **Cloud as convenience**
   - Sync, mobile continuity, and live crawling are paid services.
   - Local use is never sabotaged.
   - Cloud crawler is architecturally a virtual device in the sync protocol.

5. **Truth over engagement**
   - Licensing state must never affect correctness or ranking integrity.
   - No silent degradation — every failure surfaced with reason codes.
   - No platform engagement metrics displayed (likes, upvotes, view counts stripped).

6. **No surveillance**
   - No covert telemetry.
   - No device fingerprinting.
   - Sync payloads encrypted client-side — server stores only ciphertext.

---

## 5. System Overview

### High-Level Architecture

- **Core Engine**: Kotlin (shared KMP module — compiles to JVM on desktop/Android, Kotlin/Native on iOS)
- **Storage**: SQLite 3 with FTS5 via SQLDelight (type-safe multiplatform SQL)
- **Desktop Shell**: Tauri 2.x (system webview, ~5MB — platform embeds work natively)
- **Desktop UI**: SolidJS + TypeScript (fine-grained reactivity, no virtual DOM)
- **Android UI**: Kotlin + Jetpack Compose
- **iOS UI**: Swift + SwiftUI (imports shared engine as native framework)
- **Emacs Client**: `actual-feed.el` — first-class buffer-based text interface
- **Protocol**: JSON-RPC 2.0 over Unix domain socket (Linux/macOS) or HTTP localhost
- **Local LLM**: Desktop: Qwen3-30B-A3B (MoE, Q4, 32GB RAM). Mobile: Phi-4 mini / Qwen2.5-3B (Q4, ~2GB). Via llama.cpp (JNI on JVM, cinterop on iOS).
- **Local Transcription**: Desktop: Whisper Large-v3 Turbo (whisper.cpp). Mobile: Moonshine (ONNX, charging only).
- **Local Embedding**: Desktop: nomic-embed-text-v1.5 (137M). Mobile: all-MiniLM-L6-v2 (22M). Via ONNX Runtime.
- **Build**: Gradle with Kotlin DSL. KMP project structure.

See: [ADR-013](ADR/ADR-013-kotlin-multiplatform-toolchain.md), [ADR-014](ADR/ADR-014-v1-mvp-scope-and-implementation-plan.md), [014-desktop-application-shell.yaml](e2e-stories/014-desktop-application-shell.yaml)

**V1 ships as desktop-only** (macOS, Linux, Windows) with 5 free adapters (RSS, HN, Web Articles, Podcasts, YouTube). All three scoring layers, budgets, tripwires, parental controls, autoplay included. Mobile, sync, and paid-API sources are V1.1+. Desktop: $79 one-time. See ADR-014 for 8-phase implementation plan and pricing model.

The backend is authoritative for:
- feed item schema
- ranking logic (three-layer scoring)
- provenance tracking
- sync state
- pipeline orchestration

### Three-Stage Pipeline

1. **Stage 1 — Candidate Generation (Crawl)**: Source adapters fetch raw content, normalize into FeedItem schema. No ML. Pure I/O.
2. **Stage 2 — Feature Extraction (LLM Processing)**: Single-call LLM extraction per item produces summary, entities, claims, sources, topics, and 8 LLM-scored feature signals (topic_relevance, nominal_density, credibility, actionability, hype_score, vagueness, repetition, missing_sources). 4 additional signals computed post-LLM (source_authority, modality_preference, freshness, novelty) + content_maturity (hard pre-filter). 13 total. Tripwire evaluation folded into same call. 80%+ of compute.
3. **Stage 3 — Scoring & Ordering (Three Layers)**: Explicit formula + neural MLP taste model + Thompson sampling exploration. Blended by user-controlled sliders. Hard pre-filters (obliterate, minimums) execute before scoring.

See: [ADR-003](ADR/ADR-003-three-layer-scoring.md), [ADR-006](ADR/ADR-006-single-call-llm-extraction.md)

---

## 6. Feed Model

All content is normalized into a single `FeedItem` record stored in SQLite.

### FeedItem Schema (key fields)

**Identity** (immutable):
- `id` (UUIDv7 — time-sortable, globally unique)
- `source_url` (canonical dedup key, UNIQUE)
- `source_platform` (youtube, twitter, reddit, hackernews, stackoverflow, rss, github, web_article, podcast, quora)
- `source_id` (platform-native ID)

**Raw Metadata** (Stage 1 output):
- `title`, `author_name`, `author_id`, `published_at`, `crawled_at`
- `modality` (video, audio, text, image)
- `duration_seconds`, `thumbnail_url`, `embed_data` (JSON)
- `raw_content_hash`, `content_length_tokens`

**Enrichment** (Stage 2 output, re-extractable):
- `extraction_model_id`, `extraction_version`, `extracted_at`
- `summary` (2-4 sentences, shown in feed card)
- `transcript` (full, for audio/video, stored for FTS)
- `entities_json`, `claims_json`, `primary_sources_json`, `topics_json`

**Feature Vector** (13 signals, all [0,1]):
- Positive: `topic_relevance`, `nominal_density`, `novelty`, `credibility`, `freshness`, `actionability`
- Negative: `hype_score`, `vagueness`, `repetition`, `missing_sources`
- Metadata: `source_authority`, `modality_preference`
- Safety: `content_maturity` (0 = universally appropriate, 1 = adult only) + `maturity_flags_json` (category breakdown)

**State**: `pipeline_state` (raw → transcribing → extracting → enriched → failed)

### Supporting Tables

- `user_interactions` — append-only feedback log (thumbs_up, thumbs_down, obliterate, skip, play events). Training data for neural taste model.
- `budget_ledger` — per-session playback time tracking with device_id for cross-device budget aggregation.
- `obliterated_sources` — hard pre-filter. Rage-quit sources excluded before scoring.
- `profiles` — multi-profile support. Owner (parent) + child profiles with per-profile maturity thresholds, interaction history, and neural taste models.
- `sync_state` — premium only. Tracks sync checkpoints.

See: [005-feeditem-schema-and-extraction.yaml](e2e-stories/005-feeditem-schema-and-extraction.yaml), [ADR-001](ADR/ADR-001-local-first-sqlite-storage.md)

---

## 7. Ranking & Selection

### Three-Layer Scoring (Stage 3)

**Layer 1 — Explicit Formula (Hard)**: Weighted linear combination of 12 ranking features (content_maturity is a hard pre-filter, not a scoring signal). Fully visible, fully editable, deterministic. Hard limits (obliterate, minimum thresholds) are pre-filters — not scoring signals, not overridable.

**Layer 2 — Learned Taste (Soft)**: Small neural MLP (~1MB, ONNX Runtime + DJL on JVM) trained incrementally on user's feedback. Discovers nonlinear preference patterns. Local-only, user-owned, resettable. Enables "surprise me with something I didn't know I wanted."

**Layer 3 — Exploration (Probabilistic)**: Thompson sampling that surfaces items from outside the top-N. Prevents filter bubbles. Controlled by serendipity slider (0-50%).

**Blend**: `final = (1 - serendipity) × [taste_slider × neural + (1 - taste_slider) × formula] + serendipity × exploration`

### User Controls

- Per-signal weight sliders (12+ signals)
- Taste slider: 0% (pure formula) → 100% (pure neural)
- Serendipity slider: 0% (no exploration) → 50% (aggressive discovery). Capped at 50% because beyond that, more items are random than ranked — the user's formula and taste model become minority contributors, making the feed feel arbitrary rather than serendipitous
- **Tuning presets**: Named, versioned, portable, exportable. Assignable per-feed. Shipped defaults: deliberate, discovery, balanced, deep_research, breaking_news, background_audio.
- Score breakdown on demand for every item: formula contribution, neural adjustment, exploration flag.

### Negative Selection

Users downweight via negative signal weights: emotional language (hype_score), vague claims (vagueness), missing sources (missing_sources), repetition (repetition). Downweighting is preference expression, not punishment.

### Obliterate & Exalt

**Obliterate** (negative terminal): Rage-quit a source — thumbs-down rapid tap within 2000ms triggers "obliterate." The obliterated entity is the item's author (author_id) if available, otherwise the domain (source_domain). This granularity means obliterating one YouTube channel does not obliterate all of YouTube. Source permanently pre-filtered (not scored, not visible). Visually satisfying destruction animation. Reversible from settings.

**Exalt** (positive terminal): Devotion-boost a source — thumbs-up rapid tap within 2000ms triggers "exalt." Source gets maximum algorithmic boost (source_authority locked to 1.0). Distinctive badge (star/crown) in feed. No cap on exalted sources — same philosophy as obliterate (no moral guardrails on user choices). Exalt does NOT trigger notifications (that's what tripwires are for — separate concerns). Exalt does NOT exempt from budget enforcement (budgets are budgets). Reversible from settings.

Symmetrical terminal gestures: same mechanic, opposite polarity, purely for influencing the feed algorithm. The user has equal power to permanently promote and permanently demote.

### Tune My Recommendations

Kebab menu (three dots, bottom-right of every content card) provides granular per-item feedback: "More like this," "Less like this," "Already seen / done with," "Often revisit," "Not relevant anymore" (time-bounded suppression), "Misleading / spam" (source credibility decay), "Why was this recommended?" (score breakdown). All actions logged and feed into both formula adjustments and neural taste model. This is the power-user refinement layer — the feed works well without it.

Feature signals are percentile-normalized against the user's own item history before scoring — signals mean "percentile rank among items you've seen," not raw LLM output. This makes formula weights stable across model swaps. Content maturity is exempt (uses raw scores for absolute thresholds).

See: [ADR-003](ADR/ADR-003-three-layer-scoring.md), [ADR-015](ADR/ADR-015-feature-signal-calibration.md), [003-ranking-pipeline.yaml](e2e-stories/003-ranking-pipeline.yaml), [015-tuning-presets-and-custom-feeds.yaml](e2e-stories/015-tuning-presets-and-custom-feeds.yaml)

---

## 8. Feed Experience

### Main Feed

Default view. Interleaved across all checked sources and modalities. Ranked by the user's active preset (default: balanced). Visually rich — thumbnails, video previews, rich media for vetted content. LLM agents act as bouncers, not censors.

Each item shows: summary, source, author, timestamp, confidence indicator, direct link to original, "why this appeared" score breakdown. No platform engagement metrics.

### Custom Feeds

User-created filtered views by modality (video, audio, text) or source (YouTube only, Reddit only). Each custom feed has its own tuning preset. Main feed remains default.

### Feed Refresh

Configurable per-feed: manual, persistently current (live), automatic on app open, automatic on startup. Vertical or horizontal autoscroll toggle. Vertical/horizontal carousels by category (optional).

### Autoplay / Audio Feed

Four modes: **scan** (TTS summaries only, 15-20 items/10min), **deep** (full original content), **background** (short items full, long items summarized), **project-focused** (filtered to active feed/preset). TTS via Piper (ONNX, local-first, ~50MB per voice). Queue = ranked feed (not a separate playlist). Hardware controls: single-press play/pause, double-press skip, triple-press/long-press thumbs-up. Budget enforcement applies during autoplay. Tripwire fires interrupt playback.

See: [012-autoplay-and-audio-feed.yaml](e2e-stories/012-autoplay-and-audio-feed.yaml)

### Search

FTS5 full-text search across title, summary, transcript, entities, and topics. Local-only, offline, instant. Results show highlighted snippets with BM25 relevance scoring. Filters by source platform, modality, date range, and score range. "Find similar" uses embedding nearest neighbors to discover related content across modalities. Saved searches with optional notification on new matches.

See: [013-search-and-discovery.yaml](e2e-stories/013-search-and-discovery.yaml)

### Compute Budget Bar & Pipeline Dashboard

Single horizontal bar with colored segments per pipeline stage (crawling, transcription, summarization, ranking, storage). Updates in real-time as user ticks sources or swaps models. Percentages shown per segment. Reflects actual device capabilities (recalibrates on startup). Pipeline dashboard shows per-stage queue depth, processing rates, error counts, and model health. Every failure surfaced with reason code, retry button, and graceful degradation path.

See: [002-core-experience.yaml](e2e-stories/002-core-experience.yaml), [016-error-recovery-and-pipeline-transparency.yaml](e2e-stories/016-error-recovery-and-pipeline-transparency.yaml)

---

## 9. Content Budgets

Optional per-content-type time limits: video hours, audio hours, social minutes, article count. Budgets apply across all feeds. Configurable per day-of-week (weekday vs weekend profiles). Budget period: daily, weekly, or rolling window.

**Soft budgets** (dismissable) are default. **Hard budgets** (PIN-locked) are opt-in. Budget UI is inline on the content block, not a modal — feed browsing continues.

### Budget Enforcement

**Pause-then-collapse pattern**: Programmatically pause the platform player → collapse/hide the embed → show budget UI in the vacated space. No overlay on any platform's player at any time (YouTube ToS compliance). Budget UI shows: simplified bar, current watch time, time until available.

### Platform Embedding

Each platform uses its official embed API: YouTube IFrame Player API, Twitter oEmbed, Reddit API, Stack Overflow CC BY-SA with attribution. YouTube native controls and branding preserved. Ads appear as the platform controls them.

See: [ADR-004](ADR/ADR-004-platform-embedding-via-official-apis.md), [ADR-005](ADR/ADR-005-per-content-type-budgets.md), [004-budget-and-embedding.yaml](e2e-stories/004-budget-and-embedding.yaml)

---

## 10. Tripwires & Events

Event-driven interrupts parallel to the feed. User-defined **natural language conditions** evaluated by the local LLM during Stage 2 extraction (folded into same call, not separate). Confidence threshold: 0.7 (user-adjustable).

Examples: "Any CVE affecting Spring Framework", "Stripe raises prices", "New Java LTS release announced."

When a tripwire fires: (1) notification with tripwire name, item title, confidence, reason. (2) Item injected at top of relevant feed with distinctive visual treatment.

**Cloud tripwires** (premium): evaluate continuously. **Local tripwires**: evaluate on crawl schedule (honest gap communicated). Shipped tripwire packs for common domains (Java Ecosystem, Security Advisories, AI/ML Releases). Community sharing V2.

Critical-severity tripwires can override Do Not Disturb (opt-in, iOS Critical Alert entitlement + Android PRIORITY_MAX).

See: [ADR-010](ADR/ADR-010-natural-language-tripwires.md), [006-tripwires-and-events.yaml](e2e-stories/006-tripwires-and-events.yaml)

---

## 11. Content Extraction Pipeline

### Source Adapters (Stage 1)

8 adapters, each independently updatable:

| Source | Method | Legal Basis |
|---|---|---|
| YouTube | yt-dlp (metadata + transcript) | Public metadata extraction |
| Twitter/X | X API v2 (Basic tier, $100/mo) | Official API |
| Reddit | Reddit API (OAuth2) | Official API (paid tier for commercial) |
| Hacker News | Algolia HN Search API | Free, no auth |
| Stack Overflow | Stack Exchange API v2.3 | CC BY-SA 4.0 |
| RSS | ROME library (JVM) | Standard syndication |
| Web Articles | HTTP + Readability extraction | Standard browsing |
| Podcasts | RSS with enclosures | Intended for distribution |

### Enrichment (Stage 2)

Single LLM call per item producing structured JSON: summary, entities, claims, primary sources, topic labels, 8 feature signals, content maturity assessment, and tripwire evaluations. Post-LLM: freshness (math), novelty/repetition (embedding similarity), source_authority and modality_preference (user config lookup).

Idempotent: same content + same model + same topics = same output. Re-extraction on model swap with before/after comparison.

See: [ADR-006](ADR/ADR-006-single-call-llm-extraction.md), [005-feeditem-schema-and-extraction.yaml](e2e-stories/005-feeditem-schema-and-extraction.yaml)

---

## 12. Cross-Device Sync (Premium)

### Architecture

Client-centric with server relay. Each device maintains complete local SQLite. Server stores per-user append-only changelog of **encrypted** change events. Zero-knowledge: all payloads encrypted client-side (PBKDF2 → AES-256-GCM) before upload. Server cannot read user data.

### What Syncs

- `feed_items` (higher extraction_version wins)
- `user_interactions` (append-only merge)
- `budget_ledger` (append-only with device_id — cross-device budget aggregation: each device appends its own time entries, budget enforcement sums all device entries for the current period. No conflict possible — entries are device-stamped and never mutated. If devices sync stale, the budget may temporarily under-count; next sync corrects it.)
- `obliterated_sources` (last-write-wins)
- `tripwires`, `presets`, `feed_configs` (sync_version counter, conflict log for manual review)

### What Does Not Sync

- Neural model weights (retrains from merged interaction log — deterministic)
- Embedding vectors (recomputed per-device)
- Thumbnail cache (re-fetched from source URLs)
- Pipeline state (device-local)

### Cloud Crawler

Architecturally a virtual device in the sync protocol. Crawls 24/7, enriches with server-grade models, pushes results into the user's encrypted change event stream.

### Subscription Lapse

Sync stops, cloud features stop, all local data remains intact. App continues in local-only mode. Re-subscribing resumes sync. Cloud data retained 180 days after lapse.

See: [ADR-007](ADR/ADR-007-zero-knowledge-encrypted-sync.md), [007-sync-protocol.yaml](e2e-stories/007-sync-protocol.yaml)

---

## 13. Licensing Strategy

### Licensing Model (JetBrains-style)

- Account-based activation (sign in once, receive signed JWT)
- 30-day offline grace period before re-authentication
- Concurrent session limit: 5 devices (not per-device lock)
- No hardware fingerprinting (no MAC, serial, CPU ID, disk UUID)
- No API keys required from end users

### Anti-Piracy Stance

Cloud sync is the moat. A pirated binary produces a local-only, single-device experience with no sync, no cloud crawling, and no cross-device continuity. The premium subscription is genuinely more valuable than the binary. Sequential account sharing produces a merged, incoherent feed state that degrades naturally. Professional piracy is accepted as unpreventable and not worth an arms race.

See: [ADR-002](ADR/ADR-002-jetbrains-licensing-model.md)

---

## 14. Emacs Integration

Emacs is a first-class client. `actual-feed.el` connects to the local Kotlin/JVM backend via JSON-RPC 2.0 (Unix domain socket preferred, HTTP fallback). Same API as GUI clients — no Emacs-specific endpoints. Minimum: Emacs 27.1.

### Buffer Types

- Feed buffer: ranked items with score breakdown, keyboard navigation
- Item detail: full extraction results (summary, entities, claims, features)
- Search: FTS5 query results with highlighted snippets
- Tripwires: CRUD management, fire history
- Budget: ASCII progress bars per content type
- Pipeline: processing status, model info, re-extraction trigger

### Key Integrations

- **Org-mode**: Capture templates, custom `[[actual-feed:id]]` link type, agenda TODOs from tripwire fires, tag inheritance from topics
- **mpv**: Media playback via mpv.el IPC. Budget tracking through IPC. Budget enforcement via programmatic pause.
- **Evil mode**: Compatible keybindings via `evil-define-key`
- **consult/embark**: Completing-read search, contextual actions, marginalia annotations

All operations work from keyboard only. No web view or embedded browser.

See: [ADR-009](ADR/ADR-009-emacs-first-class-client.md), [008-emacs-client.yaml](e2e-stories/008-emacs-client.yaml)

---

## 15. Parental Controls & Content Maturity

### Core Capability

If the system cannot successfully filter age-inappropriate content, it has failed. Content maturity filtering is a hard capability — not a feature toggle, not premium, not V2. It ships in V1.

The system does not define "appropriate." The parent does, via a continuous maturity threshold [0,1] per child profile. The LLM extracts a content maturity signal for every item during Stage 2 — always, even in single-user mode. For child profiles, items above the threshold are hard pre-filtered (same mechanism as obliterate). Filtered items do not exist in the child's view.

### Profiles

Multiple profiles per installation. Owner profile (parent) can create, edit, and delete child profiles. Each profile has its own feed config, interaction history, neural taste model, and maturity threshold. Profile switching requires PIN/biometric to access the owner profile. Child profiles look and feel identical to the full app — no "kids mode" aesthetic.

### Maturity Assessment

The LLM evaluates six maturity categories: violence, sexual content, language, substances, graphic content, mature themes. Overall score [0,1] plus per-category breakdown. The prompt biases toward caution — false positives (over-filtering) are tolerable, false negatives are not.

Parents can: review all filtered items, whitelist specific items, set per-category threshold overrides (stricter on violence, more permissive on language), and receive weekly digests of borderline items.

### Model Swap Safety

When models are swapped, maturity scores may shift. Child profiles continue using old maturity scores until the parent reviews and confirms the new model's assessments. This prevents a model swap from silently exposing previously-filtered content.

See: [011-parental-controls-and-maturity-filtering.yaml](e2e-stories/011-parental-controls-and-maturity-filtering.yaml)

---

## 16. Onboarding & First Run

### Flow (4-6 screens)

1. **Activation**: Account sign-in (email + password or OAuth). 30 seconds.
2. **Source Selection**: Grid with checkboxes. Free sources pre-checked. Paid sources visible, unchecked, showing per-source cost. Compute budget bar updates live. 30-60 seconds.
3. **Topic Declaration**: Text input for interests. Suggested topics from source selection. Seeds topic_relevance signal. 30-60 seconds.
4. **Model Selection**: Auto-detected by device RAM. Three tiers: Recommended / Balanced / Lightweight. Budget bar updates. 15-30 seconds.
5. **Initial Crawl**: Feed builds visibly — items stream in, transition from raw to enriched, reorder as scores compute. 2-5 minutes to first usable feed.
6. **Optional Customization**: Budgets, tripwires, presets, sync. All deferrable to settings.

Every step skippable except activation. Defaults biased toward high-signal free sources (HN, curated RSS, GitHub). No tutorial overlays — target user is technically literate.

App ships with Qwen2.5-3B Q4 (~2GB) as the bundled fallback model for immediate basic enrichment (summary + features + maturity, lower quality than desktop-recommended Qwen3-30B-A3B). Recommended model downloads in background.

Second-device setup: sign in → sync → done (skips onboarding).

See: [010-onboarding-first-run.yaml](e2e-stories/010-onboarding-first-run.yaml)

---

## 17. Privacy & Trust

- All data collection is disclosed.
- No ad tracking.
- No sale of data.
- No opaque algorithmic behavior — score breakdown for every item.
- Sync encrypted client-side — server stores only ciphertext.
- Pipeline failures surfaced with reason codes — no silent degradation.
- Subscription lapse retains all local data.
- Every item links to its original source for verification.

Trust is treated as a product requirement, not a marketing claim.

---

## 18. Platform Asymmetry

Capabilities are honestly asymmetric across platforms. Every gap maps to a real hardware constraint — not artificial degradation.

| Capability | Desktop (32GB) | Mobile (8-12GB) | Cloud (Premium) |
|---|---|---|---|
| LLM | Qwen3-30B-A3B (MoE, Q4) | Phi-4 mini / Qwen2.5-3B | Server-grade |
| Transcription | Whisper Turbo (inline) | Moonshine (charging only) | Always-on |
| Crawling | Scheduled during idle | Active + idle | 24/7 |
| Tripwires | Evaluate on crawl | Evaluate on crawl | Continuous |
| Sync | N/A | N/A | Cross-device |

See: [ADR-012](ADR/ADR-012-honest-platform-asymmetry.md), [001-architecture-tiers.yaml](e2e-stories/001-architecture-tiers.yaml)

---

## 19. Success Metrics

Qualitative:
- users report reduced feed fatigue
- users trust why items appear
- users maintain long-term use without burnout

Quantitative:
- conversion from local-only to sync
- multi-device usage
- retention over months, not days
- low churn despite high price point

---

## 20. Open Questions

- Self-hosted sync option (future V2)

### Resolved

- ~~V1.0 MVP source scope~~ → 5 free adapters: RSS, HN, Web Articles, Podcasts, YouTube. See [ADR-014](ADR/ADR-014-v1-mvp-scope-and-implementation-plan.md).
- ~~Exact pricing tiers~~ → Desktop $79, Mobile $29, Cloud $9.99/mo + per-source add-ons. See [ADR-014](ADR/ADR-014-v1-mvp-scope-and-implementation-plan.md).
- ~~Autoplay mode definitions~~ → 4 modes (scan, deep, background, project-focused). See [012-autoplay-and-audio-feed.yaml](e2e-stories/012-autoplay-and-audio-feed.yaml).
- ~~Feature signal calibration~~ → Percentile normalization against user's own item history. See [ADR-015](ADR/ADR-015-feature-signal-calibration.md).
- ~~Desktop GUI framework~~ → Tauri 2.x + SolidJS. See [ADR-013](ADR/ADR-013-kotlin-multiplatform-toolchain.md).

---

## 21. Document Index

### Implementation Plan

- [implementation-plan.md](implementation-plan.md) — Task-level implementation plan. 9 phases (0-8), 45+ tasks, full story/constraint traceability. V1 = Phases 0-4 (desktop-only). See also [ADR-014](ADR/ADR-014-v1-mvp-scope-and-implementation-plan.md) for phase structure and pricing.
- [extraction-prompt.md](extraction-prompt.md) — Stage 2 LLM prompt template, JSON output schema, signal calibration guidance, token budget. The single most important piece of design in the system.
- [wireframes.md](wireframes.md) — ASCII wireframes for 4 core screens: Feed, Item Detail, Settings, Onboarding (6 screens). Interaction patterns for obliterate/exalt gestures and score breakdown.
- [project-tree.md](project-tree.md) — Canonical directory layout, file naming, module boundaries, runtime paths. **Authoritative for all path references.**
- [vocabulary.md](vocabulary.md) — Locked glossary of every primitive, concept, enum, and abbreviation. **Authoritative for all naming.**

### Architecture Decision Records (docs/ADR/)

| ADR | Title |
|---|---|
| [ADR-001](ADR/ADR-001-local-first-sqlite-storage.md) | Local-First SQLite Storage |
| [ADR-002](ADR/ADR-002-jetbrains-licensing-model.md) | JetBrains-Style Licensing Model |
| [ADR-003](ADR/ADR-003-three-layer-scoring.md) | Three-Layer Scoring Architecture |
| [ADR-004](ADR/ADR-004-platform-embedding-via-official-apis.md) | Platform Content Embedding via Official APIs |
| [ADR-005](ADR/ADR-005-per-content-type-budgets.md) | Per-Content-Type Budget System |
| [ADR-006](ADR/ADR-006-single-call-llm-extraction.md) | Single-Call LLM Feature Extraction |
| [ADR-007](ADR/ADR-007-zero-knowledge-encrypted-sync.md) | Zero-Knowledge Encrypted Client-Centric Sync |
| [ADR-009](ADR/ADR-009-emacs-first-class-client.md) | Emacs as First-Class Client via JSON-RPC |
| [ADR-010](ADR/ADR-010-natural-language-tripwires.md) | Natural Language Tripwires via LLM Evaluation |
| [ADR-011](ADR/ADR-011-no-silent-degradation.md) | No Silent Degradation Principle |
| [ADR-012](ADR/ADR-012-honest-platform-asymmetry.md) | Honest Platform Asymmetry |
| [ADR-013](ADR/ADR-013-kotlin-multiplatform-toolchain.md) | Kotlin Multiplatform Toolchain with Tauri Desktop Shell |
| [ADR-014](ADR/ADR-014-v1-mvp-scope-and-implementation-plan.md) | V1 MVP Scope & Phased Implementation Plan |
| [ADR-015](ADR/ADR-015-feature-signal-calibration.md) | Feature Signal Calibration via Percentile Normalization |

### End-to-End Stories (docs/e2e-stories/)

| File | Scope | User Stories | Constraints |
|---|---|---|---|
| [001](e2e-stories/001-architecture-tiers.yaml) | Architecture Tiers | US-001 to US-004 | EC-001–004, OC-001–004 |
| [002](e2e-stories/002-core-experience.yaml) | Core Experience | US-010 to US-018, US-015b, US-015c | EC-010–015, OC-010–015 |
| [003](e2e-stories/003-ranking-pipeline.yaml) | Ranking Pipeline | US-020 to US-025 | EC-020–023, OC-020–022 |
| [004](e2e-stories/004-budget-and-embedding.yaml) | Budget & Embedding | US-030 to US-036 | EC-030–034, OC-030–032 |
| [005](e2e-stories/005-feeditem-schema-and-extraction.yaml) | FeedItem Schema | US-040 to US-045 | EC-040–044, OC-040–043 |
| [006](e2e-stories/006-tripwires-and-events.yaml) | Tripwires & Events | US-050 to US-055 | EC-050–053, OC-050–053 |
| [007](e2e-stories/007-sync-protocol.yaml) | Sync Protocol | US-060 to US-065 | EC-060–063, OC-060–063 |
| [008](e2e-stories/008-emacs-client.yaml) | Emacs Client | US-070 to US-075 | EC-070–072, OC-070–072 |
| [010](e2e-stories/010-onboarding-first-run.yaml) | Onboarding | US-090 to US-095 | EC-090–093, OC-090–093 |
| [011](e2e-stories/011-parental-controls-and-maturity-filtering.yaml) | Parental Controls | US-100 to US-106 | EC-100–104, OC-100–103 |
| [012](e2e-stories/012-autoplay-and-audio-feed.yaml) | Autoplay & Audio Feed | US-110 to US-115 | EC-110–112, OC-110–112 |
| [013](e2e-stories/013-search-and-discovery.yaml) | Search & Discovery | US-120 to US-126 | EC-120–123, OC-120–123 |
| [014](e2e-stories/014-desktop-application-shell.yaml) | Desktop App Shell & Settings | US-130 to US-138 | EC-130–133, OC-130–133 |
| [015](e2e-stories/015-tuning-presets-and-custom-feeds.yaml) | Tuning Presets & Custom Feeds | US-140 to US-147 | EC-140–142, OC-140–143 |
| [016](e2e-stories/016-error-recovery-and-pipeline-transparency.yaml) | Error Recovery & Pipeline Transparency | US-150 to US-157 | EC-150–154, OC-150–152 |

**Totals**: 103 user stories, 57 collective lifetime stories, 63 enforced constraints, 57 opinionated constraints.

---

## 22. Summary

Actual Feed is **personal information infrastructure**, not media.

It succeeds by:
- restoring user control over every axis of information selection
- remaining infinite without manipulation
- compounding value through legitimacy and transparency
The product optimizes for **clarity, trust, and long-horizon thinking**.

The product makes you more intelligent, healthy, and deliberate with your life.
