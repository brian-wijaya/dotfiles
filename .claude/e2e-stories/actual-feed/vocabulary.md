# Vocabulary — Actual Feed

**This document is the canonical authority for all domain terminology.** Every term has exactly one meaning. Implementation agents must use these exact names in code, schema, UI text, logs, and documentation. Synonyms are listed only to reject them.

Last updated: 2026-02-11

---

## Primitives

### Pipeline States

The lifecycle of a content item. Stored as TEXT in `feed_items.pipeline_state`. Transitions are forward-only except `failed` (which can retry to any earlier state).

| Value | Meaning | Transition from |
|---|---|---|
| `raw` | Fetched by Stage 1 adapter, not yet processed | (initial state) |
| `transcribing` | Audio/video transcription in progress | `raw` |
| `extracting` | LLM feature extraction in progress | `raw` or `transcribing` |
| `enriched` | All 13 signals populated, ready for scoring | `extracting` |
| `failed` | Error during processing, reason code stored | any state |

**Rejected synonyms**: "pending", "queued", "processing", "complete", "done", "error". Do not use these.

---

### Feature Signals (13 total)

Every enriched item carries exactly 13 named signals. All are `Float` in [0, 1]. Stored as columns on `feed_items`. Grouped by computation source.

#### LLM-Extracted Signals (8)

Produced by the Stage 2 extraction prompt. Defined in `docs/extraction-prompt.md`.

| Signal | Column name | What it measures |
|---|---|---|
| `topic_relevance` | `topic_relevance` | Match to user's declared topics. 0 = no overlap, 1 = deep coverage. |
| `nominal_density` | `nominal_density` | Ratio of specific, named referents (people, dates, quantities) to total volume. 0 = abstract, 1 = referent-dense. |
| `credibility` | `credibility` | Quality of sourcing: named sources, cited evidence, transparent methodology. 0 = unsourced speculation, 1 = rigorous. |
| `actionability` | `actionability` | Concrete, executable guidance. 0 = pure observation, 1 = specific steps/tools/commands. |
| `hype_score` | `hype_score` | Promotional/sensationalist language. 0 = measured, 1 = maximally promotional. **Typically negative-weighted.** |
| `vagueness` | `vagueness` | Avoidance of commitment: hedging, passive voice, weasel words. 0 = specific and falsifiable, 1 = entirely vague. **Typically negative-weighted.** |
| `repetition` | `repetition` | Semantic redundancy within the content. 0 = every paragraph adds new info, 1 = single point restated. **Typically negative-weighted.** |
| `missing_sources` | `missing_sources` | Proportion of claims that should be sourced but aren't. 0 = all sourced, 1 = all unsupported. **Typically negative-weighted.** |

#### Post-LLM Computed Signals (4)

Computed in `PostLlmSignals.kt` after LLM extraction completes. No LLM call.

| Signal | Column name | Computation |
|---|---|---|
| `freshness` | `freshness` | `1.0 / (1.0 + hours_since_published / 168.0)` — half-life 1 week. |
| `novelty` | `novelty` | `1.0 - max_cosine_similarity(item_embedding, recent_100_embeddings)` |
| `source_authority` | `source_authority` | User config lookup. Exalted source = 1.0, default = 0.5, per-source configurable. |
| `modality_preference` | `modality_preference` | User config lookup. Per-modality weight reflecting user's content type preference. |

#### Safety Signal (1)

| Signal | Column name | Notes |
|---|---|---|
| `content_maturity` | `content_maturity` | [0, 1] maturity score + 6 category subscores. **NOT percentile-normalized** — uses raw LLM scores because parental controls need absolute thresholds. Hard pre-filter, not a ranking signal. |

**Column naming convention**: Signal columns on `feed_items` use the signal name directly — no prefix. The names are already unique and self-documenting. SQLDelight catches any collision at compile time.

**Rejected synonyms**: "feature", "score", "metric", "attribute", "dimension". The word is **signal**. A collection of signals is a **feature vector**. The output of Stage 3 is a **score**.

---

### Interaction Types

User feedback gestures. Stored as TEXT in `user_interactions.interaction_type`. Append-only — interactions are never modified or deleted.

| Value | Trigger | Effect on scoring |
|---|---|---|
| `thumbs_up` | Single thumbs-up tap | Positive training signal (label=1.0) |
| `thumbs_down` | Single thumbs-down tap | Negative training signal (label=0.0) |
| `obliterate` | Rapid double thumbs-down within 2000ms + confirm | Source hard pre-filtered from feed |
| `exalt` | Rapid double thumbs-up within 2000ms + confirm | Source authority locked to 1.0 |
| `skip` | Scrolled past without interaction | Weak negative (label=0.3) |
| `play_start` | Media playback initiated | Implicit positive |
| `play_pause` | Media paused (manual, not budget) | Neutral |
| `play_complete` | Media played to end | Strong positive |
| `more_like_this` | Kebab menu selection | Positive + topic boost |
| `less_like_this` | Kebab menu selection | Negative + topic suppress |
| `already_seen` | Kebab menu selection | Suppress duplicates of this item |
| `often_revisit` | Kebab menu selection | Freshness decay exemption |
| `not_relevant_anymore` | Kebab menu selection | Time-bounded suppression (30 days) |
| `misleading_spam` | Kebab menu selection | Credibility decay for source |

**Rejected synonyms**: "feedback", "action", "event", "signal" (signal = feature signal), "gesture" (gesture = UI pattern that produces an interaction). The word is **interaction**.

---

### Source Platforms

Where content comes from. Stored as TEXT in `feed_items.source_platform`.

| Value | Adapter | Auth | V1 |
|---|---|---|---|
| `rss` | `RssAdapter` | None | Yes |
| `hackernews` | `HackerNewsAdapter` | None | Yes |
| `youtube` | `YoutubeAdapter` | None (yt-dlp) | Yes |
| `web_article` | `WebArticleAdapter` | None | Yes |
| `podcast` | `PodcastAdapter` | None | Yes |
| `twitter_x` | (Phase 6) | X API v2 ($100/mo) | No |
| `reddit` | (Phase 6) | Reddit OAuth2 | No |
| `stackoverflow` | (Phase 6) | Stack Exchange API | No |
| `github` | (Phase 6) | GitHub API | No |

**Rejected synonyms**: "provider", "feed", "channel", "origin". The word is **source** (noun) or **adapter** (the code that fetches from a source). A **source platform** is the type. A **source** is a specific instance (e.g., a particular RSS feed URL).

---

### Modalities

Content's primary medium. Stored as TEXT in `feed_items.modality`.

| Value | Meaning |
|---|---|
| `text` | Written article, blog post, social media text |
| `audio` | Podcast, audio recording |
| `video` | YouTube video, video recording |
| `image` | Image post (V1.1+, not used in V1) |

**Rejected synonyms**: "type", "format", "media_type", "content_type" (content_type = budget category). The word is **modality**.

---

### Error Codes

Structured failure reasons. Stored as TEXT in `pipeline_log.error_code`. Always SCREAMING_SNAKE_CASE.

#### Network errors
`NETWORK_TIMEOUT`, `NETWORK_UNREACHABLE`, `NETWORK_DNS_FAILURE`, `NETWORK_TLS_ERROR`, `NETWORK_HTTP_4XX`, `NETWORK_HTTP_5XX`

#### LLM errors
`LLM_OOM`, `LLM_MODEL_NOT_LOADED`, `LLM_TIMEOUT`, `LLM_INVALID_OUTPUT`, `LLM_SERVER_FAILED`, `LLM_CONTEXT_OVERFLOW`

#### Transcription errors
`TRANSCRIPTION_TIMEOUT`, `TRANSCRIPTION_FORMAT_UNSUPPORTED`, `TRANSCRIPTION_MODEL_NOT_LOADED`, `TRANSCRIPTION_DECODE_ERROR`, `TRANSCRIPTION_SERVER_FAILED`

#### Embedding errors
`EMBEDDING_COMPUTE_ERROR`, `EMBEDDING_MODEL_NOT_LOADED`

#### Storage errors
`STORAGE_QUOTA_EXCEEDED`, `STORAGE_CORRUPTION`, `STORAGE_MIGRATION_FAILED`

#### Platform errors
`PLATFORM_RATE_LIMITED`, `PLATFORM_AUTH_EXPIRED`, `PLATFORM_CONTENT_REMOVED`, `PLATFORM_API_CHANGED`

#### App errors
`MEDIA_PLAYER_MISSING`

**Rejected synonyms**: "error_type", "failure_reason", "status". The word is **error code**. Human-readable text goes in `error_message`.

---

### Budget Types

Content consumption categories. Stored as TEXT in `budget_ledger.content_type`.

| Value | Unit | Tracked by |
|---|---|---|
| `video_minutes` | Minutes | Embed play time |
| `audio_minutes` | Minutes | Audio play time |
| `social_minutes` | Minutes | Time spent on social media items |
| `article_count` | Count | Number of articles opened |

**Rejected synonyms**: "limit", "quota", "allowance", "cap". The word is **budget**. A budget has a **limit** (the configured max) and **consumption** (current usage).

---

### Budget Modes

| Value | Behavior at 100% |
|---|---|
| `soft` | Warning at 80%, dismissable pause at 100% |
| `hard` | Warning at 80%, non-dismissable pause at 100% (PIN override) |

---

### Budget Periods

| Value | Reset |
|---|---|
| `daily` | Midnight local time (configurable) |
| `weekly` | Configurable day-of-week |
| `rolling_24h` | Continuous sliding window |

---

### Maturity Categories

Subcategories of `content_maturity`. Each is a `Float` in [0, 1]. Stored as JSON in `feed_items.maturity_flags_json`.

| Value | What it detects |
|---|---|
| `violence` | Physical violence, warfare, graphic injury |
| `sexual` | Sexual content, nudity, sexual language |
| `language` | Profanity, slurs, vulgar language |
| `substances` | Drug use, alcohol abuse, substance promotion |
| `graphic` | Gore, disturbing imagery, shock content |
| `mature_themes` | Death, mental illness, abuse, complex moral content |

---

### Claim Types

How a claim is epistemically grounded. Stored in `claims_json` per item.

| Value | Meaning |
|---|---|
| `factual` | Verifiable statement about the world |
| `analytical` | Interpretation or conclusion drawn from evidence |
| `predictive` | Forward-looking statement about future events |
| `opinion` | Subjective judgment or preference |

---

### Source Types (within content)

How sources cited within a content item are categorized. Stored in `primary_sources_json` per item.

| Value | Meaning |
|---|---|
| `primary_research` | Peer-reviewed paper, original dataset, official specification |
| `official_statement` | Press release, company blog, government publication |
| `expert_opinion` | Named expert quoted or interviewed |
| `anonymous` | "Sources familiar with the matter", unnamed insiders |
| `self_referential` | Author cites their own previous work |
| `none` | No source cited for this claim |

---

### Entity Types

Named entities extracted from content. Stored in `entities_json` per item.

| Value | Meaning |
|---|---|
| `person` | Named individual |
| `organization` | Company, institution, government body |
| `product` | Named software, hardware, service |
| `technology` | Protocol, framework, algorithm, standard |
| `location` | City, country, region |
| `event` | Conference, incident, launch, election |
| `concept` | Abstract idea, theory, movement |

---

### Autoplay Modes

| Value | Behavior |
|---|---|
| `scan` | Piper TTS reads summaries only. 15-20 items per 10 minutes. |
| `deep` | Full original content. Video plays natively, articles TTS'd. |
| `background` | Short items (<5min) play full, long items get TTS summary. |
| `project_focused` | Queue filtered to active custom feed/preset. |

---

### Tuning Presets (Shipped Defaults)

Read-only. Users can clone to create editable copies. Names are `snake_case`.

| Name | Character |
|---|---|
| `deliberate` | High credibility, low hype/vagueness, low serendipity |
| `discovery` | High novelty, high serendipity, high neural weight |
| `balanced` | Middle of everything |
| `deep_research` | Maximum credibility, maximum hype suppression |
| `breaking_news` | High freshness, neutral hype, moderate serendipity |
| `background_audio` | Moderate everything, tuned for passive listening |

---

## Architecture Concepts

### Stage 1 — Crawl (Candidate Generation)

Source adapters fetch raw content from external platforms, normalize it to `FeedItem` schema, insert into SQLite with `pipeline_state=raw`. Pure I/O. No ML. No scoring.

**Rejected synonyms**: "fetch", "ingest", "import", "sync". The verb is **crawl**. The code is an **adapter**. The output is a **raw item**.

### Stage 2 — Extract (Feature Extraction)

Single-call LLM prompt processes each item. Produces: summary, entities, claims, sources, topics, 8 LLM signals, maturity assessment, tripwire evaluations. Post-LLM computes 4 additional signals. Percentile normalization applied.

**Rejected synonyms**: "analyze", "process", "enrich" (acceptable in casual use but "extract" is canonical), "summarize" (summarization is one output of extraction, not the operation itself). The verb is **extract**. The prompt is the **extraction prompt**. The output is an **enriched item**.

### Stage 3 — Score (Scoring & Ordering)

Three-layer blend: formula + neural + exploration. Hard pre-filters (obliterate, maturity) execute before scoring.

**Rejected synonyms**: "rank" (acceptable in casual use but "score" is the operation, "rank" is the result), "sort", "order". The verb is **score**. The output is a **scored item** with a **score breakdown**.

### Layer 1 — Formula (Explicit)

Weighted linear combination of 12 ranking signals (content_maturity is a pre-filter, not a ranking signal). Fully visible, editable, deterministic. Defined by a **tuning preset**.

### Layer 2 — Neural Taste (Learned)

Small MLP (~1MB ONNX) trained on interaction log. Input: 12 normalized signals. Output: single preference score [0, 1]. User-owned, local-only, resettable.

**Rejected synonyms**: "AI model", "recommendation engine", "ML model". The term is **neural taste model** or **taste model**.

### Layer 3 — Exploration (Probabilistic)

Thompson sampling. Maintains Beta distribution per item from interactions. Surfaces items outside top-N to prevent filter bubbles.

### Blend Formula

```
final = (1 - serendipity) × [taste_slider × neural + (1 - taste_slider) × formula] + serendipity × exploration
```

- `taste_slider`: 0% = pure formula, 100% = pure neural
- `serendipity`: 0% = deterministic, 50% = max (capped)

### Percentile Normalization

Each signal is normalized against the user's own item history via a 100-bin histogram. Raw values map to percentile rank [0, 1]. Activates after 100 items (cold start: raw scores). Content maturity exempt. New model = new histogram.

---

## Product Concepts

### Obliterate

Terminal negative gesture. Rapid double thumbs-down within 2000ms + confirmation. Adds source (author_id or source_domain) to `obliterated_sources`. Items from obliterated sources are hard pre-filtered — they never appear in the feed. Reversible from settings.

**What it does NOT do**: trigger notifications, affect budgets, delete existing items from database. It is purely a **feed filter**.

### Exalt

Terminal positive gesture. Rapid double thumbs-up within 2000ms + confirmation. Adds source to `exalted_sources`. Sets `source_authority = 1.0` for items from that source. Visual badge (star) in feed.

**What it does NOT do**: trigger notifications (that's tripwires), exempt from budget enforcement (budgets are budgets), bypass maturity filtering. It is purely a **scoring boost**.

### Tripwire

Natural language event-driven condition. Evaluated by the LLM during Stage 2 extraction — no separate call. When content matches a tripwire condition with confidence >= threshold, a **tripwire fire** is recorded and a notification sent.

**Rejected synonyms**: "alert", "filter", "notification rule", "watch", "monitor". The word is **tripwire**. It **fires**. A record of a firing is a **tripwire fire**.

### Custom Feed

User-created filtered view by modality, source platform, or topic. Each has its own tuning preset. Max 20 per profile.

**Rejected synonyms**: "channel", "tab", "collection", "stream". The term is **custom feed**. The main feed (unfiltered) is the **main feed**.

### Profile

Identity container. Owner (parent) or child. Each profile has its own: interaction history, neural taste model, preset selection, maturity threshold, budget config. Profile switching requires PIN for owner access.

**Rejected synonyms**: "account", "user" (user = the human; profile = a configuration within the app). The word is **profile**.

### Compute Budget Bar

Visual indicator showing estimated daily compute allocation across pipeline stages. Updates live as sources are toggled or models changed. Not the same as content budgets.

**Disambiguation**: "compute budget" = pipeline resource allocation. "content budget" = user's consumption time limits. Always qualify which one.

### Score Breakdown

Per-signal contribution display. Shows each signal's raw value, its weight in the active preset, and its contribution to the final score. Also shows formula/neural/blend totals. Available on every item.

---

## Abbreviations

| Abbreviation | Expansion | Context |
|---|---|---|
| BM25 | Best Match 25 | FTS5 ranking function |
| DJL | Deep Java Library | Neural model training (JVM) |
| FTS5 | Full-Text Search 5 | SQLite full-text search engine |
| GGUF | GPT-Generated Unified Format | Quantized LLM model file format |
| KMP | Kotlin Multiplatform | Cross-platform engine strategy |
| LLM | Large Language Model | Inference engine for extraction |
| MLP | Multi-Layer Perceptron | Neural taste model architecture |
| MoE | Mixture of Experts | Qwen3-30B-A3B architecture |
| ONNX | Open Neural Network Exchange | Portable model inference format |
| PRD | Product Requirements Document | docs/PRD.md |
| TTS | Text-to-Speech | Piper engine for autoplay |
| UDS | Unix Domain Socket | JSON-RPC transport (Linux/macOS) |
| UUIDv7 | UUID Version 7 | Time-sortable unique identifier |

---

## JSON-RPC Method Namespace

All API methods use dot-namespaced camelCase. The namespace is the domain, the method is the operation.

| Namespace | Scope |
|---|---|
| `feed.*` | Feed items: list, get, score, interact |
| `search.*` | Full-text search and similarity |
| `tripwires.*` | Tripwire CRUD and fire history |
| `pipeline.*` | Pipeline status and retry |
| `settings.*` | App configuration |
| `adapters.*` | Source adapter management |
| `profiles.*` | Profile CRUD and switching |
| `presets.*` | Tuning preset CRUD |
| `budgets.*` | Budget status and configuration |
| `autoplay.*` | Autoplay queue and controls |

---

## CSS Naming Convention

Plain CSS. No framework (no Tailwind, no CSS-in-JS, no CSS modules). Custom properties for theming. Component-scoped class names to avoid collisions.

### Class Naming

Component-scoped BEM pattern: `.component__element--modifier`

| Pattern | Example | When |
|---|---|---|
| `.component` | `.feed-card` | Block: top-level component container |
| `.component__element` | `.feed-card__title` | Element: child within a block |
| `.component__element--modifier` | `.feed-card__title--failed` | Modifier: variant state |
| `.component--modifier` | `.feed-card--exploration` | Block modifier: variant of entire block |

Component names use **kebab-case**, matching directory names. One CSS file per component or per component directory.

### File Organization

| File | Scope |
|---|---|
| `globals.css` | CSS custom properties (colors, spacing, typography, z-index), reset, body defaults |
| `components/<dir>/<Component>.css` | Scoped to that component. Imported in the `.tsx` file. |

### Custom Properties

All design tokens are CSS custom properties on `:root`. No magic numbers in component CSS.

```css
:root {
  /* Colors */
  --color-bg:            #ffffff;
  --color-bg-surface:    #f8f9fa;
  --color-text:          #1a1a1a;
  --color-text-muted:    #6b7280;
  --color-border:        #e5e7eb;
  --color-accent:        #2563eb;
  --color-score-high:    #16a34a;
  --color-score-mid:     #ca8a04;
  --color-score-low:     #dc2626;
  --color-badge-exalt:   #eab308;
  --color-badge-explore: #8b5cf6;
  --color-badge-failed:  #ef4444;

  /* Spacing (4px base) */
  --space-1: 4px;
  --space-2: 8px;
  --space-3: 12px;
  --space-4: 16px;
  --space-6: 24px;
  --space-8: 32px;

  /* Typography */
  --font-sans:  system-ui, -apple-system, sans-serif;
  --font-mono:  ui-monospace, 'Cascadia Code', 'Fira Code', monospace;
  --text-xs:    0.75rem;
  --text-sm:    0.875rem;
  --text-base:  1rem;
  --text-lg:    1.125rem;
  --text-xl:    1.25rem;

  /* Layout */
  --sidebar-width: 240px;
  --toolbar-height: 48px;
  --card-max-width: 720px;

  /* Z-index scale */
  --z-dropdown:  100;
  --z-overlay:   200;
  --z-modal:     300;
  --z-toast:     400;

  /* Transitions */
  --duration-fast: 100ms;
  --duration-base: 200ms;
  --duration-slow: 400ms;
}
```

**Rejected approaches**: Tailwind, CSS modules, styled-components, Emotion, Sass/SCSS. Plain CSS with custom properties. No build-time CSS transformation beyond standard Vite handling.

---

---

## Words That Mean Different Things

| Word | In this project | NOT this |
|---|---|---|
| **signal** | One of 13 feature dimensions on a feed item | A UI event or OS signal |
| **score** | The final output of Stage 3 scoring [0, 100] | A signal value or a game score |
| **source** | Where content comes from (RSS feed, YT channel) | Source code |
| **adapter** | Code that crawls a specific source platform | Design pattern adapter |
| **extraction** | Stage 2 LLM processing that produces structured data | Text extraction from PDF |
| **enriched** | Item has all 13 signals populated | Data enrichment in ETL |
| **preset** | Named configuration of all tuning sliders | Default settings |
| **budget** | Time/count limit on content consumption | Financial budget |
| **fire** (verb) | A tripwire matching content | Terminate employment |
| **profile** | App identity container (owner or child) | User profile on a social network |
| **modality** | Content medium (text/audio/video) | Learning modality |
| **obliterate** | Terminal negative gesture on a source | Delete or destroy |
| **exalt** | Terminal positive gesture on a source | Praise or worship |
