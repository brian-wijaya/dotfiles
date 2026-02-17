# Vault Data Pipeline v2 — Authored Decision Surface

Status: DRAFT — interrogation in progress
Started: 2026-02-11

## Context

The vault data pipeline (VaultWatcher in kinetic) indexes documents from 6 source types into vault.db for FTS5 search and session memory. The pipeline currently has no topical/narrative tracking — topics exist only as hardcoded keyword matches on Claude sessions. The `tags` + `doc_tags` tables exist in schema but are empty. The user wants keyword/filename tagging that enables semantic/narrative tracing over time.

Current scale: 17,415 documents, 175,983 chunks, 2,446 sessions, 13,185 emails. DB: 1.7GB + 1.1GB WAL.

---

## User Stories

### US-1: Tag-filtered vault search
**As** a user searching my vault,
**I want** to filter search results by topic tags (e.g., "show me everything tagged project:kinetic"),
**so that** I can narrow 17K documents to a relevant subset before or alongside FTS5 text search.

**Acceptance**: `RECALL_search_hybrid` accepts optional `tags` parameter. Results filtered to documents having ALL specified tags. Tags combinable with text queries.

### US-2: Automatic topic extraction at index time
**As** a user adding new documents to my vault,
**I want** topics to be extracted automatically during indexing (from file paths, content keywords, and optionally LLM),
**so that** I don't need to manually tag anything.

**Acceptance**: Every newly indexed document gets >= 1 tag. Path-derived tags (zero-cost) always applied. Keyword-extracted tags applied for documents with sufficient content. Each tag records its extraction source (path, yake, llm, manual).

### US-3: Narrative timeline reconstruction
**As** a user reviewing my work history,
**I want** to ask "what happened with X over the last N days" and get a chronological topic-threaded timeline,
**so that** I can trace how a project/feature/bug evolved across sessions, documents, and emails.

**Acceptance**: A query like `RECALL_search_sessions(query="kinetic", recency_bias=0.5)` returns sessions tagged with project:kinetic, ordered by time, with co-occurring topics showing what else was active alongside.

### US-4: Context Deposits — ephemeral context preservation
**As** an agent working on a task,
**I want** to deposit structured contextual understanding (tags, decisions, causal links, outcomes) into the pipeline during my session,
**so that** the indexer receives high-fidelity metadata directly from the narrator (me) instead of reconstructing it from the cold transcript.

**Core principle**: The agent holds perfect contextual understanding during the conversation — topics, intent, narrative arc, decision significance, causal chains. This understanding is strictly higher quality than any post-hoc reconstruction. Depositing it costs nearly nothing. Not depositing it forces the indexer to approximate it with heuristics and LLM calls.

**Acceptance**:
- `RECALL_save_session` accepts `tags: string[]` parameter. Written to `doc_tags` with `source='agent'`.
- New `RECALL_deposit_context(event_type, payload)` tool for mid-session deposits. Stored in `context_deposits` table (SQLite, indexed). Replaces JSONL ledger as the structured event store.
- Indexer parses tool_use blocks from .jsonl to extract file paths touched. Path-derived tags propagate automatically.
- Agent-deposited tags take priority over YAKE. YAKE is fallback for non-agent documents.

**Event types (Level 3 — default, deposit on every occurrence):**
- `decision` — design/implementation choice with rationale
- `causal_link` — X caused/resolved/blocked Y
- `outcome` — result of a work sequence
- `topic_shift` — conversation pivot with old→new topic
- `question_open` / `question_resolved` — open questions paired with resolutions

**Event types (Level 4 — deposit at two strategic triggers only):**
- `observation` — noticed X while investigating Y
- `hypothesis` — X might be caused by Y
- `verification` — confirmed/refuted hypothesis by checking Z
- `pivot` — abandoned approach X for Y, with reason

**Level 4 triggers (the two moments where context loss is greatest):**
1. **topic_shift** — when depositing a topic_shift event, also deposit Level 4 events summarizing the reasoning path on the *outgoing* topic. Close the chapter: what was observed, what hypotheses were tested, what approaches were tried and abandoned. The outgoing topic's detailed context is about to fade from active attention.
2. **pre-compaction** — before context compaction, deposit everything in working memory that won't survive compression. All active reasoning chains, unresolved hypotheses, the narrative arc so far. This is the last-chance deposit — after compaction, the agent's detailed understanding is reduced to a summary.

**Agent preamble instruction**: "Deposit Level 3 context (decisions, causal links, outcomes, topic shifts, questions) as they occur. On topic_shift and before context compaction, also deposit Level 4 context (observations, hypotheses, verifications, pivots) summarizing the reasoning path."

**Ledger event types migrated to context_deposits:**
- `task_start` → `task_start` (same semantics)
- `task_complete` → `task_complete` (same semantics)
- `loose_end` → `loose_end` (same semantics)
- `question` → `question_open` (renamed for pairing with resolution)
- `blocker` → `blocker` (same semantics)
- `tangent_begin` / `tangent_end` → `tangent_begin` / `tangent_end` (same semantics)

**Storage**: `context_deposits` table in vault.db. Indexed by session, timestamp, event_type. Replaces JSONL ledger entirely.

### US-6: Worldview Model — perpetually evolving contextual memory
**As** an agent starting a new session,
**I want** the system to load a structured worldview model (environmental constants, active project states, settled decisions, active narrative threads) so that I start with deep contextual understanding without being told anything.

**Core principle**: Context deposits are how the worldview *learns*. The worldview is the *integrated memory* — perpetually updating, automatically loaded, shaping default assumptions and behavior. The agent doesn't query it; the agent operates *within* it.

**Four layers:**
1. **Environmental constants** (rarely changes): Hardware, OS, tools, preferences. Self-correcting — when an agent observes a change, it deposits the update and the worldview evolves.
2. **Project state** (weeks/months): Active projects, settled decisions, resolved bugs, architectural constraints. Accumulated from decision/outcome/causal_link deposits. Prevents re-litigation of settled questions.
3. **Active narratives** (days): Current work threads, recent topic shifts, open questions. The "what are we doing right now" surface. Accumulated from recent deposits.
4. **Deep history** (rarely accessed): Old narratives, completed projects, historical decisions. Available on demand, not loaded by default.

**Read-write loop:**
```
load_worldview → agent works → deposit_context → ... → next session → load_worldview (now richer)
```

**Acceptance:**
- `RECALL_load_worldview()` materializes relevant worldview at session start. Returns structured model: env constants, project states, settled decisions, active threads.
- `RECALL_deposit_context()` writes back — decisions settle, facts accumulate, narratives thread.
- After 30 days of use, the agent starts each session with accurate understanding of the user's environment, projects, preferences, and active work without explicit instruction.
- The worldview subsumes what MEMORY.md does manually today — but derived, structured, and self-correcting.

### US-7: Email visibility in search
**As** a user searching my vault,
**I want** emails to appear in hybrid search results alongside documents and conversations,
**so that** the 76% of my documents that are emails aren't invisible to search.

**Acceptance**: `RECALL_search_hybrid` returns email results. Emails chunked and indexed into chunks_fts via the standard documents → chunks pipeline. Email-specific metadata (sender, thread_id, etc.) preserved in the emails table. Content-hash fixed to be content-based, not path-based.

---

## Collective User Lifetime Stories

### CULS-1: Tag namespace growth under 10 years of use
**Given** 6 source types ingesting continuously for 10 years,
**When** the document count reaches 500K+ and tag count reaches 10K+,
**Then** tag queries (JOIN documents + doc_tags + tags) complete in < 50ms, tag-filtered FTS5 search in < 200ms, and the doc_tags junction table remains < 100MB.

**Scaling assumptions**: 50K docs/year, avg 5 tags/doc = 250K junction rows/year. At 10 years: 2.5M junction rows. SQLite handles this easily with proper indexes. The real risk is tag namespace pollution — thousands of near-duplicate tags (emacs, Emacs, emacs-config, emacs_config) degrading search precision. Mitigated by OC-6 (lowercase canonicalization).

### CULS-2: Extraction quality under adversarial input
**Given** that document sources include imported ChatGPT conversations (3,404), emails from untrusted senders (13,185), and Claude sessions with prompt injection in tool results,
**When** keyword extraction runs on these documents,
**Then** extracted tags reflect actual document topics, not injected instructions or spam content.

**Mitigation**: Path-derived tags are immune (filesystem structure is trusted). Statistical extraction (YAKE) is resistant (frequency-based, not instruction-following). LLM extraction is the only vulnerable tier — but it already runs in a subprocess with timeout, and bad tags are bounded (max N per document).

### CULS-3: Incremental adoption without reindex
**Given** that the current pipeline indexes 17K documents with no tags,
**When** topical tracking is added,
**Then** existing documents receive tags via background backfill, and the system functions correctly during the transition period (partial tagging).

**Acceptance**: Tag-filtered search returns results from tagged documents only (no false negatives from untagged docs — untagged docs appear in unfiltered searches). Backfill is interruptible, resumable, and idempotent.

### CULS-4: Context deposit volume under sustained agent use
**Given** that 10+ agent sessions run daily, each producing 8-15 Level 3 deposits plus Level 4 bursts at topic shifts and compaction,
**When** the system has been running for 3 years,
**Then** the context_deposits table contains ~150K-300K rows. Queries by session_source_id or time range complete in < 10ms via index. ReassembleLooseEnds (10-day lookback) returns in < 50ms. Table size remains < 100MB.

**Scaling**: 20 deposits/session × 10 sessions/day × 365 days = 73K rows/year. At 200 bytes avg payload: ~15MB/year. Trivially handled by SQLite with proper indexes.

---

## Enforced Constraints

### EC-1: SQLite single-writer
vault.db is WAL mode with a single writer (VaultWatcher). Tag extraction, context deposit insertion, and chunk writes must go through the same write path. No concurrent writes from separate processes. Context deposits from agents go through the MCP tool → SqliteClient write path (single connection, serialized).

### EC-2: No GPU, Java-only pipeline
The machine has no GPU. All extraction must be CPU-only. The kinetic pipeline is Java — no Python subprocess calls for extraction (LlmFactScorer's `claude -p` is the only subprocess call, and it's a CLI tool not a Python library). YAKE implemented natively in Java.

### EC-3: Subscription-only LLM billing
LLM calls use `claude -p --model haiku` (subscription). No per-token API cost. But each call has ~1-3s latency and 30s timeout. Budget: acceptable for new documents at ingest time, not acceptable for bulk backfill of 17K docs (would take 5-14 hours).

### EC-4: Existing schema compatibility
Both Java (kinetic) and Python (vault-rag) share vault.db. Schema changes must be additive (new tables, new columns, new indexes). Cannot drop or rename existing columns. The Python side may still write directly to vault.db. The JSONL ledger file is kept on disk during transition but no longer written to; ReassembleLooseEnds reads from context_deposits table. Ledger data migrated to context_deposits on first startup.

### EC-5: WAL size discipline
WAL is already 1.1GB. Any bulk operation (backfill) must checkpoint periodically to prevent unbounded WAL growth. Passive checkpoints only (non-blocking).

---

## Opinionated Constraints

### OC-1: Document-level tagging, not chunk-level
Tags are assigned to documents, not chunks. Rationale: primary use is search filtering and narrative threading, not sub-document attribution. FTS5 chunk-level search provides sub-document precision within a tagged document set — tags narrow the document scope, FTS5 MATCH narrows to specific chunks within those documents. Even for 700-chunk conversations spanning multiple days, the tag+FTS5 combination returns only the relevant chunks. Chunk-level tagging would 10-100x the junction table for minimal search quality gain.

**Status**: CONFIRMED.

### OC-2: Flat namespace with prefix conventions
Tags are flat strings with colon-delimited prefixes: `project:kinetic`, `lang:python`, `tool:emacs`, `activity:debugging`, `topic:display-isolation`. No hierarchical parent-child relationships in schema. Prefix-based queries (`tag LIKE 'project:%'`) provide faceted filtering. The `tags` table is a flat list; grouping is by convention only.

**Status**: CONFIRMED.

### OC-3: Three extraction tiers, all Java-native
1. **Path** (always, free, <1ms): Parse `source_path` into prefix-convention tags. Deterministic, human-curated signal.
2. **YAKE-in-Java** (always for content > 100 words, <10ms): Implement YAKE statistical keyword extraction natively in Java (~250 LOC, zero dependencies). Scores words by position, frequency, casing, context diversity, sentence dispersion. No stopword list, no trained model, no corpus statistics. Reference: yakejava GitHub port + original YAKE paper.
3. **LLM** (for conversations at index time, 1-3s): Expand existing `claude -p --model haiku` call in LlmFactScorer to also return `KEYWORDS: k1, k2, k3`. Combined with existing fact/title scoring — no additional LLM call needed.

Each tier stores its source in `doc_tags.source` column (`path`, `yake`, `llm`, `manual`, `agent`). Higher tiers don't override lower — all coexist. Confidence score on junction table for YAKE (statistical score) and LLM (1.0 fixed).

Backfill strategy: Tiers 1+2 run on all 17K existing docs in minutes (CPU-only). Tier 3 only on new conversations going forward.

**Status**: CONFIRMED. Java-only, no Python dependencies.

### OC-4: Emails get chunked into the main pipeline
The current email isolation (separate table, invisible to SearchHybrid) is a bug, not a feature. Emails should flow through the same documents → chunks → chunks_fts pipeline as everything else, with email-specific metadata preserved in the emails table. Content-hash changed from path-based to content-based (SHA-256 of body text). MIME decoding added for multipart messages.

**Status**: CONFIRMED.

### OC-5: Agent-deposited tags are primary for conversations, YAKE is fallback
For Claude conversations, the agent's own tag deposits (via save_session or deposit_context) are the primary metadata source. YAKE extraction runs only if agent tags are absent (cold-start, backfill, or pre-pipeline conversations). For all other document types (org, email, ChatGPT), YAKE is primary since no agent was involved. Path-derived tags (tier 1) always run regardless.

**Fidelity hierarchy**: Agent deposits > LLM reconstruction > YAKE statistical > path heuristic. Each tier fills gaps left by the one above. The pipeline degrades gracefully — worst case (no agent, no LLM, no content) you still get path-derived tags.

**Status**: CONFIRMED.

### OC-6: All-lowercase tag canonicalization
All tags lowercased on insert. `Emacs` → `emacs`, `Project:Kinetic` → `project:kinetic`. The tag namespace is a controlled vocabulary, not free text. No alias/synonym table initially — deferred unless namespace pollution becomes measurable (CULS-1 monitoring).

**Status**: CONFIRMED.

### OC-7: Context deposits replace the JSONL ledger
The `context_deposits` SQLite table replaces `RECALL_append_ledger_event` and the JSONL ledger file as the sole structured event store. All existing ledger event types (task_start, task_complete, loose_end, question, blocker, tangent_begin/end) migrate to context_deposits with identical semantics. `ReassembleLooseEnds` queries context_deposits instead of scanning the JSONL file.

**Rationale**: The JSONL ledger is an O(N) sequential scan on every query. Context deposits in SQLite are indexed by session, timestamp, and event_type. Same event semantics, strictly better query performance, unified storage with the rest of the pipeline.

**Migration**: On first startup after deployment, read existing JSONL ledger, insert events into context_deposits, rename ledger file to `.ledger.migrated`. `RECALL_append_ledger_event` tool is replaced by `RECALL_deposit_context` — same functionality, broader event type set, SQLite storage.

**Status**: CONFIRMED.

### OC-8: Level 4 deposits at strategic triggers only
Level 4 event types (observation, hypothesis, verification, pivot) are deposited only at two triggers:
1. **topic_shift** — summarize the outgoing topic's reasoning path before pivoting
2. **pre-compaction** — dump all active reasoning chains before context compression

Level 3 event types (decision, causal_link, outcome, topic_shift, question_open/resolved) are deposited as they occur, without restriction.

**Rationale**: Level 4 at every occurrence would add cognitive overhead to the agent without proportional retrieval value. At topic_shift and pre-compaction, the delta between "what the agent knows" and "what will survive" is maximal — these are the moments where Level 4 deposits have the highest preservation value per deposit.

**Status**: CONFIRMED.

### OC-9: Tag prefix vocabulary and path extraction rules
**Canonical prefixes:**

| Prefix | Meaning | Examples |
|--------|---------|----------|
| `project:` | Named project/system | `project:kinetic`, `project:somatic`, `project:vault-rag` |
| `lang:` | Programming language | `lang:java`, `lang:python`, `lang:elisp` |
| `tool:` | Software tool/application | `tool:emacs`, `tool:polybar`, `tool:sqlite`, `tool:ghostty` |
| `activity:` | What's being done | `activity:debugging`, `activity:refactoring`, `activity:design` |
| `topic:` | Domain/subject area | `topic:display-isolation`, `topic:fts5`, `topic:tagging` |
| `source:` | Document origin | `source:claude`, `source:email`, `source:org`, `source:chatgpt` |
| `env:` | Environment/hardware fact | `env:no-gpu`, `env:arch-linux`, `env:ultrawide` |
| `preference:` | User preference | `preference:java-over-python`, `preference:no-symlinks` |

**Path extraction rules per source type:**

```
source_type=claude (source_path="claude:<session-id>"):
  → source:claude
  → file-touch propagation: parse tool_use blocks from .jsonl,
    resolve file paths → derive project:/lang: tags from touched files

source_type=org (source_path="org/journal.org"):
  → source:org
  → directory after "org/" as topic if meaningful (skip common names like "archive")
  → org-mode #+FILETAGS if present → direct tag mapping

source_type=email (source_path="email://account/folder/..."):
  → source:email
  → source:email:<account> (e.g., source:email:gmail)
  → folder as topic if meaningful (skip INBOX, Sent, Trash, Drafts)

source_type=chatgpt (source_path="chatgpt:<conv-id>"):
  → source:chatgpt

source_type=markdown (source_path="docs/adr/ADR-015.md"):
  → source:markdown
  → directory components as topics: "docs/adr" → topic:adr
  → parent project from path (e.g., "kinetic/docs/" → project:kinetic)

source_type=onenote (source_path="onenote/..."):
  → source:onenote

General rules:
  → File extension → lang: tag (.java→lang:java, .py→lang:python,
    .el→lang:elisp, .rs→lang:rust, .ts→lang:typescript, .org→skip)
  → Known project directories → project: tag
    (~/vault/programs/kinetic/ → project:kinetic,
     ~/vault/programs/somatic/ → project:somatic)
  → All tags lowercased
  → Skip noise directories: node_modules, .git, __pycache__, .cache, build, target
```

**Status**: CONFIRMED.

### OC-10: Worldview materialization — hybrid with caching
`RECALL_load_worldview()` dynamically assembles the worldview from indexed SQLite queries, cached per session, invalidated when new deposits are made. Assembly queries:

1. **Environmental constants**: `event_type='decision'` deposits tagged `env:*` or `preference:*`, last-writer-wins per tag
2. **Project state**: Most recent `outcome` and `decision` deposits per `project:*` tag
3. **Active narratives**: Deposits from last N days, grouped by topic thread (co-occurring tags)
4. **Settled decisions**: `decision` deposits not superseded by later decisions on the same topic

**Status**: CONFIRMED.

### OC-11: MEMORY.md as manual override layer
MEMORY.md remains the user's explicit preference surface (authoritative intent). The worldview model provides derived understanding (accumulated from deposits). When they conflict, MEMORY.md wins. They coexist:
- MEMORY.md: "always use bun" (user said this)
- Worldview: "user prefers Java over Python for pipeline code" (derived from decisions)

The agent loads both: MEMORY.md (via auto-memory system) + worldview (via `load_worldview()`). MEMORY.md entries are treated as higher authority than worldview entries.

**Status**: CONFIRMED.

---

## Schema Changes

### Modified: `doc_tags` (add source + confidence columns)
```sql
ALTER TABLE doc_tags ADD COLUMN source TEXT DEFAULT 'auto';
ALTER TABLE doc_tags ADD COLUMN confidence REAL DEFAULT 1.0;
```
Source values: `path`, `yake`, `llm`, `agent`, `manual`, `auto` (legacy/default).

### New: `context_deposits` (replaces JSONL ledger)
```sql
CREATE TABLE IF NOT EXISTS context_deposits (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_source_id TEXT,
    timestamp TEXT NOT NULL,
    event_type TEXT NOT NULL,
    payload TEXT NOT NULL,
    created_at TEXT DEFAULT (datetime('now'))
);
CREATE INDEX IF NOT EXISTS idx_deposits_session ON context_deposits(session_source_id);
CREATE INDEX IF NOT EXISTS idx_deposits_time ON context_deposits(timestamp);
CREATE INDEX IF NOT EXISTS idx_deposits_type ON context_deposits(event_type);
```

Event types: `decision`, `causal_link`, `outcome`, `topic_shift`, `question_open`, `question_resolved`, `observation`, `hypothesis`, `verification`, `pivot`, `task_start`, `task_complete`, `loose_end`, `blocker`, `tangent_begin`, `tangent_end`.

Payload is JSON, structure varies by event_type. Examples:
```json
{"type": "decision", "description": "Chose YAKE over RAKE", "rationale": "No stopword dependency, single-document scoring", "tags": ["project:kinetic", "topic:keyword-extraction"]}
{"type": "causal_link", "from": "System.exit(0) in parent-watchdog", "relation": "caused", "to": "Zombie kinetic processes accumulating", "tags": ["project:kinetic", "activity:debugging"]}
{"type": "topic_shift", "from_topic": "kinetic lifecycle debugging", "to_topic": "pipeline redesign", "summary": "Lifecycle issue resolved, pivoting to topical tracking"}
{"type": "task_start", "event_id": "task-001", "description": "Implement YAKE in Java", "priority": "high"}
```

---

## Resolved Questions

- **Q1**: Document-level tagging. CONFIRMED. FTS5 chunk search gives sub-document precision.
- **Q2**: Flat namespace with colon-delimited prefixes. CONFIRMED.
- **Q3**: YAKE implemented natively in Java (~250 LOC). CONFIRMED. No Python.
- **Q4**: Email chunking into main pipeline. CONFIRMED. Part of this work.
- **Q5**: All-lowercase canonicalization. CONFIRMED.
- **Q7**: Context deposits replace JSONL ledger. CONFIRMED (Option B — full migration).

## Open Questions

- **Q6**: ~~Path extraction mapping~~ RESOLVED. See OC-9.
- **Q8**: ~~How should `ReassembleLooseEnds` evolve?~~ → Subsumed by Q9/worldview model design.
- **Q9**: ~~Should there be a RECALL_query_deposits tool?~~ → Reframed: context deposits feed a **worldview model**, not a query interface. The tool is `RECALL_load_worldview()` (read) + `RECALL_deposit_context()` (write). See US-6. The worldview is loaded at session start, not queried on demand. ReassembleLooseEnds evolves into (or is replaced by) load_worldview.
- **Q10**: ~~Tag prefix vocabulary~~ RESOLVED. See OC-9.
- **Q11**: ~~Worldview materialization~~ RESOLVED: Hybrid — dynamic assembly from indexed SQLite queries, cached per session, invalidated on new deposits. Assembly is cheap (indexed queries), caching prevents redundant assembly within a session.
- **Q12**: ~~MEMORY.md vs worldview~~ RESOLVED: MEMORY.md is a manual override layer (authoritative user intent). Worldview is derived understanding (accumulated from deposits). When they conflict, MEMORY.md wins. They coexist — MEMORY.md for explicit preferences, worldview for inferred knowledge.
