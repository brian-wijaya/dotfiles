# Loose End: Autonomous Mode Family

**Date**: 2026-02-13
**Status**: Conceptual — needs authorial extraction
**Source**: User proposal during actual-server extraction session

---

## Two Axes

**Human involvement**: synchronous (every step) vs asynchronous (grading/review) vs none
**Continuity**: session-bounded vs 24/7

| Mode | Human involvement | Continuity | Who leads |
|---|---|---|---|
| Authorial extraction (exists) | Synchronous | Session | Human leads |
| Infancy mode | None (autonomous) | Session | Agent leads |
| Self-parenting mode | Async grading | 24/7 | Agent team |
| Parent-parenting mode | Async grading | 24/7 | Agent team (with meta-feedback) |
| Authorial-proposal mode | Async grading | 24/7 | Agent team (with digital twin) |

---

## Mode Definitions

### 1. `/authorial-extraction-mode` (exists)
Human in the loop at every step. Human leads discussion. Agent researches, proposes, and produces artifacts. Human approves, corrects, or redirects. Synchronous.

### 2. `/infancy-mode`
**Autonomous, session-bounded.** Agent leads the discussion, researches, and proposes. No human input during execution. The critical distinction: all assumptions — from mild to bold — are **fully externalized through introspection, then documented** with confidence ratings. The agent produces authorial extraction artifacts with its assumptions visible and graded.

**Key property**: Assumption externalization is not optional overhead — it IS the output. The artifacts include the reasoning, the confidence levels, and the assumption chain. A human reviewing after the fact can see exactly where the agent was confident vs. speculative.

This is the bridge between human-led and fully autonomous. The agent learns to operate without real-time human feedback by making its uncertainty legible.

### 3. `/self-parenting-mode`
**Autonomous, 24/7.** Agent team (parent + child roles) conducts authorial extraction continuously. Infrastructure: Ralph, OpenClaw, or actual's own scheduling (Temporal/Kafka).

**Parent agent(s)**: Interrogate, challenge, demand specificity, enforce constraint collapse.
**Child agent(s)**: Propose, explore, generate, defend.

**Async human feedback via watched files**: The authorial extraction artifacts live in a directory structure that the human can edit. When a file is saved:
- Edits are detected (filesystem watch)
- Each edit is classified on a spectrum: "human made it perfect" → "human indicated need for full rewrite"
- Human can also write **reviews** — standalone text that becomes part of the intellectual corpus
- The system incorporates the feedback into subsequent cycles

### 4. `/parent-parenting-mode`
**Autonomous, 24/7.** Same as self-parenting, but the child role agents provide constructive feedback to the parent about parenting quality. Meta-level: "Your question was too vague," "You accepted that too easily," "You missed this contradiction."

Both roles co-evolve. Parent gets better at interrogation. Child gets better at proposing.

Same async human grading system as self-parenting mode.

### 5. `/authorial-proposal-mode`
**Autonomous, 24/7.** Long-running process that:
- Creates and maintains a **digital twin** of the user — preferences, personality, decision patterns, aesthetic biases, risk tolerance, communication style — derived from existing session data, reviews, edits, vault contents
- Uses the twin to simulate the user's presence as the "author" in authorial extraction sessions
- Produces **git-versioned** authorial extraction artifacts (branches, or whatever versioning system Ralph uses)
- **Standardized format**: output shape never changes — same sections, same structure, same length constraints
- **Only quality improves**: depth, precision, accuracy, recency, relevancy increase over time as the system incorporates new field developments, independent research, and introspection
- Latest version always ready for human review — diff against previous shows exactly what changed

Same async human grading system. The twin improves from grading data (human edits correct the twin's model of what the human would say).

---

## Async Human Feedback System (modes 3-5)

The artifact directory is a **watched filesystem**. Human interaction is asynchronous and teacher-like.

**Human actions**:
1. **Edit artifacts directly** — the diff between the agent's version and the human's edit IS the feedback signal
2. **Write reviews** — standalone text that becomes part of the intellectual corpus
3. **Grade** — classify the edit on a spectrum from "perfect, no changes" to "fundamentally wrong, full rewrite needed"

**Feedback classification** (strawman, needs design):
- `perfect` — human accepted as-is
- `minor` — small corrections (typos, phrasing, precision)
- `substantive` — changed conclusions, added constraints, rejected proposals
- `structural` — reorganized, reframed, changed the question being answered
- `rewrite` — human indicated the entire artifact needs to be redone

**Open design question**: The file/directory structure is contingent on what different model tiers do best. Frontier models (Claude, GPT) may handle different artifact formats than best-in-class open source models (Qwen, Llama). The structure must be optimized for the agent that will consume it, not just the human who grades it.

---

## Progression

```
1. authorial-extraction    (human leads, synchronous, session)
2. infancy-mode            (agent leads, autonomous, session, assumptions externalized)
3. self-parenting-mode     (agent team, autonomous, 24/7, async grading)
4. parent-parenting-mode   (agent team + meta-feedback, autonomous, 24/7, async grading)
5. authorial-proposal-mode (digital twin + agent team, autonomous, 24/7, async grading)
```

Each step: more autonomy, same artifact quality standard. The human's role shifts from author → absent-but-assumption-visible → asynchronous teacher → asynchronous teacher of a system that simulates their presence.

---

## Enforced Constraint: Filesystem Isolation

**Rule**: Autonomous agents are NOT allowed to edit the user's original source code or documents. They can only copy files FROM the user's environment INTO their own containerized environment. The agent can NEVER copy into the user's environment. When a version is ready, the agent notifies the user and provides instructions for the user to pull it themselves.

```
user's environment ──copy──> agent's container (agent works freely)
                              │
                              ▼ (agent produces output)
                              │
                    agent notifies user: "version ready at <path>"
                              │
                    user copies agent's output into their own environment
                    (user archives their own version first if desired)
```

**Properties**:
- Agent can NEVER write to user's environment — not even via swap
- User pulls from agent's container (pull model, like a PR)
- Agent proposes, user accepts — no ambient risk
- User decides their own archival/rollback strategy
- Mirrors display isolation (:99 sandbox / :0 real) applied to filesystem
- Same file paths in both environments — no renaming needed

**Staleness detection still needed**: Agent should report which files it started from (snapshot manifest with hashes). User can diff against their current state before pulling.

---

## Idea Trickle: Async Human Input During Autonomous Operation

**Problem**: Autonomous modes run 24/7. The user will inevitably have new ideas while operations are in progress. The system must absorb new ideas immediately, but decide whether they require a ground-up redesign or are non-breaking changes.

**Communication channel principle**: The human can ramble in any format. The system records verbatim, interprets sensibly, organizes, and appends to Chronicle. Stream of consciousness is a first-class input, not a side channel.

**Input channels** (all feed the same Chronicle topic):
- Telegram/Slack/Discord (via OpenClaw)
- Voice (via Actual Dictation → text)
- Watched directory (drop a .md file)
- Email (via existing mbsync pipeline)
- Direct session input (during collaborative sessions)

**Chronicle topic**: `chronicle.human_input` — raw verbatim input, append-only, immutable. Interpretation/organization is a downstream projection. If the interpretation is wrong, replay the raw input.

**Triage** (rule-based or local LLM):
1. Is this relevant to a running autonomous operation?
   - **Yes, non-breaking**: inject as context, operation continues
   - **Yes, breaking**: flag conflict, present options to agent team (continue current path? restart? explore both in parallel?)
   - **No**: index normally, no interrupt to running operations
2. Extract: intent, confidence, affected domains, urgency

**Breaking vs non-breaking detection**: The triage agent checks whether the new idea contradicts any assumption in the agent team's current Worldview materialization. If it contradicts an L2 (project decision) or L1 (environmental constant) entry, it's breaking. If it's additive or modifies only L3 (narrative), it's non-breaking.

**Open question**: What's the right latency for triage? Should the agent team learn about new ideas within seconds (Kafka consumer) or is a periodic scan (every 15 minutes) sufficient? Probably depends on urgency — most ideas can wait 15 minutes, but "stop, the API key is compromised" cannot.

---

## Open Questions

### Digital Twin
- What does the twin need to simulate? Decision patterns, aesthetic preferences, risk tolerance, interrogation style, correction patterns?
- What corpus trains it? Session transcripts, human edits (diffs), reviews, CLAUDE.md, commit messages, code review comments?
- How is twin drift detected? (Twin diverges from real human over time)
- What's the minimum viable twin?
- Is the twin a Worldview projection of the user?

### Feedback System
- What file/directory structure works best for which model tier?
- How does the system distinguish "human didn't edit because it's perfect" from "human didn't edit because they haven't looked yet"?
- How are reviews structured? Free text? Structured rubric? Both?
- How does grading data flow back into the agent's behavior? Fine-tuning? Prompt engineering? Constitutional updates?

### Quality Measurement
- How does the system know when quality has genuinely improved vs. hallucinating improvement?
- Is there a ground truth? Or is the human's grading the only signal?
- Can the system detect when it's plateauing?

### Infrastructure

**OpenClaw vs Ralph — RESOLVED (2026-02-14, research complete):**
They solve different problems. Complementary, not overlapping.

- **OpenClaw** = always-on 24/7 daemon. Messaging (Telegram, Slack, Discord, +9 more), cron scheduling, Docker sandboxing per session/agent. Community MCP bridges exist but no native MCP client yet.
- **Ralph** = ephemeral task loop (`while :; do cat PROMPT.md | claude-code; done`). Fresh context per iteration, git as memory. NOT 24/7 — runs to completion then stops. No messaging, no scheduling.
- **OpenClaw has a feature request (#6890) to add Ralph loops** as a built-in capability. Natural composition.

**Three-layer architecture for autonomous modes:**
1. **OpenClaw** = communication/scheduling shell (24/7 daemon, messaging channels for idea trickle, Docker sandboxing enforcing "agent can't write to user's environment")
2. **Ralph pattern** = iteration discipline (fresh context per authorial extraction pass, prevents hallucination drift, git-as-memory between iterations)
3. **actual** = knowledge backbone (Chronicle for all events, Temporal for workflow orchestration, Worldview for triage agent)

**Remaining open questions:**
- Git branching strategy for artifact versioning — one branch per cycle? Per day? Per quality improvement?
- How do different model tiers (frontier vs open source) divide labor within the agent team?
- OpenClaw's MCP story is immature (community bridges only) — is this a blocker? Or does actual's gateway serve as the MCP entry point regardless?
- NanoClaw (lightweight fork) enforces mandatory OS-level container isolation — worth evaluating vs stock OpenClaw sandboxing?

### Model Tier Strategy

**Design principle**: Every juncture works with open source (Qwen 2.5 32B / Llama 3.1 8B). Frontier (Claude) is swapped in one juncture at a time, ordered by quality gap.

**Configuration ladder** (worst → best):
- Level 0: All local LLM (everything works, nothing is great)
- Level 1: Parent agent → Claude; rest → local (biggest single improvement)
- Level 2: + Digital twin → Claude
- Level 3: + Infancy mode → Claude
- Level 4: + Child agent → Claude
- Tier 3 junctures (triage, summarization, extraction, ambient) stay on local LLM regardless of budget

**Key insight**: If you can only afford ONE frontier model agent, make it the parent/interrogator. Strong interrogation produces good extraction from any child. Weak interrogation produces mediocre extraction regardless of child quality.

**Architectural requirement**: Every juncture must produce usable results with open source. No hard dependency on frontier capability. Frontier improves quality through the same interface, same format, same grading rubric.

**Ralph's fresh-context pattern benefits open source disproportionately**: Clean context + structured prompt dramatically improves open source model performance vs. the same model at turn 47 of accumulated context. Architecture compensates for model quality.

### Communication / Input
- Which messaging platforms does the user actually want to use for idea trickle?
- Is `chronicle.human_input` the right topic name, or should human input go on existing topics with a special event type?
- How does the triage agent learn what's "breaking" for a given autonomous operation? Does each operation declare its assumptions as a queryable set?
- Voice input: does Actual Dictation need a direct-to-Chronicle pipeline, or is paste-into-Telegram sufficient?
