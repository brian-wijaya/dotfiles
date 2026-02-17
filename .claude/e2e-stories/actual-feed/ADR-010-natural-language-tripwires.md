# ADR-010: Natural Language Tripwires via LLM Evaluation

## Status
Accepted

## Context
Users want event-driven alerts ("tell me when X happens") separate from the ranked feed. The tripwire system must be expressive enough for nuanced conditions ("Any time a FAANG company open-sources a database") while keeping compute bounded.

Candidates considered:
- **Structured rule builder (if/and/or)**: Precise and deterministic. But: inaccessible for nuanced conditions. Cannot express implicit relationships. Rejected as primary mechanism (may be added as V2 alternative).
- **Keyword matching**: Cheap but brittle. "Oracle acquires framework" misses "Oracle purchases" or "Oracle takes over." Rejected.
- **Natural language conditions evaluated by LLM**: Strictly more expressive than rules. The LLM interprets nuance. Cost is managed by folding evaluation into the existing Stage 2 extraction call.

## Decision
Tripwires are natural language conditions evaluated by the local LLM. Evaluation is folded into the Stage 2 extraction prompt — no separate LLM call per tripwire. Active tripwire conditions are appended to the extraction prompt (~150 tokens per tripwire). Firing threshold: confidence >= 0.7 (user-adjustable). Cooldown prevents re-fire on semantically similar items.

Keyword pre-filter reduces average tripwires evaluated per item from N to ~5 by matching tripwire keywords against item title + entities before including in the LLM prompt. Critical-severity tripwires bypass the pre-filter and are always evaluated.

Cloud tripwires evaluate continuously (premium). Local tripwires evaluate only on crawl (honest gap communicated). Shipped tripwire packs for common domains; community sharing is V2.

## Consequences
- **Positive**: Highly expressive. Nuanced conditions handled by LLM interpretation. No additional inference calls. Compute bounded by keyword pre-filter.
- **Negative**: Interpretation ambiguity — different models may interpret conditions differently. Model swap may change tripwire firing behavior. 0.7 confidence threshold trades false negatives for no false positives. Practical limit: 30 tripwires before compute overhead warning. At ~150 tokens per tripwire, 30 tripwires add ~4,500 tokens to the extraction prompt — roughly 15% of Qwen3-30B-A3B's 32K context window. Beyond 30, the system surfaces a warning: "N tripwires active. Extraction may slow. Consider consolidating." This is a soft warning, not a hard cap.

## Constraints Implemented
- EC-050: Tripwire evaluation folded into Stage 2 extraction, not a separate call
- EC-051: Tripwire fires require confidence >= 0.7 (user-adjustable)
- EC-052: Cloud vs local tripwire gap is honestly communicated
- EC-053: Tripwire evaluation does not affect item scoring
- OC-050: Tripwires are natural language conditions, not structured rules
- OC-051: Tripwire packs are shipped, not crowdsourced (V1)
- OC-052: Critical tripwires can override Do Not Disturb (opt-in)
- OC-053: Pre-filter tripwires by keyword overlap before LLM evaluation
