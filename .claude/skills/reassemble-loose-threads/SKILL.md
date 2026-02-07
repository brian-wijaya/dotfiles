---
name: reassemble-loose-threads
description: Reassemble loose threads from recent sessions to find unfinished work. Use when the user says "loose threads", "where were we", "what's pending", "what was I working on", or wants to find unfinished work across sessions.
---

Reassemble loose threads from recent sessions and present them as a structured list.

## Procedure

1. Call `vault-rag reassemble_loose_ends(lookback_days=10)` to get structured work items.
   - If it errors (e.g. UTF-8 decode), fall back to steps 2-3.

2. Call `vault-rag get_recent_sessions(limit=15)` to get recent session summaries.

3. Call `vault-rag search_sessions` with multiple topic queries in parallel to find substantive sessions:
   - Use `recency_bias=0.7` for general resumption queries
   - Use `recency_bias=0.3` for specific topic searches
   - Try queries like: "phase TODO next implementation", "unfinished pending blocked", and topic-specific queries based on session summaries
   - IMPORTANT: FTS5 treats hyphens as NOT operator. Avoid hyphenated terms (use "vault rag" not "vault-rag", use "e2e" not "e-2-e").

4. Check `cd ~/dotfiles && git status --short` for uncommitted backup changes.

5. Check `cd ~/dotfiles && git log --oneline -10` for recent work context.

6. Synthesize findings into a structured report:

```
### Loose Threads

**1. [Thread name]** (date/timeframe)
- What was done
- What remains
- Any blockers or broken state

**2. [Thread name]** ...

### Uncommitted Changes
- List any pending dotfile backups

### Broken Infrastructure
- Note any tools/services that are currently erroring
```

## Important

- Present threads in priority order (most actionable first)
- Flag broken tooling that blocks future work
- Be specific about what "next step" means for each thread
- Don't pad with resolved items — only show genuinely open threads
