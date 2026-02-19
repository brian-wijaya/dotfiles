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

4. Check open GitHub issues across all repos:
   ```bash
   gh search issues --owner SignalAssembly --state open --limit 30
   ```

5. **Audit each issue against current reality.** Do NOT assume open issues are still valid. For each issue:
   - Read the issue description to understand the claimed problem
   - Run a concrete check to verify whether the problem still exists (query a DB, check a config, test a tool, grep for the code pattern, check process state, etc.)
   - Classify as: STILL VALID, STALE (should be closed), or PARTIALLY RESOLVED
   - For STALE issues: close them immediately with `gh issue close <number> --comment "Verified resolved: <evidence>"`
   - Only report issues that are STILL VALID or PARTIALLY RESOLVED

6. Cross-reference surviving issues with session threads — issues that match active threads should be noted inline.

7. Check `cd ~/dotfiles && git status --short` for uncommitted backup changes.

8. Check `cd ~/dotfiles && git log --oneline -10` for recent work context.

9. Synthesize findings into a structured report:

```
### Loose Threads

**1. [Thread name]** (date/timeframe)
- What was done
- What remains
- Any blockers or broken state
- Related issues: #N, #M

**2. [Thread name]** ...

### Open GitHub Issues (verified still valid)
| # | Title | Repo | Label | Verdict |
|---|-------|------|-------|---------|
(sorted by severity: critical/bug first, then enhancement/cleanup)
(only issues verified as STILL VALID or PARTIALLY RESOLVED)

### Closed This Run
| # | Title | Reason |
|---|-------|--------|
(issues verified as resolved and closed during this skill execution)

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
- NEVER report an issue as open without first verifying it still reproduces or the code still has the defect
- Close stale issues during the skill run — don't leave cleanup for the user
