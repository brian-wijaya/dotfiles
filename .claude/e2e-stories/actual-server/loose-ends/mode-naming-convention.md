# Loose End: Mode Naming Convention

**Date**: 2026-02-14
**Status**: Convention decided, not yet applied to existing skills
**Source**: User decision during actual-server extraction session

---

## Convention

All mode prompts carry exactly one of two suffixes:

- `<concept>-collaborative-mode` — human is in the synchronous loop
- `<concept>-autonomous-mode` — agent operates independently

This is a **naming requirement**, not optional. Every mode must declare its involvement model in the name.

## What Lives in the Name vs. the Description

**In the name**: collaborative or autonomous. One bit. That's all.

**In the description body** (richer data, worth more than a word):
- **Time/resource envelope**: explicit expected duration. Example: "1-10 minutes depending on scope" for POSE collaborative mode.
- **Tool recommendations**: which tools the mode benefits from. This is the author's (agent's) best judgment for optimally fine-tuning outcomes — not a hard requirement, but an opinionated recommendation.
- **Continuity model**: session-bounded vs 24/7 vs on-demand. Documented, not named.
- **Async feedback mechanisms**: for autonomous modes, how the human interacts asynchronously (grading, reviews, edits).

## Existing Skills That Would Be Renamed

### Collaborative modes
| Current | Renamed |
|---|---|
| `authorial-extraction-mode` | `authorial-extraction-collaborative-mode` |
| `pose` | `pose-collaborative-mode` |
| `pedagogical` | `pedagogical-collaborative-mode` |
| `ontological-model` | `ontological-model-collaborative-mode` |
| `case-learning-mode` | `case-learning-collaborative-mode` |
| `systems-case-learning-mode` | `systems-case-learning-collaborative-mode` |

### Autonomous modes (proposed, not yet built)
| Mode | Name |
|---|---|
| Infancy mode | `infancy-autonomous-mode` |
| Self-parenting mode | `self-parenting-autonomous-mode` |
| Parent-parenting mode | `parent-parenting-autonomous-mode` |
| Authorial proposal mode | `authorial-proposal-autonomous-mode` |

### Not modes (procedures, utilities — naming convention does not apply)
- `backup-session-files`, `backup-dotfile`, `backup-vault`, `backup-both-dotfile-and-vault`
- `keyboard-handoff`, `sense-user`, `somatic-aware`
- `reassemble-loose-threads`, `loose-config-search`
- `code-glossary`, `workflow-trace`
- `debug-visual`, `fixed-delay-audit`
- `e2e-loop-demonstration`, `e2e-loop-headless`
- `extract-mode-prompt`, `extract-procedure-prompt`
- `mode-prompt-extraction-mode`, `procedure-prompt-extraction-mode`

### Ambiguous (need classification)
| Skill | Likely classification |
|---|---|
| `mode-prompt-extraction-mode` | Collaborative (requires user input to confirm candidates) |
| `procedure-prompt-extraction-mode` | Collaborative (same) |
| `extract-mode-prompt` | Could be autonomous (single-shot extraction from context) |
| `extract-procedure-prompt` | Could be autonomous (same) |

## Time/Resource Envelopes (strawman)

| Mode | Expected duration | Notes |
|---|---|---|
| `pose-collaborative-mode` | 1-10 minutes | Scope-dependent |
| `authorial-extraction-collaborative-mode` | 30 min - hours | Deep interrogation, multiple decision surfaces |
| `pedagogical-collaborative-mode` | 5-30 minutes | Concept depth dependent |
| `ontological-model-collaborative-mode` | 5-15 minutes | Single concept decomposition |
| `case-learning-collaborative-mode` | 10-30 minutes | Per case study |
| `infancy-autonomous-mode` | 10-60 minutes | Session-bounded, no human input during |
| `self-parenting-autonomous-mode` | 24/7 continuous | Async grading |
| `parent-parenting-autonomous-mode` | 24/7 continuous | Async grading + meta-feedback |
| `authorial-proposal-autonomous-mode` | 24/7 continuous | Async grading + digital twin |
