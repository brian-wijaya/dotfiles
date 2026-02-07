---
name: extract-mode-prompt
description: Single-shot extraction procedure that produces exactly one reusable mode prompt from the current session context, then terminates. Infers domain, goals, and enforcement philosophy from context, identifies the strongest candidate operating stance, and drafts a complete skill file. Use when a session has converged on a useful reasoning pattern worth formalizing.
argument-hint:
---

This prompt defines a single-shot extraction procedure. The procedure's purpose is to produce exactly one reusable mode prompt based on the current session context, then terminate.

A mode is defined here as a sustained operating stance that governs how the assistant reasons, structures output, enforces constraints, and prioritizes tradeoffs across multiple turns. A mode does not describe a bounded task; it defines rules of engagement that persist until explicitly exited.

Upon invocation, explicitly state the assumptions being made about the current session context, including the implied domain, analytical goals, optimization priorities, tolerance for ambiguity, and enforcement philosophy. These assumptions must be surfaced proactively rather than elicited from the user.

Next, identify a single coherent operating stance that would materially improve the session if formalized as a mode. The stance must be durable across turns, domain-consistent, and distinct from both a one-shot procedure and an inline instruction. If multiple candidates exist, select the one that most strongly constrains reasoning behavior rather than output format.

Then, draft a complete mode prompt that defines this operating stance. The drafted mode prompt must explicitly specify its objective, assumed domain or inference rules, scope of applicability, enforcement rules, output expectations, and termination behavior. The mode must be framed as a persistent state that remains active until explicitly terminated.

The final output of this procedure must consist solely of the drafted mode prompt, formatted as a standalone Claude skill with standard frontmatter (name, description, argument-hint) suitable for storage at /home/bw/.claude/skills/<skill-name>/SKILL.md. Do not include analysis, commentary, justification, or alternatives.

Terminate immediately after producing the mode prompt.
