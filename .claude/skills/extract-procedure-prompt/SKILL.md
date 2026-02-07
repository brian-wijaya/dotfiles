---
name: extract-procedure-prompt
description: Single-shot extraction procedure that produces exactly one reusable procedure prompt from the current session context, then terminates. Infers repeated actions, inputs, outputs, and success criteria from context, identifies the most structurally central bounded operation, and drafts a complete skill file. Use when a session contains a repeatable action worth formalizing as a one-shot procedure.
argument-hint:
---

This prompt defines a single-shot extraction procedure. The procedure's purpose is to produce exactly one reusable procedure prompt based on the current session context, then terminate.

A procedure is defined here as a bounded sequence of analytical steps with an explicit initiation condition, ordered execution, a concrete output artifact, and a deterministic termination point. This procedure must not maintain an ongoing interpretive stance and must not persist beyond completion of its output.

Upon invocation, explicitly state the assumptions being made about the current session context, including the implied domain, repeated actions observed, expected inputs, expected outputs, and success criteria. These assumptions must be surfaced proactively rather than elicited from the user.

Next, identify a single repeatable action that can be expressed as a predicate-shaped function. The action must be narrow enough to execute in one invocation and must have a clear completion condition. If multiple candidates exist, select the most structurally central one rather than presenting alternatives.

Then, draft a complete procedure prompt that defines this action. The drafted procedure prompt must explicitly specify its triggering condition, required inputs, ordered steps, expected outputs, and termination behavior. The procedure must be framed as an operation that executes once and ends.

The final output of this procedure must consist solely of the drafted procedure prompt, formatted as a standalone Claude skill with standard frontmatter (name, description, argument-hint) suitable for storage at /home/bw/.claude/skills/<skill-name>/SKILL.md. Do not include analysis, commentary, justification, or alternatives.

Terminate immediately after producing the procedure prompt.
