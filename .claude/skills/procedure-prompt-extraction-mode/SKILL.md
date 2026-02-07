---
name: procedure-prompt-extraction-mode
description: Investigate session context to identify bounded, single-shot operations worth formalizing as reusable procedure prompts. Proposes candidates with initiation/termination conditions, contrasts extraction strategies, and drafts procedure skill files on confirmation. Use when the user wants to discover repeatable actions in their workflow that should become standalone procedures.
argument-hint: [domain or workflow to examine]
---

Operate in PROCEDURE PROMPT EXTRACTION MODE. This mode exists to investigate the current session context and identify opportunities where one or more bounded, single-shot procedures should be formalized as reusable procedure prompts.

A procedure is defined here as a finite operation with a clear start, ordered internal steps, a concrete output, and an explicit termination point. This mode is responsible for identifying such operations, not for executing them.

At the start of each response, explicitly state the assumptions you are making about the current context, including inferred domains, recurring tasks, repeated analytical moves, and gaps in the existing procedure prompt collection. These assumptions must be explicit and owned.

Based on these assumptions, propose one or more candidate procedures that warrant extraction. For each candidate, describe the action it captures, why it qualifies as a procedure rather than a mode or inline prompt, and what its initiation condition and termination condition would be.

Where multiple extraction strategies are possible, present them as contrasts and explain the tradeoffs between narrower versus broader procedural scope. Actively recommend a preferred extraction path based on structural clarity and reuse value.

Pose targeted clarification questions only when the answers would materially affect which procedures should be extracted or how their boundaries should be defined.

Once clarification is obtained, invoke the appropriate extraction procedure to draft the selected procedure prompts, formatted for direct storage as Claude skills at /home/bw/.claude/skills/<skill-name>/SKILL.md using the standard frontmatter format (name, description, argument-hint).

This mode remains active and iterative until explicitly terminated.
