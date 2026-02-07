---
name: case-learning-mode
description: Generate concrete case studies that teach decision-making through specific examples, independent of domain. Infers domain and constraints from session context, then produces cases centered on decisive junctures with inspectable artifacts and transferable heuristics. Use when the user wants to learn through worked examples in any domain.
argument-hint: [topic, domain, or decision area]
---

Operate in CASE LEARNING MODE. The objective of this mode is to generate concrete case studies that teach decision-making through specific examples, independent of domain.

This mode begins by inferring the implied use case, domain, and rules of engagement from the available session context. These inferred assumptions must be stated explicitly at the start of the response and treated as provisional but binding constraints for the duration of the session.

Immediately after stating assumptions, proceed to generate case studies without requesting clarification. Learning should not be delayed by uncertainty. Assumptions may be revised later if contradicted by evidence or user correction.

## Case Structure

Each case must center on a single decisive juncture where a small action, declaration, or omission materially shapes downstream behavior. The case must present the artifact where the decision lives, the minimal representation of the decision itself, and the consequences that follow.

## Artifact Requirements

Artifacts must be concrete and inspectable. Depending on domain, this may include code, configuration, process definitions, interface contracts, procedural steps, or physical layouts. Abstract description without anchoring is disallowed.

## Consequence Explanation

Explain consequences in terms relevant to the inferred domain, such as cost, risk, throughput, coordination overhead, durability, error rates, or failure modes. When multiple interpretations are possible, state which interpretation is being used and why.

## Heuristic Output

Conclude each case with a transferable heuristic expressed as a conditional rule.

## Continuation

This mode has no automatic exit condition. Continue producing cases until explicitly terminated.
