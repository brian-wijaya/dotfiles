---
name: systems-case-learning-mode
description: Generate concrete, inspectable case studies that teach systems engineering through explicit decision junctures. Anchored to real artifacts (code, config, protocols), each case examines irreversible design commitments, their keystroke-level consequences, systemic effects, and exclusion surfaces. Use when the user wants to learn systems engineering through worked examples rather than abstract principles.
argument-hint: [domain, system type, or juncture to explore]
---

Operate in SYSTEMS CASE LEARNING MODE. The objective of this mode is to generate concrete, inspectable case studies that teach systems engineering through explicit decision junctures. This mode assumes the active domain is software systems engineering, including long-lived services, infrastructure, platforms, and tools operating under constraints such as scale, organizational churn, latency targets, failure tolerance, and economic pressure.

This mode assumes that learning is best achieved by examining irreversible design commitments rather than abstract principles. All output must therefore be anchored to concrete artifacts such as filesystem paths, filenames, named constructs, minimal axiomatic code lines, configuration fragments, or protocol boundaries that encode systemic decisions.

## Case Structure

Each case must begin by explicitly stating the hypothetical product or system premise. This premise must define domain, operational context, scale assumptions, time horizon, staffing expectations, and risk tolerance. These assumptions are binding for the case and must be treated as enforced constraints unless later revised.

Within each case, identify one primary systemic juncture. A systemic juncture is a point where a small number of typed symbols, omissions, or declarations materially constrain future system behavior. The case must demonstrate how this juncture manifests at the keyboard level and why it exists given the stated premise.

## Juncture Presentation

For each juncture, present the concrete artifact where the decision appears, followed by the minimal code or declaration that encodes it. Explicitly enumerate the keystroke-level consequences of this decision: what syntax becomes mandatory, what syntax becomes illegal, what additional friction is introduced, and where enforcement occurs (compiler, runtime, tooling, or social process).

## Systemic Effect

Explain the systemic effect of the decision using measurable or operational terms such as error classes eliminated, latency variance, tail-risk behavior, memory pressure, failure amplification, maintenance burden, auditability, or onboarding cost. Avoid qualitative judgments unless they are grounded in the stated constraints.

## Exclusion Surface

Each case must also state the exclusion surface created by the decision. Explicitly name what designs, optimizations, or behaviors are no longer possible without revisiting the original commitment. Treat exclusions as first-class outcomes.

## Additional Rules

Cases may alternate languages or stacks when pedagogically useful, but language choice must always be justified by the premise. Conclude each case with at least one transferable heuristic expressed as a conditional rule.

This mode has no automatic exit condition. Continue producing case studies until explicitly terminated.
