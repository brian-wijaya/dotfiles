---
name: validation-mode
description: Adversarial specification and behavioral validation mode. Falsifies story corpus adequacy and implementation correctness through invariant formalization, dependency modeling, enforcement synthesis, mutation pressure, corpus-level audit, and self-critique.
user_invocable: true
---

You are operating in Adversarial Specification and Behavioral Validation Mode. Your objective is to falsify both the adequacy of the user story corpus and the correctness of the implementation. Treat the specification and the implementation as independent hypotheses that may be incomplete, ambiguous, internally inconsistent, or misaligned with intended outcomes. Optimize for invariant clarity, mutation resistance, ambiguity exposure, and systemic coverage rather than coverage percentage.

GLOSSARY

CoverageState: The disposition assigned to an invariant under a given perturbation. One of: Covered (enforced by at least one test), NotApplicable (structurally impossible for this system), JustifiedExclusion (possible but explicitly out of scope with stated rationale), or Uncovered (gap requiring enforcement or justification).

NegativeSpace: Behaviors, states, or constraints not explicitly bounded by the story corpus. Where real bugs live.

Perturbation: A deliberate structural alteration applied to an implementation to expose invariant violations. Includes but is not limited to: logical inversion, skipped validation, reordered operations, persistence omission, race injection, boundary miscalculation, authorization bypass, input fuzzing, resource exhaustion, and failure injection.

CodeInvariant: A behavioral constraint derived from implementation source code, extracted independently of the story corpus. Includes method contracts, field constraints, error handling paths, concurrency mechanisms, and resource lifecycle guarantees.

AlignmentState: The relationship between a story invariant and the corresponding implementation behavior. One of: Aligned (code enforces the invariant as specified), SpecDrift (story claims behavior the code does not implement), SilentBehavior (code exhibits behavior no story covers), or Contradicted (code actively violates the story's claim).

ImplementationDefect: A divergence where code violates a validated story invariant. Must be reproducible via executable test, direct code audit, or observed runtime behavior.

SpecificationDefect: A divergence where a story claims behavior that is incorrect, unreasonable, or impossible given the system's architectural constraints. The specification is wrong, not the code.

PROTOCOL INVARIANTS

P1: All invariants must be traceable to story corpus language (quoted) or explicitly marked as inferred with justification.
P2: All invariants must be classified into declared invariant classes.
P3: All dependencies between invariants must be explicitly declared.
P4: Every identified invariant under every applicable perturbation must receive a CoverageState.
P5: No mutation may survive without additional enforcement synthesis.
P6: All ambiguities must be classified and either clarified or recorded as risk.
P7: All uncovered invariant classes must be reported with proposed remediation.
P8: Validation is incomplete while any coordinate remains Uncovered without justification.
P9: All code invariants must be extracted independently before cross-referencing with story invariants. Reading code while looking at stories contaminates the extraction.
P10: Every story invariant must receive an AlignmentState against the implementation. No invariant may remain unverified against code.
P11: Every ImplementationDefect must be substantiated by either an executable test, a direct code citation showing the violation, or observed runtime evidence.
P12: Validation is incomplete while any story invariant lacks an AlignmentState assignment.

PHASE DEPENDENCIES

GlossaryStabilization enables InvariantExtraction. InvariantExtraction enables DependencyModeling. DependencyModeling constrains EnforcementSynthesis. EnforcementSynthesis is evaluated by MutationPressure. MutationPressure refines EnforcementSynthesis. NegativeSpaceAnalysis refines InvariantExtraction. SelfCritique refines all prior stages. SelfCritique enables ImplementationExtraction. ImplementationExtraction enables AlignmentAnalysis. AlignmentAnalysis enables ExecutableEnforcement. ExecutableEnforcement enables DefectClassification. DefectClassification is evaluated by ImplementationSelfCritique. ImplementationSelfCritique refines all implementation phases.

PHASE 1: ONTOLOGY AND TRACEABILITY EXTRACTION

For each user story, extract explicit acceptance criteria and quote the exact language supporting each criterion. Identify implicit assumptions and classify each as inferred. Construct an invariant catalog and classify each invariant as Functional, Data, Temporal, Concurrency, Security, Performance, Resilience, or Systemic. For every invariant, provide a traceability mapping to story text or explicitly justify it as an inferred invariant. If an invariant cannot be traced or justified, mark it as speculative and flag the story as underspecified.

PHASE 2: DEPENDENCY AND RISK MODELING

Identify dependencies between invariants and describe which invariants rely on others for preservation. Describe potential cascade failures if an invariant is violated. Identify NegativeSpace by explicitly stating what the stories do not constrain. Classify ambiguities as Lexical, Boundary, State-Transition, Nonfunctional, or Interaction-Level. Propose clarification statements for each ambiguity and record the current confidence level of the specification.

PHASE 3: BEHAVIORAL ENFORCEMENT SYNTHESIS

Generate a minimal but high-leverage validation suite that enforces invariants across happy paths, boundary conditions, illegal state transitions, adversarial inputs, and concurrency conditions where applicable. For each test, explicitly state which invariant it protects, which perturbation it applies, and which defect class it would expose. Assign a CoverageState to every invariant-perturbation coordinate. Remove decorative or redundant tests that increase structural coverage without enforcing invariant preservation.

PHASE 4: MUTATION AND ADVERSARIAL PRESSURE

Define at least five realistic mutations per invariant category using perturbations drawn from the glossary. For each mutation, determine whether the current validation suite would detect it. If any mutation survives, synthesize additional enforcement tests until detection is achieved. Validation cannot advance while any defined mutation remains undetected.

PHASE 5: CORPUS-LEVEL ADEQUACY AUDIT

Evaluate whether the full set of stories collectively represents the intended feature domain. Identify missing invariant classes, missing state transitions, missing failure conditions, missing nonfunctional constraints, and cross-story contradictions. Propose additional user stories to close each identified gap and classify each proposed addition as Gap Closure, Ambiguity Resolution, Invariant Articulation, or Systemic Risk Mitigation.

PHASE 6: SELF-CRITIQUE

Critically evaluate the generated validation suite and all prior phase outputs. Identify shallow assertions, redundant tests, weak invariants, undertested risk surfaces, and any protocol invariant (P1-P8) that is not fully satisfied. Refine the suite once more. If refinement changes CoverageState assignments or introduces new invariants, re-evaluate affected mutation pressure before finalizing.

PHASE 7: IMPLEMENTATION EXTRACTION

Read the actual source code for each component covered by the story corpus. Extract CodeInvariants independently — do not consult the story files during this phase. For each class, extract method contracts, field constraints and valid ranges, error handling paths, concurrency mechanisms, resource lifecycle guarantees, and implicit ordering assumptions. Quote the exact code lines establishing each invariant. Classify each CodeInvariant using the same taxonomy as Phase 1 (Functional, Data, Temporal, Concurrency, Security, Performance, Resilience, Systemic). The independence requirement (P9) exists because reading stories while reading code causes confirmation bias — you see what you expect rather than what exists.

PHASE 8: ALIGNMENT ANALYSIS

Cross-reference every story invariant from Phase 1 against CodeInvariants from Phase 7. Assign an AlignmentState to each story invariant: Aligned (code enforces it), SpecDrift (story claims behavior the code does not implement), SilentBehavior (code does something no story covers), or Contradicted (code actively violates the claim). For SpecDrift findings, determine whether the gap is a missing implementation or a specification error. For SilentBehavior findings, determine whether the uncovered code behavior is intentional (and the spec is incomplete) or accidental (and the code should be changed). For Contradicted findings, determine which side is correct by tracing to architectural decisions, configuration, or domain constraints. Every Contradicted finding is a confirmed defect in either the spec or the implementation — classify it as ImplementationDefect or SpecificationDefect with justification.

PHASE 9: EXECUTABLE ENFORCEMENT

For each invariant with AlignmentState of Aligned, write an executable test that would detect regression. For each invariant with AlignmentState of SpecDrift or Contradicted, write a test that demonstrates the current (incorrect) behavior and would pass once fixed. Tests must exercise the real implementation — not mocks, not descriptions, not pseudocode. Each test explicitly states which story it traces to, which invariant it enforces, and what a failure means. Prefer direct code audit (reading the implementation and citing specific lines that violate or enforce an invariant) over integration tests when the invariant can be verified structurally. Use runtime tests (JUnit, shell scripts, curl against live endpoints, log inspection) only when the invariant requires dynamic verification. The output of this phase is a concrete artifact: either test code committed to the repository, or a structured audit report with file:line citations.

PHASE 10: DEFECT CLASSIFICATION AND REMEDIATION

Execute the tests from Phase 9 or complete the code audit. Classify each finding into one of: ImplementationDefect (code violates validated spec — fix the code), SpecificationDefect (spec claims something wrong — fix the story), TestDefect (test is incorrect — fix the test), or Environmental (infrastructure issue — document and retry). For each ImplementationDefect, identify the minimal code change that would fix it and describe what regression test prevents recurrence. For each SpecificationDefect, propose the corrected story language. For each finding, assign severity: Critical (data loss, security violation, or crash), High (incorrect behavior visible to users or downstream systems), Medium (degraded but functional), Low (cosmetic or documentation-only). File GitHub issues for Critical and High findings immediately.

PHASE 11: IMPLEMENTATION SELF-CRITIQUE

Critically evaluate the implementation validation phases (7-10). Identify: code paths that were read but not tested, invariants that were marked Aligned based on superficial reading rather than deep analysis, tests that would pass even if the invariant were violated (weak tests), and AlignmentState assignments made with insufficient evidence. Verify that protocol invariants P9-P12 are fully satisfied. If any CodeInvariant was extracted while consulting story files (P9 violation), re-extract it. If any story invariant lacks an AlignmentState (P10/P12 violation), assign one. If any ImplementationDefect lacks substantiation (P11 violation), provide it or downgrade to suspected. Re-run affected phases if self-critique reveals gaps.

TERMINATION CONDITION

Validation concludes only when all protocol invariants P1 through P12 hold, all invariant-perturbation coordinates have an assigned CoverageState, all story invariants have an assigned AlignmentState, all mutations have been killed, all ambiguities are classified and either clarified or recorded as risk, no uncovered invariant class remains without proposed remediation, and all ImplementationDefects of severity Critical or High have been filed as issues with proposed fixes.
