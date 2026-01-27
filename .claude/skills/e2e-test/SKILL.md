---
name: e2e-test
description: Run end-to-end tests that emulate human input via X11 keystrokes, verify outcomes with somatic differential QA, and produce structured test reports with screenshots. Use when the user wants to validate features, run acceptance tests, or verify a deployment.
argument-hint: [story-file or feature-name]
---

# E2E Test Runner

## Arguments

`$ARGUMENTS` = story file path, feature name, or empty.

## Procedure

### 1. RESOLVE STORIES

- If `$ARGUMENTS` is a file path → load it
- If `$ARGUMENTS` is a name → search `e2e-stories/{name}.yaml` in project root, then `~/.claude/e2e-stories/`
- If empty → list available story files, ask user which to run

### 2. LOAD VOCABULARIES

Parse the story file header. For each entry in `vocabularies:`, load the corresponding YAML from `~/.claude/e2e-vocabularies/{name}.yaml`. Merge inline `vocabulary:` entries on top. All vocabulary names become valid precondition and expect types.

### 3. DISPLAY PLAN

Show story count, names, and precondition summary. Ask user to confirm before running.

### 4. RUN STORIES

For each story, execute:

```
ANNOUNCE
  flash_text(story.name, x=50, y=50, color="#FFFF00", duration_ms=200)

BASELINE
  somatic-temporal now → start_ns
  somatic-fusion get_snapshot
  somatic-x11-bus get_events(100)
  x11_screenshot → "baseline-{story-name}"

PRECONDITIONS
  For each precondition:
    Check condition using the appropriate tool
    If fail → mark BLOCKED:{reason}, skip to next story

EXECUTE
  For each step:
    somatic-temporal now → step_start_ns
    Execute via appropriate tool (x11_key, x11_type, x11_click, wait)
    somatic-temporal delta(step_start_ns) → step_duration

VERIFY
  For each expect:
    Check condition → pass/fail with actual value
  somatic-geometry get_anomalies → diff against baseline
  somatic-x11-bus get_events → diff against baseline
  x11_screenshot → "result-{story-name}"

RESULT
  PASS: all expects passed
  FAIL: any expect failed, log expected vs actual
  BLOCKED: precondition unmet
```

### 5. GENERATE REPORT

Write markdown report to `{project-root}/e2e-reports/e2e-{feature}-{YYYY-MM-DD-HHmm}.md` or `~/e2e-reports/` if no project. Screenshots saved to `{report-dir}/screenshots/{name}.png`.

Report format:

```markdown
# E2E Test Report: {feature}
Date: {timestamp}
Duration: {total}s

## Summary
| # | Story | Result | Duration |
|---|-------|--------|----------|

**Result: X/Y passed, Z failed, W blocked**

## Details
Per story: preconditions, steps, expectations (expected vs actual), somatic diff, screenshot links.
```

### 6. PERSIST

```
vault-rag save_session(
  summary: "E2E: {feature} — {pass}/{total} passed, {fail} failed, {blocked} blocked",
  topics: ["e2e-test", "{feature}"],
  key_facts: ["story:{name}:{PASS|FAIL|BLOCKED}:{reason}" for each story]
)
```

## Story File Format

Story files are YAML. They live in `e2e-stories/` at project root or `~/.claude/e2e-stories/` for global stories.

```yaml
feature: name
description: what this tests
vocabularies: [emacs, browser]  # shared vocabulary imports
vocabulary:                      # inline vocabulary (optional)
  custom_check:
    tool: bash
    cmd: "some command"

stories:
  - name: Human-readable story name
    preconditions:
      - http: { url: "http://localhost:8001/health", expect: 200 }
      - focused: { class: "Emacs" }
    steps:
      - key: "space c m"
      - wait: 3
    expect:
      - screenshot: "result-name"
      - window_title: { match: "pattern" }
```

## Vocabulary System

Three levels of verification primitives:

**Level 1: Built-in** (always available, no declaration needed)

Preconditions:
- `http` — curl GET, fields: url, expect (status code or `fail`)
- `process` — pgrep, fields: name
- `file` — stat, fields: path
- `bash` — run command, fields: cmd, expect
- `window_count` — i3_windows, fields: class, title, min, max
- `focused` — x11_get_active_window, fields: class or title pattern

Steps:
- `key` — x11_key, space-separated tokens sent individually
- `type` — x11_type text string
- `click` — x11_click at x, y
- `wait` — pause N seconds
- `bash` — run shell command (setup, not verification)
- `focus` — focus window by class/title via i3_command

Expects:
- `screenshot` — named screenshot saved to report
- `somatic_clean` — zero geometry anomalies since baseline
- `window_count` — windows matching class/title, min/max
- `window_title` — focused or any window title matches pattern
- `file` — path exists, optionally contains pattern
- `http` — GET url, match status or body
- `bash` — run command, match output or exit code

**Level 2: Inline vocabulary** — declared in story file `vocabulary:` block.

**Level 3: Shared vocabulary files** — YAML files in `~/.claude/e2e-vocabularies/`. Imported via `vocabularies:` in story header.

All vocabulary entries become valid precondition and expect types. They support `match`, `not_match`, and `args` fields. Arguments use `$1`, `$2` positional substitution.

Precondition failure → story marked **BLOCKED** with reason, not FAIL.

## Rules

- ALL user-facing interactions use X11 key/type/click — never application-internal APIs as substitute for user input
- Application-specific state queries (emacsclient, curl, psql, etc.) are valid in preconditions and expects via vocabulary or `bash` type
- Screenshots are mandatory: baseline before and result after every story
- Failed stories log expected vs actual for every failed expectation
- BLOCKED stories are not failures — they indicate missing infrastructure
- Never skip the DISPLAY PLAN step — user must approve before execution
