---
name: e2e-test-debug-loop
description: Run end-to-end tests that emulate human input via X11 keystrokes, verify outcomes with somatic differential QA, retry failures via orchestrator, and produce structured test reports with screenshots. Use when the user wants to validate features, run acceptance tests, or verify a deployment.
argument-hint: [story-file or feature-name]
---

# E2E Test Debug Loop

Persistent end-to-end testing with human-emulated X11 input, somatic differential QA, orchestrator-driven retries, and structured reporting.

## Arguments

`$ARGUMENTS` = story file path, feature name, or empty

## Story File Locations

- Project: `{project-root}/e2e-stories/{name}.yaml`
- Global: `~/.claude/e2e-stories/{name}.yaml`

## Vocabulary Locations

- Shared: `~/.claude/e2e-vocabularies/{name}.yaml`

## Procedure

### 1. RESOLVE STORIES

```
IF $ARGUMENTS is a file path:
  Load story file directly
ELIF $ARGUMENTS is a name:
  Search e2e-stories/{name}.yaml in project root
  Then ~/.claude/e2e-stories/{name}.yaml
ELSE:
  List available story files
  Ask user which to run
```

### 2. LOAD VOCABULARIES

```
Load built-in vocabulary (always available)
Load shared vocabularies from ~/.claude/e2e-vocabularies/ as declared in story
Load inline vocabulary from story file header
```

### 3. DISPLAY PLAN

Show to user:
- Story count and names
- Precondition summary for each story
- Estimated scope

**WAIT for user confirmation before proceeding.**

### 4. RUN STORIES

For each story, execute:

```
ANNOUNCE
  somatic-hud flash_text(story.name, x=50, y=50, color="#FFFF00", duration_ms=200)

BASELINE
  somatic-temporal now → start_ns
  somatic-fusion get_snapshot
  somatic-x11-bus get_events(100)
  x11_screenshot → "baseline-{story-name}"

PRECONDITIONS
  For each precondition:
    Check condition using appropriate tool
    If fail → mark BLOCKED:{reason}, skip to next story

EXECUTE
  For each step:
    somatic-temporal now → step_start_ns
    Execute via X11 tools (x11_key, x11_type, x11_click)
    somatic-temporal delta(step_start_ns) → step_duration

VERIFY
  For each expect:
    Check condition → pass/fail with actual value
  somatic-geometry get_anomalies → diff against baseline
  somatic-x11-bus get_events → diff against baseline
  x11_screenshot → "result-{story-name}"

RESULT
  PASS: all expects passed → flash green
  FAIL: any expect failed → flash red, log expected vs actual
  BLOCKED: precondition unmet → flash yellow
```

### 5. RETRY LOOP (Orchestrator Integration)

```
IF any story FAILED:
  Analyze failure:
    - Genuine bug? → Log for code fix, continue
    - Flaky test? → Retry up to 3 times
    - Infrastructure missing? → Mark BLOCKED

  Track failure hashes for stagnation detection
  IF 3 identical failures → mark STAGNATED, stop retrying
```

### 6. GENERATE REPORT

Write to `{project-root}/e2e-reports/e2e-{feature}-{YYYY-MM-DD-HHmm}.md`
Or `~/e2e-reports/` if no project root.

Screenshots saved to `{report-dir}/screenshots/{name}.png`

Report format:
```markdown
# E2E Test Report: {feature}
Date: {timestamp}
Duration: {total}s

## Summary
| # | Story | Result | Duration |
|---|-------|--------|----------|
| 1 | Story name | ✅ PASS | 3.2s |
| 2 | Story name | ❌ FAIL | 2.1s |
| 3 | Story name | ⏸ BLOCKED | — |

**Result: X/Y passed, Z failed, W blocked**

## Details

### 1. Story name — ✅ PASS
**Preconditions**: condition → status
**Steps**: step sequence
**Expectations**: each expect with actual value
**Somatic**: anomaly count, event diff
**Screenshots**: [baseline](path) | [result](path)
```

### 7. PERSIST TO VAULT

```
vault-rag save_session(
  summary: "E2E: {feature} — {pass}/{total} passed, {fail} failed, {blocked} blocked",
  topics: ["e2e-test", "{feature}"],
  key_facts: ["story:{name}:{PASS|FAIL|BLOCKED}:{reason}" for each story]
)
```

## Rules

1. **X11 Input Only**: ALL user-facing interactions use x11_key, x11_type, x11_click. Never use application-internal APIs as substitute for user input.

2. **State Queries Allowed**: Application-specific state queries (emacsclient -e, curl, psql, etc.) are valid in preconditions and expects via vocabulary.

3. **Screenshots Mandatory**: Capture baseline before and result after every story.

4. **Expected vs Actual**: Failed stories log expected vs actual for every failed expectation.

5. **BLOCKED ≠ FAIL**: Blocked stories indicate missing infrastructure, not test failures.

6. **User Approval Required**: Never skip the DISPLAY PLAN step.

7. **Stagnation Detection**: After 3 identical failures, stop retrying and mark STAGNATED.

## Built-in Vocabulary

### Preconditions

| Type | Fields | Behavior |
|------|--------|----------|
| `http` | url, expect (status or `fail`) | curl GET, match status |
| `process` | name | pgrep |
| `file` | path | stat |
| `bash` | cmd, expect | Run command, match output |
| `window_count` | class, title, min/max | i3_windows query |
| `focused` | class or title | x11_get_active_window |

### Steps

| Type | Behavior |
|------|----------|
| `key` | x11_key, space-separated tokens |
| `type` | x11_type text string |
| `click` | x11_click at x, y |
| `wait` | Pause N seconds |
| `bash` | Run shell command (setup only) |
| `focus` | Focus window via i3_command |

### Expects

| Type | Behavior |
|------|----------|
| `screenshot` | Named screenshot saved to report |
| `somatic_clean` | Zero geometry anomalies since baseline |
| `window_count` | Windows matching class/title, min/max |
| `window_title` | Window title matches pattern |
| `file` | Path exists, optionally contains pattern |
| `http` | GET url, match status or body |
| `bash` | Run command, match output |

## Example Story File

```yaml
feature: example-feature
description: Feature description
vocabularies: [emacs, browser]  # Load shared vocabularies

vocabulary:  # Inline vocabulary
  app_status:
    tool: bash
    cmd: "curl -s localhost:8080/status"

stories:
  - name: Basic functionality
    preconditions:
      - http: { url: "http://localhost:8080/health", expect: 200 }
      - focused: { class: "Emacs" }
    steps:
      - key: "space c m"
      - wait: 2
    expect:
      - screenshot: "basic-result"
      - app_status: { match: "running" }
      - somatic_clean: true
```
