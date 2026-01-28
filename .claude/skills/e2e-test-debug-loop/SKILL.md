---
name: e2e-test-debug-loop
description: Persistent end-to-end test loop that emulates human input via X11, verifies outcomes with somatic differential QA, retries failures via orchestrator, and produces structured reports with screenshots. Integrates with Python orchestrator for multi-session coordination.
argument-hint: [story-file or feature-name] [--persist]
---

# E2E Test Debug Loop

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

### 4. RUN STORIES (persistence loop)

Run all stories. After each full pass, check results. If any stories are FAIL (not BLOCKED), loop back and re-run only the failing stories. Continue until either all stories are PASS/BLOCKED or a retry produces no improvement (same failures twice in a row). Each retry is a new attempt — reset state, re-baseline, re-execute from scratch.

Track attempt count per story. Reports include all attempts, not just the final one.

Per story per attempt:

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
    x11_screenshot after EVERY step that changes visual state
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

## Visual Capture Discipline

The goal is a complete visual record of every UI state transition. Screenshots are the primary artifact — the report is built from them.

- **Screenshot after every step that changes visual state.** Not just baseline and result — capture each intermediate state. A keystroke that opens a menu, a wait that lets content load, a click that navigates — each gets a screenshot.
- **Catch transients.** If a UI element flashes briefly (loading indicator, notification, animation), attempt to capture it. If missed, re-run the story with tighter screenshot timing around that step. Missing a transient is not a failure — it's a reason to retry.
- **Note peripheral observations.** While executing a story, if something unrelated looks wrong — a broken modeline, a missing icon, a window that shouldn't be there, a font rendering issue — log it in the report under a "Peripheral Observations" section for that story. This is not distraction. Tunnel vision misses real problems. A tester with broad awareness catches issues that unit tests never will.
- **Retry for visual fidelity.** If a screenshot is ambiguous (content still loading, cursor obscuring text, window mid-resize), re-run that step to get a clean capture. The report should be readable by someone who wasn't present.

## Persistence via Ralph Loop

A skill prompt cannot make Claude retry across session boundaries — Claude processes instructions once and exits. Real persistence requires an **external loop** that intercepts the exit and re-injects the task. The ralph-loop plugin provides this.

### How it works

When e2e-test has failures that need retrying, it activates ralph-loop:

1. After the first full pass, if any stories are FAIL (not BLOCKED), write a state file summarizing which stories failed and why
2. Invoke `/ralph-loop` with the e2e-test prompt and completion promise `ALL E2E STORIES PASS OR BLOCKED`
3. Ralph's Stop hook intercepts exit, checks for `<promise>ALL E2E STORIES PASS OR BLOCKED</promise>`
4. If the promise is absent (failures remain), ralph blocks exit and re-injects the prompt
5. Claude wakes in the same session, reads the state file, re-runs only failing stories
6. When all stories are genuinely PASS or BLOCKED, emit the promise to exit the loop

### State file: `.claude/e2e-state.local.md`

Written after each pass. Contains:

```markdown
---
feature: deepwiki
attempt: 2
stories_total: 4
stories_pass: 2
stories_fail: 1
stories_blocked: 1
---

## Failing Stories
- Server down shows helpful error: expected minibuffer match "not reachable", got "Wrong type argument"

## Blocked Stories
- Ask question via RAG: BLOCKED — http localhost:8001 refused

## Peripheral Observations
- Modeline shows wrong branch name in deepwiki buffer
```

### When NOT to use ralph-loop

- If the user only wants a single pass (default without `--persist` flag)
- If all failures are BLOCKED (infrastructure missing, not code bugs)
- If the same failures reproduce identically across 3 attempts (genuine bugs, not flakiness)

### Activation

The skill checks user intent:
- `/e2e-test deepwiki` — single pass, report results
- `/e2e-test deepwiki --persist` — ralph-loop until 100% or stagnation
- If failures exist after single pass, ask user: "N stories failed. Run persistent retry loop?"

### Stagnation detection

Between attempts, compare the current failure set to the previous one. If identical failures with identical actual values appear twice in a row, the failures are genuine — emit the completion promise and stop. Do not burn iterations on deterministic bugs.

The report records all attempts, not just the final one. Each attempt's screenshots, expects, and peripheral observations are preserved.
