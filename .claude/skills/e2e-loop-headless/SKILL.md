---
name: e2e-loop-headless
description: Run end-to-end tests in an isolated Xvfb display via display isolation. No interference with user's desktop. Optionally attach an xpra viewer to spectate. Use for CI, batch testing, unattended runs, or when you don't want tests touching your screen.
argument-hint: [story-file or feature-name] [--spectate]
---

# E2E Loop — Headless

Isolated end-to-end testing in a dedicated Xvfb display. Creates a headless X11 environment, runs stories with full sensor instrumentation, and tears it down when done. The user's desktop is never touched.

**Display isolation is the default.** With `display.enabled = true` in `~/.config/kinetic/kinetic.toml`, kinetic auto-creates display :99 on startup and auto-attaches an xpra viewer window (tiled on a separate workspace, read-only). All display-routed tools (X11 input, screenshots, sensor sensors) automatically target :99. The user can observe the agent's display via the xpra viewer. The polybar agent module (robot icon) shows activity state: cyan=idle, orange=active, green=done.

## Arguments

`$ARGUMENTS` = story file path, feature name, or empty. Append `--spectate` to auto-attach an xpra viewer window.

## Story File Locations

- Project: `{project-root}/e2e-stories/{name}.yaml`
- Global: `~/.claude/e2e-stories/{name}.yaml`

## Vocabulary Locations

- Shared: `~/.claude/e2e-vocabularies/{name}.yaml`

## Procedure

### 1. RESOLVE STORIES

```
IF $ARGUMENTS contains a file path:
  Load story file directly
ELIF $ARGUMENTS contains a name (not --spectate):
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
- Display isolation: will create Xvfb display
- Spectate mode: yes/no (whether xpra viewer will open)

**WAIT for user confirmation before proceeding.**

### 4. CREATE ISOLATED DISPLAY

With `display.enabled = true` (the default in `~/.config/kinetic/kinetic.toml`), kinetic has already created display :99 on startup and auto-attached an xpra viewer. In this case, **skip display creation** — the display already exists and all tools are already routing to :99.

If display isolation was not enabled at startup, create one manually:

```
ACT_create_display(width=1920, height=1080, depth=24)
  → Returns display_id (e.g., "d99")

Store display_id for all subsequent tool calls.

This starts:
  - Xvfb on a free display number
  - i3 window manager inside the Xvfb
  - Per-display sensor instance (via SENSOR_SHM_SUFFIX)
```

All subsequent X11 tool calls MUST pass `display_id` parameter to target the isolated display, not the user's real display.

### 4.1. SPECTATE MODE (if --spectate or user requested)

When display isolation is enabled at startup, the xpra viewer is already attached automatically — no need to call `ACT_attach_viewer`. The user sees the agent's display :99 in a read-only xpra window tiled on a separate workspace.

For manually created displays, attach the viewer explicitly:

```
ACT_attach_viewer(display_id)
  → Opens an xpra window on the user's real display
  → Read-only: user can watch but not interfere
  → Report the xpra port in case user wants to connect separately

ACT_post_message(text="Spectating display {display_id}", category="e2e")
```

The user sees a window on their desktop showing the Xvfb display contents in real time. They can resize, move, or close it without affecting the test.

### 4.2. LAUNCH TEST SUBJECT

Since the Xvfb display starts empty (just i3), launch whatever the stories need.

```
FOR EACH required application (from story preconditions):

  IF application is Emacs:
    Launch a SEPARATE Emacs daemon on display :99. Do NOT connect to the user's
    live Emacs at localhost:8585 — that is on the user's display (:0).
    ACT_execute_command(
      command="DISPLAY=:99 emacs --daemon=claude-test && DISPLAY=:99 emacsclient -c -s claude-test",
      timeout=30
    )
    Wait for window to appear: poll SENSE_read_window_layout(display_id=display_id) until Emacs window present

  IF application is a terminal:
    ACT_execute_command(
      command="DISPLAY=:{display_number} ghostty &",
      timeout=10
    )
    Wait for window

  IF application is a browser:
    ACT_execute_command(
      command="DISPLAY=:{display_number} google-chrome-stable --no-first-run --disable-gpu &",
      timeout=15
    )
    Wait for window

  Verify window presence via SENSE_read_window_layout(display_id=display_id)
```

**Note**: Unlike the demonstration skill, we CAN use direct process launch here because no human is watching to learn keybindings. Efficiency over pedagogy.

### 5. RUN STORIES

For each story, execute:

```
ANNOUNCE
  ACT_post_message(text=story.name, category="e2e")

BASELINE
  ACT_now → start_ns
  SENSE_read_snapshot → baseline_state
  SENSE_read_events → baseline_events
  SENSE_capture_screen_region(display_id=display_id) → "baseline-{story-name}"

PRECONDITIONS
  For each precondition:
    Check condition using appropriate tool
    ALL display-targeting tools pass display_id=display_id
    If fail → mark BLOCKED:{reason}, skip to next story

EXECUTE
  For each step:
    ACT_now → step_start_ns
    Execute via X11 tools WITH display_id:
      ACT_send_keystroke(keys=..., display_id=display_id)
      ACT_send_text_input(text=..., display_id=display_id)
      ACT_send_click(x=..., y=..., display_id=display_id)
    ACT_delta(step_start_ns) → step_duration

INPUT SPEED
  Headless mode uses fast timing by default:
    - 20ms between keystrokes (vs 100-300ms in demonstration)
    - 50ms x11_type delay (vs 200ms in demonstration)
    - No thinking tax pauses
  This is pure functional verification, not a performance for an audience.

VERIFY
  For each expect:
    Check condition → pass/fail with actual value
  SENSE_read_anomalies(display_id=display_id) → diff against baseline
  SENSE_read_events(display_id=display_id) → diff against baseline
  SENSE_capture_screen_region(display_id=display_id) → "result-{story-name}"

RESULT
  PASS: all expects passed
  FAIL: any expect failed → log expected vs actual
  BLOCKED: precondition unmet
```

### 6. RETRY LOOP

```
IF any story FAILED:
  Analyze failure:
    - Genuine bug? → Log for code fix, continue
    - Flaky test? → Retry up to 3 times
    - Infrastructure missing? → Mark BLOCKED

  Track failure hashes for stagnation detection
  IF 3 identical failures → mark STAGNATED, stop retrying
```

### 7. TEARDOWN

```
IF spectate mode:
  ACT_detach_viewer(display_id=display_id)

ACT_destroy_display(display_id=display_id)
  → Stops Xvfb, i3, sensor, xpra
  → Cleans up all resources
```

**CRITICAL**: Always tear down the display, even on error. Wrap the entire run in a try/finally pattern. If the skill is interrupted, the display leaks. The user can clean up manually with `ACT_destroy_display` or check for orphans with `SENSE_list_displays`.

### 8. GENERATE REPORT

Write to `{project-root}/e2e-reports/e2e-{feature}-{YYYY-MM-DD-HHmm}.md`
Or `~/e2e-reports/` if no project root.

Screenshots saved to `{report-dir}/screenshots/{name}.png`

Report format:
```markdown
# E2E Test Report: {feature}
Date: {timestamp}
Duration: {total}s
Mode: headless (display {display_id})
Spectate: yes/no

## Summary
| # | Story | Result | Duration |
|---|-------|--------|----------|
| 1 | Story name | PASS | 1.2s |
| 2 | Story name | FAIL | 0.8s |
| 3 | Story name | BLOCKED | -- |

**Result: X/Y passed, Z failed, W blocked**

## Details

### 1. Story name -- PASS
**Preconditions**: condition -> status
**Steps**: step sequence
**Expectations**: each expect with actual value
**Sensor**: anomaly count, event diff
**Screenshots**: [baseline](path) | [result](path)
```

### 9. PERSIST TO VAULT

```
RECALL_save_session(
  summary: "E2E headless: {feature} -- {pass}/{total} passed, {fail} failed, {blocked} blocked",
  topics: ["e2e-test", "headless", "{feature}"],
  key_facts: [
    "e2e:{feature}:{story-name}:PASS:{duration_s}s",
    "e2e:{feature}:{story-name}:FAIL:{duration_s}s:{expected}!={actual}",
    "e2e:{feature}:{story-name}:BLOCKED:{reason}",
    "e2e-run:{feature}:{date}:{pass}/{total}:headless",
    "e2e-run:{feature}:retries:{retry_count}",
    "e2e-run:{feature}:stagnated:{story-name}" (if any),
    "e2e-regression:{feature}:{story-name}:was-PASS-now-FAIL",
    "e2e-fix:{feature}:{story-name}:was-FAIL-now-PASS",
    "e2e-flaky:{feature}:{story-name}:inconsistent-across-{N}-runs"
  ]
)
```

## Rules

1. **Display Isolation Required**: Every X11-targeting tool call MUST pass `display_id`. Forgetting this sends input to the user's real display. This is the cardinal sin of headless mode.

2. **Always Tear Down**: `ACT_destroy_display` must be called in all exit paths. Leaked Xvfb processes waste resources and block display numbers.

3. **Direct Launch Allowed**: Unlike demonstration mode, you CAN launch processes directly (no "no-magic" constraint). The headless display is a controlled environment with no observer to teach.

4. **Fast Timing Default**: Use minimal delays between keystrokes. The purpose is functional verification, not human emulation. Override with `--realistic` in arguments if human-paced timing is needed.

5. **Screenshots Mandatory**: Capture baseline before and result after every story, even though no one is watching live. The screenshots are the forensic record.

6. **Expected vs Actual**: Failed stories log expected vs actual for every failed expectation.

7. **BLOCKED != FAIL**: Blocked stories indicate missing infrastructure, not test failures.

8. **Stagnation Detection**: After 3 identical failures, stop retrying and mark STAGNATED.

9. **Spectate is Read-Only**: The xpra viewer is read-only. The user cannot accidentally interfere with tests by clicking in the viewer window.

10. **No Cohabitation Protocol**: The headless display is exclusively owned by the test runner. No shared-space negotiation needed.

## Differences from Demonstration Mode

| Aspect | Demonstration (`/e2e-loop-demonstration`) | Headless (`/e2e-loop-headless`) |
|--------|--------------------------------------------|---------------------------------|
| Display | Agent's own display :99 (user watches via xpra viewer) | Isolated Xvfb (same :99, auto-created) |
| User interaction | User observes via xpra viewer on separate workspace | None (spectate is read-only) |
| Input speed | Human-paced (50 WPM, thinking tax) | Fast (20ms between keys) |
| Keystroke display | screenkey / sensor-attention | Not needed (spectate optional) |
| Process launch | Human keybindings only (no magic) | Direct launch allowed |
| Pedagogical value | High (user learns by watching) | None (purely functional) |
| Desktop interference | Temporary workspace ownership | Zero (completely isolated) |
| Teardown | Restore workspace | Destroy Xvfb display |

## Built-in Vocabulary

### Preconditions

| Type | Fields | Behavior |
|------|--------|----------|
| `http` | url, expect (status or `fail`) | curl GET, match status |
| `process` | name | pgrep |
| `file` | path | stat |
| `bash` | cmd, expect | Run command, match output |
| `window_count` | class, title, min/max | SENSE_read_window_layout query (with display_id) |
| `focused` | class or title | SENSE_read_focus (with display_id) |

### Steps

| Type | Behavior |
|------|----------|
| `key` | ACT_send_keystroke with display_id |
| `type` | ACT_send_text_input with display_id |
| `click` | ACT_send_click with display_id |
| `wait` | Pause N seconds |
| `bash` | Run shell command (setup only) |
| `focus` | ACT_focus_window with display_id |

### Expects

| Type | Behavior |
|------|----------|
| `screenshot` | SENSE_capture_screen_region with display_id |
| `sensor_clean` | Zero geometry anomalies since baseline |
| `window_count` | Windows matching class/title via SENSE_read_window_layout with display_id |
| `window_title` | Window title matches pattern |
| `file` | Path exists, optionally contains pattern |
| `http` | GET url, match status or body |
| `bash` | Run command, match output |

## Example Story File

Story files are identical between demonstration and headless modes. The same `.yaml` file works with both skills — the difference is in execution environment, not story format.

```yaml
feature: example-feature
description: Feature description
vocabularies: [emacs, browser]

vocabulary:
  app_status:
    tool: bash
    cmd: "curl -s localhost:8080/status"

stories:
  - name: Basic functionality
    preconditions:
      - process: { name: "emacs" }
    steps:
      - key: "space c m"
      - wait: 2
    expect:
      - screenshot: "basic-result"
      - app_status: { match: "running" }
      - sensor_clean: true
```
