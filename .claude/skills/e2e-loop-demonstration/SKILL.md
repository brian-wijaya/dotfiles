---
name: e2e-loop-demonstration
description: Run end-to-end tests on your live display with human-emulated X11 input, keystroke visualization, cohabitation protocol, and pedagogical narration. The user watches in real time and learns efficient keybindings. Use for interactive test runs, feature demos, and acceptance testing.
argument-hint: [story-file or feature-name]
---

# E2E Loop — Demonstration

Live-display end-to-end testing with human-emulated X11 input, somatic differential QA, cohabitation protocol, keystroke visualization, and structured reporting. Runs on the user's primary display — the user watches and learns.

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

### 3.5. WORKSPACE SETUP

Before running stories, establish a clean testing environment on the current workspace. **All setup actions use human input (x11_key), not magic commands.**

```
IDENTIFY CONTEXT (read-only queries are allowed)
  i3_workspaces → find which workspace has the terminal running this session
  i3_windows → list all windows across all workspaces
  x11_get_active_window → identify terminal window ID (Claude's "body")
  This is reconnaissance, not action. No state changes yet.

REGISTER BODY (if somatic-attention is available)
  somatic-attention register_body(terminal_window_id)
  This anchors all visual overlays to Claude's terminal window.
  Keystroke echo activates automatically.

START KEYSTROKE DISPLAY (if somatic-attention is NOT available)
  If screenkey is not running, start it via its keybinding (super+F11)
  All subsequent keystrokes will be visible on screen

PREPARE TEST SUBJECT (using human keybindings)
  IF test subject (e.g., Emacs) is on a different workspace:

    Example: Emacs on workspace 2, terminal on workspace 1

    x11_key "super+2"           # Go to workspace 2
    wait 300ms
    # Focus the Emacs window if not already focused
    x11_key "super+shift+Left"  # or Right, depending on layout
    wait 200ms
    x11_key "super+shift+1"     # Move Emacs to workspace 1
    wait 200ms
    x11_key "super+1"           # Return to test workspace
    wait 300ms

CLEAR OBSTRUCTIONS (using human keybindings)
  Any windows on test workspace that would obstruct testing:
    Focus the window (super+shift+arrow or click)
    x11_key "super+shift+2"     # Move to workspace 2
    Repeat for each obstruction

PREPARE CLEAN STATE (using application keybindings)
  For Emacs testing:
    Focus Emacs
    x11_key "space b d" repeatedly to close buffers (or Emacs keybinding)
    x11_key "space w 1" to ensure single window
    Navigate to a clean starting state
    The goal: screenshots should be clear and unambiguous

DOCUMENT SETUP
  Log what was moved/opened/closed in the report's "Setup" section
  Include the keystrokes used — this teaches the user
```

**Workspace ownership principle**: The e2e runner takes temporary ownership of the test workspace using the same keybindings the user would use. Other workspaces serve as holding areas. The user watching can learn efficient window management by observing.

**Safety**: Never close windows with unsaved work. For Emacs, use `x11_key "space b s"` (save buffer) or similar before closing. State queries like `emacsclient -e '(buffer-modified-p)'` can check for unsaved changes.

### 3.6. COHABITATION PROTOCOL

The agent is a steward — it serves in shared space, never owns it. The user is always the guest with priority. This protocol governs cohabitation of the desktop during test runs.

**Core principle**: The user can use their machine normally on other workspaces, and may interact with the test workspace at any time. The agent adapts; the user never waits.

```
SHARED SPACE RULES:

1. NEVER ASSUME EXCLUSIVE ACCESS
   - The user may move, resize, or focus any window at any time
   - The user may switch to the test workspace and interact with the test subject
   - The user may be using their own Emacs instance on another workspace
   - Query window geometry (somatic-geometry, i3_windows) before every action
     that depends on window position

2. ADAPT TO ENVIRONMENTAL CHANGES
   - Before each story: re-query target window ID and geometry
   - If target window moved/resized: adjust coordinates silently, continue
   - If target window was closed: mark story BLOCKED, log reason, continue
   - If target window lost focus mid-story: pause, re-focus via keyboard, resume
   - If an unknown window appeared on test workspace: work around it

3. NEVER KILL PROCESSES
   - If a process conflict arises (e.g., two Emacs instances), do NOT pkill
   - Instead: pause execution, notify the user via somatic-hud post_message
     AND output a message in the conversation (the user may be on another workspace)
   - Wait for user response — do not block the conversation while waiting
   - The user decides what to do with their processes

4. USER INPUT PRIORITY
   - If somatic-input-capture detects physical keyboard input (source_id
     matches a physical device, not xdotool's virtual device), pause for 2s
   - This means the user is actively typing somewhere — don't compete
   - Resume automatically after 2s of physical input silence
   - Log pauses in the report: "Paused {duration}s: user input detected"

5. GRACEFUL DEGRADATION
   - If the test workspace becomes unusable (user rearranged everything),
     pause and ask: "Your test workspace has changed. Want me to continue
     with the new layout, reset, or stop?"
   - Never silently fail — always surface the issue

6. PROCESS SAFETY ESCALATION
   When a process conflict requires user attention:
     a. Flash warning via somatic-hud (red, 300ms)
     b. Output message in conversation: describe the conflict, what you need
     c. Continue processing other non-conflicting stories if possible
     d. Return to blocked stories after user responds
   The user should never feel interrupted — the agent works around problems
   and asks for help asynchronously.
```

**Input source differentiation**: XI2 source tagging (via `source_id` in KeyEvent) distinguishes physical keyboard input (user) from virtual input (xdotool/agent). Physical device IDs are typically low numbers (3-12); xdotool creates virtual devices with higher IDs. The agent can detect user activity by filtering for physical source IDs in the somatic-input-capture stream.

## Human Emulation Presentation

E2E tests are not just functional verification — they are **demonstrations** of human-like interaction, presented as if to an audience viewing through the primary monitor. The tester acts as a **digital choreographer**, demonstrating input sequences in real time that the user can learn from and replicate.

### The No-Magic Principle

**CRITICAL**: Every action must go through the same input path a human would use. No shortcuts. No API calls that bypass the keyboard/mouse.

**FORBIDDEN** (these are "magic"):
- `i3_command` for window management (moving, focusing, workspaces)
- Direct process manipulation
- Any tool that achieves a result without the keystrokes a human would press

**REQUIRED** (human input path):
- `x11_key` for all keyboard input, including window manager shortcuts
- `x11_click` for mouse actions
- `x11_type` for typing text

Examples of the difference:

| Task | WRONG (magic) | RIGHT (human input) |
|------|---------------|---------------------|
| Switch to workspace 2 | `i3_command "workspace 2"` | `x11_key "super+2"` |
| Move window to workspace 1 | `i3_command "[...] move to workspace 1"` | `x11_key "super+shift+1"` |
| Focus Emacs | `i3_command "[class=Emacs] focus"` | `x11_key "super+shift+Right"` (cycle) or click |
| Open terminal | `process_start("ghostty")` | User's keybinding, e.g., `x11_key "super+Return"` |

The user watching should be able to replicate every action by pressing the same keys. If they can't, it's magic.

### Keyboard-First Navigation

**Mouse is a failure mode, not a navigation tool.** The user operates a modal keyboard-first workflow (Vimium-C in Chrome, i3 keybindings, Evil mode in Emacs). The tester must compute the optimal keyboard path for every navigation action.

**Procedure for window focus changes:**
```
1. Query i3_windows (read-only, allowed) to get current window positions
2. Determine spatial relationship between current focus and target
   Example: Terminal at x=20, Emacs at x=1535 → target is RIGHT
3. Compute keyboard path:
   - Same workspace, adjacent: super+Right (or Left/Up/Down)
   - Same workspace, 2 windows away: super+Right × 2
   - Different workspace: super+N to switch, then super+Direction
4. Execute keyboard path via x11_key
5. Verify focus: x11_get_active_window
6. If wrong window focused: retry once with alternative direction
7. If second attempt fails: log as MOUSE_FALLBACK in report, use x11_click
```

**Mouse fallback is a test degradation event.** Every mouse click during navigation is logged in the report's "Setup" section with the reason keyboard navigation failed. This informs future skill improvements.

### Keystroke Display

A keystroke visualizer MUST be running during e2e tests. Two options:

**Preferred: somatic-attention** (if available)
```
BEFORE TESTING:
  register_body(terminal_window_id) via somatic-attention MCP
  Keystroke echo activates automatically, anchored to terminal
  set_attention(target_window_id) before each story's steps
```

**Fallback: screenkey**
```
BEFORE TESTING:
  Check if screenkey is running (pgrep screenkey)
  If not: x11_key for screenkey launch keybinding (super+F11)
  Verify it started (pgrep screenkey again)
  OR note in report that keystroke display was unavailable
```

Either way, the purpose is:
1. Every keypress appears on screen
2. The user can follow along and learn
3. The tester might demonstrate more efficient approaches the user hasn't tried

### Cognitive Realism Model

**Source of truth**: `~/vault/perpetual/input-emulation.org` — the perpetual specification for all input emulation decisions, including the full leader map reference and common task dispatch table. Consult it before every e2e run.

The tester emulates a **proficient but unhurried human** — someone who knows their tools well, uses efficient keybindings, but thinks between actions. No rain man behavior. The observer should feel they're watching a skilled colleague work, not a script execute.

#### Typing Speed

**Target: 50 WPM average** (~240ms per character), with realistic variance:

```
BASE_DELAY = 240ms per character
VARIANCE:
  Common words/paths:     200-280ms (muscle memory)
  Uncommon strings:       280-400ms (reading/thinking)
  After errors/backspace: 350-500ms (recovery pause)
  Between logical groups: 500-1500ms (thinking tax)

x11_type delay parameter: 200 (ms between chars)
x11_key between separate keys: 300-500ms (sleep between calls)
```

**Thinking tax**: Pause 500-1500ms between logically distinct operations. Examples:
- After opening a menu, before selecting an item
- After a file loads, before navigating within it
- After reading minibuffer output, before next command
- Between unrelated key sequences

#### Navigation Decision Model

When the tester needs to reach a target (file, buffer, position), choose the method **the user is most likely supposed to use**, given their configuration:

```
DECISION HIERARCHY (highest priority first):

1. DEDICATED KEYBINDING
   If the target has a direct binding (e.g., SPC f i for init.el,
   SPC b b for buffer list), USE IT.
   Query: emacs_elisp to check for bindings before typing paths.
   The user configured these bindings for a reason — demonstrate them.

2. RECENT/FREQUENT ACCESS
   If the target is a recently visited file/buffer:
     SPC b b (buffer switch) + partial name + TAB/RET
     SPC f r (recent files) + partial name
   Humans revisit files, not re-navigate from scratch.

3. TAB COMPLETION
   If typing a path, use TAB aggressively:
     ~/.em TAB → ~/.emacs.d/
     init TAB → init.el
   Never type a full path when TAB would complete it.
   Pause 300-500ms before each TAB (reading the completion).

4. RELATIVE NAVIGATION
   If already near the target:
     In Emacs: w, b, e for word motion; gg, G for top/bottom
     In file manager: h, l, j, k for directory traversal
   Never type absolute paths when relative motion is shorter.

5. RAW TYPING (last resort)
   Only for truly novel paths or strings with no shortcut.
   Even then, use TAB completion for directory segments.
```

**Anti-patterns (FORBIDDEN):**
- Typing `/home/bw/.emacs.d/init.el` when `SPC f i` or `SPC f r` + "init" works
- Typing full filenames when TAB completion is available
- Moving cursor 13 characters right instead of using `w` twice
- Any navigation that requires knowing exact character counts

#### Audio Feedback

During e2e execution, all keystrokes produce audio feedback:
- **Regular keys**: Short tap sound (material-design ui_tap variants, rotated)
- **Modifier keys** (Shift, Ctrl, Super, Alt): Distinct modifier sound
- **Return/Enter**: Confirm sound
- **Escape**: Cancel/dismiss sound
- **Backspace**: Error-correction sound

Audio is provided by the keystroke sound daemon (somatic-input-capture based).
If the daemon is not running, note in the report that keystroke audio was unavailable.

### Principles

1. **Workspace Visibility**: The test workspace must remain active and visible throughout testing. Use human keybindings (e.g., `super+1`) to switch workspaces, never direct commands.

2. **Human-Paced Input**: Follow the Cognitive Realism Model above.
   - 50 WPM average (~240ms per character)
   - Thinking tax between logical groups (500-1500ms)
   - After significant actions: wait for visual feedback before continuing
   - This allows an observer to follow along and mentally trace each input

3. **Externalized Terminal Commands**: When terminal commands must be run during testing:
   - Use the keybinding to open a terminal (e.g., `super+Return`)
   - Type commands via `x11_type` at human speed
   - Let the output appear visually on screen
   - This demonstrates the full manual workflow

4. **Narrated Execution**: Each action should be visually traceable:
   - Flash the story name before starting
   - Screenshots capture each state transition
   - Keystroke display shows what was pressed
   - The visual record should tell the complete story

5. **Pedagogical Value**: The tester may know more efficient keybindings or workflows than the user. By demonstrating them visibly, the user learns. This is a feature, not a side effect.

### Implementation

```
WORKSPACE NAVIGATION (example: move Emacs from workspace 2 to workspace 1):
  x11_key "super+2"           # Go to workspace 2
  wait 300ms                  # Let workspace switch complete
  x11_key "super+shift+Left"  # Focus Emacs if needed (or Right, depending on layout)
  x11_key "super+shift+1"     # Move focused window to workspace 1
  x11_key "super+1"           # Return to workspace 1
  wait 300ms                  # Let workspace switch complete

FOR KEYSTROKES:
  x11_key with ~100ms delay between keys
  For sequences like "space f f", send each key separately with visible pacing

FOR TERMINAL COMMANDS:
  x11_key "super+Return"      # Open terminal (user's configured binding)
  wait for window
  x11_type "command here"     # Type at human speed (~50ms per char)
  x11_key "Return"            # Execute
  wait for output
  screenshot

BETWEEN STORIES:
  Brief pause (500ms-1s) to let observer register completion
```

### 3.5. SESSION EXEMPTION

No exemption needed — resource scaling is handled by kinetic's VaultResourceScaler
based on X11 idle time, independent of e2e test execution.

### 4. RUN STORIES

For each story, execute:

```
ANNOUNCE
  somatic-hud post_message(story.name, x=50, y=50, color="#FFFF00", duration_ms=200)

BASELINE
  somatic-temporal now → start_ns
  somatic-fusion get_snapshot
  somatic-x11-bus get_events(100)
  x11_screenshot → "baseline-{story-name}"

PRECONDITIONS
  For each precondition:
    Check condition using appropriate tool
    If fail → mark BLOCKED:{reason}, skip to next story

ATTENTION (if somatic-attention registered)
  Determine target window for this story's steps
  somatic-attention set_attention(target_window_id, label=story.name)
  somatic-attention set_status("executing", message=story.name)

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

SOUND FEEDBACK
  On PASS:    Bash "~/.claude/hooks/e2e-sound.sh pass"
  On FAIL:    Bash "~/.claude/hooks/e2e-sound.sh fail"
  On BLOCKED: Bash "~/.claude/hooks/e2e-sound.sh blocked"
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
    Bash "~/.claude/hooks/e2e-sound.sh stagnated"
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

E2E results are structured knowledge that accumulates across runs. Each run produces facts that enable regression detection, flakiness tracking, and feature health monitoring.

```
vault-rag save_session(
  summary: "E2E: {feature} — {pass}/{total} passed, {fail} failed, {blocked} blocked",
  topics: ["e2e-test", "{feature}"],
  key_facts: [
    // Per-story results with duration and failure detail
    "e2e:{feature}:{story-name}:PASS:{duration_s}s",
    "e2e:{feature}:{story-name}:FAIL:{duration_s}s:{expected}!={actual}",
    "e2e:{feature}:{story-name}:BLOCKED:{reason}",

    // Run-level metadata
    "e2e-run:{feature}:{date}:{pass}/{total}",
    "e2e-run:{feature}:retries:{retry_count}",
    "e2e-run:{feature}:stagnated:{story-name}" (if any),

    // Cohabitation protocol events
    "e2e-cohabitation:pause:{duration_s}s:user-input" (if user input caused pause),
    "e2e-cohabitation:mouse-fallback:{story-name}:{reason}" (if keyboard nav failed),
    "e2e-cohabitation:window-moved:{story-name}" (if target geometry changed mid-run),

    // Regression detection: compare against previous run facts
    // If vault-rag search_sessions(query="e2e:{feature}") returns prior results,
    // emit regression/fix facts:
    "e2e-regression:{feature}:{story-name}:was-PASS-now-FAIL",
    "e2e-fix:{feature}:{story-name}:was-FAIL-now-PASS",
    "e2e-flaky:{feature}:{story-name}:inconsistent-across-{N}-runs"
  ]
)
```

**Regression detection procedure**: Before persisting, search vault for the most recent prior e2e run of the same feature. Compare per-story results. Any story that changed status (PASS→FAIL, FAIL→PASS, new BLOCKED) emits a regression/fix/flaky fact. This creates a queryable history: "which stories regressed this week?" becomes a vault-rag search.

## Rules

1. **X11 Input Only**: ALL interactions use x11_key, x11_type, x11_click. This includes window management, workspace switching, and application control. Never use direct APIs (i3_command, process_start, etc.) for actions a human would do with keyboard/mouse.

2. **No Magic**: If a human couldn't do it by pressing keys or clicking, don't do it. The user watching should be able to replicate every action. Direct API calls that bypass input are forbidden.

3. **State Queries Allowed**: Read-only queries (emacsclient -e, curl, i3_windows, etc.) are valid for preconditions and expects. These don't change state — they just observe it.

4. **Screenshots Mandatory**: Capture baseline before and result after every story.

5. **Expected vs Actual**: Failed stories log expected vs actual for every failed expectation.

6. **BLOCKED ≠ FAIL**: Blocked stories indicate missing infrastructure, not test failures.

7. **User Approval Required**: Never skip the DISPLAY PLAN step.

8. **Stagnation Detection**: After 3 identical failures, stop retrying and mark STAGNATED.

9. **Human-Paced Input**: ~100ms between keystrokes, pauses between logical groups. Never faster than a human could type.

10. **Keystroke Display**: Run screenkey or equivalent so all input is visible on screen. The user learns by watching.

11. **Pedagogical Intent**: The test run teaches. Demonstrate efficient keybindings. The user may learn workflows they didn't know.

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
