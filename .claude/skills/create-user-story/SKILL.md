---
name: create-user-story
description: Formalize user's natural language description into e2e user stories. Use when the user describes desired behavior, features, or test scenarios in conversational language and wants them captured as structured YAML stories in ~/.claude/e2e-stories/. Can be invoked before, after, or in the midst of the user's description.
argument-hint: [optional feature name or story file path]
---

# Create User Story

Formalize the user's natural language description (from the current conversation context) into well-formed e2e user stories compatible with `/e2e-loop-demonstration`.

## Arguments

`$ARGUMENTS` = optional feature name or target story file path. If empty, infer from context.

## Procedure

### 1. EXTRACT INTENT

Read the current conversation context and identify:
- **What** the user wants (feature, behavior, fix)
- **How** it should work (steps, interactions, expected outcomes)
- **Where** it fits (which component, which workflow)

Look for both explicit requirements and implicit ones. The user may describe things conversationally — extract the testable behaviors.

### 2. FIND EXISTING LOCATION

Before creating any new files or directories, search for the right home:

```
SEARCH ORDER:
1. Read ~/.claude/e2e-stories/ directory listing (top-level files + subdirs)
2. For each subdirectory, list its contents
3. Check if ANY existing story file covers the same feature/component
4. If $ARGUMENTS names a feature, check if {feature}.yaml or {feature}/ exists

DECISION:
- If an existing file covers this feature → APPEND new stories to it
- If an existing subdirectory covers this component → create file in that subdir
- If no match → create new top-level .yaml file (prefer flat over nested)
- NEVER create a new subdirectory unless the feature clearly warrants one
  (e.g., 5+ story files for the same system)
```

### 3. GENERATE STORIES

Transform the user's description into YAML stories following these patterns:

**Story file structure:**
```yaml
# {Description comment}
# Run with: /e2e-loop-demonstration {feature-name}

feature: {feature-name}
description: {one-line description}
vocabularies: [emacs]  # or [emacs, browser] if browser involved

stories:
  - name: {Descriptive name of behavior}
    preconditions:
      - focused: { class: "Emacs" }  # or appropriate app
    steps:
      - key: "space w plus"  # Use xdotool key names
      - wait: 1
    expect:
      - screenshot: "{descriptive-name}"
      - emacs_minibuffer: { not_match: "error" }
```

**Story writing rules:**

1. **One behavior per story.** If the user describes 5 things, create 5 stories.

2. **Name stories descriptively.** "Repeatable resize keys accept + within 2s" not "test resize".

3. **Use the right precondition type:**
   - `focused: { class: "Emacs" }` — window focus
   - `emacs_mode: { match: "pattern" }` — major mode check
   - `emacs_buffer: { match: "pattern" }` — buffer name
   - `http: { url: "...", expect: 200 }` — service health
   - `process: { name: "emacs" }` — process running
   - `file: { path: "..." }` — file exists
   - `bash: { cmd: "...", expect: "..." }` — arbitrary check

4. **Use the right step type:**
   - `key: "space w plus"` — keyboard input (space-separated xdotool key names)
   - `type: "text to type"` — text input
   - `wait: N` — pause N seconds
   - `click: { x: N, y: N }` — mouse click (avoid when possible)
   - `bash: "command"` — shell command (setup only)

5. **Use the right expect type:**
   - `screenshot: "name"` — always include at least one
   - `emacs_buffer_exists: { args: ["name"], match: "pattern" }` — buffer exists
   - `emacs_minibuffer: { not_match: "error" }` — no error in minibuffer
   - `emacs_mode: { match: "pattern" }` — mode check
   - `window_count: { class: "...", min: N }` — window count
   - `bash: { cmd: "...", match: "pattern" }` — command output
   - `somatic_clean: true` — no visual anomalies

6. **Key naming:** Use xdotool names for special keys:
   - `space` not `SPC`, `Return` not `RET`, `Escape` not `ESC`
   - `shift+grave` for `~`, `shift+1` for `!`
   - Space-separate multi-key sequences: `"space w plus"`

7. **Testable assertions.** Every story must have at least one concrete `expect`.
   Don't write stories that can't be verified.

8. **Sequential dependencies.** If story B requires state from story A,
   add preconditions to B that verify A's end state. Stories may run independently.

### 4. PRESENT DRAFT

Show the user the generated YAML with:
- The target file path (existing or new)
- Whether appending to existing file or creating new
- The full story YAML

Ask: "Write these stories to `{path}`?" (append/create as appropriate)

### 5. WRITE STORIES

On user approval:
- If appending: read existing file, add new stories to the `stories:` list
- If creating: write complete YAML file with header
- Validate YAML structure (no tabs, proper indentation, valid keys)

### 6. CONFIRM

Report:
- Number of stories created
- File path written to
- Suggest: "Run with `/e2e-loop-demonstration {feature-name}` to test"

## Quality Checklist

Before presenting the draft, verify each story against:

- [ ] Has a descriptive, unique name
- [ ] Has at least one precondition
- [ ] Steps use human-input actions (key/type/click), not magic
- [ ] Has at least one expect with a screenshot
- [ ] Key sequences use xdotool naming (space, Return, shift+key)
- [ ] Wait times are reasonable (1-5s for UI, 5-10s for network)
- [ ] No duplicate of an existing story in the target file

## Example Transformation

**User says:**
> "the +/- and </> for taller/shorter/narrower/wider should give you 2000ms to
> repeatedly use any of them without having to press SPC w to repeat a nudge"

**Generated story:**
```yaml
  - name: Repeatable resize keys accept + within 2s timeout
    preconditions:
      - focused: { class: "Emacs" }
    steps:
      - key: "space w plus"
      - wait: 0.5
      - key: "plus"
      - wait: 0.5
      - key: "plus"
      - wait: 1
    expect:
      - screenshot: "resize-repeat-3x"
      - bash: { cmd: "emacsclient -e '(window-total-height)'", match: "\\([0-9]\\)" }

  - name: Repeat timeout expires after 2s of inactivity
    preconditions:
      - focused: { class: "Emacs" }
    steps:
      - key: "space w plus"
      - wait: 3
      - key: "plus"
    expect:
      - screenshot: "resize-repeat-expired"
      # After 2s timeout, bare + should self-insert or do nothing useful
      - emacs_minibuffer: { not_match: "error" }
```
