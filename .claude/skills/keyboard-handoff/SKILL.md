---
name: keyboard-handoff
description: Coordinate keyboard access with user before sending X11 keystrokes. Detects typing activity, notifies user, waits for idle, then sends keys. Use BEFORE any x11_key sequence during interactive testing.
user-invocable: false
allowed-tools: mcp__gateway__SENSE_is_typing, mcp__gateway__SENSE_post_message, mcp__x11__notify, mcp__x11__x11_key, mcp__x11__x11_get_active_window, mcp__x11__i3_command
---

# Keyboard Handoff Protocol

## When to Use

BEFORE sending any `x11_key` calls **that target the user's live display (:0)** during interactive testing. The user may be actively using their keyboard — sending keys blindly will:
1. Corrupt the test (keys go to wrong place)
2. Interrupt the user's work
3. Cause confusing state that's hard to debug

**When NOT needed**: Keyboard handoff is NOT required when sending keystrokes to the agent's own isolated display (:99). Since gateway auto-creates display :99 on startup (`display.enabled = true` in `~/.config/actual/gateway.toml`), and all display-routed tools automatically target :99, handoff is unnecessary for the vast majority of agent operations. The user cannot be typing on :99 — it is the agent's exclusive display. Only invoke this protocol when explicitly targeting the user's display (:0).

## Protocol Steps

### 1. Check if user is typing
```
mcp__gateway__SENSE_is_typing → {typing: bool, last_key_age_ms: int}
```
If `typing: true` or `last_key_age_ms < 3000`:
- Post overlay: "⌨ Waiting for keyboard idle..."
- Poll `is_typing` every 2-3 seconds until idle for 3+ seconds

### 2. Notify user
```
mcp__gateway__SENSE_post_message(text="⌨ KEYBOARD SEIZED — ~5s for testing", category="warn", ttl_ms=15000)
```
- Include estimated duration in the message
- Wait 1.5 seconds after notification before sending keys

### 3. Focus correct window
- Verify target window (Emacs) is focused via `x11_get_active_window`
- If not focused, use `i3_command` to focus it
- Wait briefly after focus change

### 4. Send keystroke sequence
- Send all planned `x11_key` calls
- Keep sequence as short as possible

### 5. Release
```
mcp__gateway__SENSE_post_message(text="⌨ Keyboard released", category="info", ttl_ms=4000)
```

## Prefer elisp

ALWAYS prefer `emacs_elisp` for testing when possible. Only use X11 keystrokes when you specifically need to test:
- `post-self-insert-hook` behavior (auto-expanding snippets)
- Evil state transitions triggered by actual key input
- Real TAB dispatch chains
- CDLaTeX backtick/quote sequences that depend on `last-command-event`

For everything else (checking buffer content, mode state, variable values, function return values), use `emacs_elisp`.

## Batching

When running multiple X11 key tests, batch them into one seized window:
1. Seize once
2. Run all key sequences with elisp verification between each
3. Release once

Do NOT seize/release for each individual test.
