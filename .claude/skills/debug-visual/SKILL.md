---
name: debug-visual
description: Debug visual issues (flickering, focus stealing, layout glitches) using somatic X11 perception. Use when something looks wrong on screen.
allowed-tools: mcp__somatic-temporal__now, mcp__somatic-temporal__delta, mcp__somatic-x11-bus__read_events, mcp__somatic-x11-bus__read_focus, mcp__somatic-hud__flash_text
---

# Debug Visual Issue with Somatic Perception

You have access to the X11 sensory bus which captures all desktop events with nanosecond timestamps.

**Display targeting**: By default, screenshots and X11 event queries target the agent's isolated display :99 (auto-created by kinetic when `display.enabled = true`). This is the agent's own Xvfb display, not the user's live screen. If debugging an issue on the user's display (:0), pass the appropriate display parameter explicitly.

## Process

1. **Mark time** before reproduction:
   ```
   mcp__somatic-temporal__now → note the timestamp
   ```

2. **Ask user to reproduce** the visual issue

3. **Query X11 events** that occurred:
   ```
   mcp__somatic-x11-bus__read_events(count=100)
   ```

4. **Analyze for patterns**:
   - **Flickering**: Rapid ConfigureNotify events on same window (< 100ms apart)
   - **Focus stealing**: FocusOut followed by FocusIn on different window
   - **Ghost windows**: CreateNotify → DestroyNotify with no MapNotify
   - **Layout thrashing**: Multiple ConfigureNotify with changing x,y,w,h

5. **Report findings** with:
   - Exact timestamps (use `mcp__somatic-temporal__delta` to show human-readable intervals)
   - Window IDs involved
   - Event sequence that caused the issue
   - Likely culprit

6. **Flash confirmation** when done:
   ```
   mcp__somatic-hud__flash_text(text="DEBUG COMPLETE", x=100, y=100, color="#00FF00")
   ```

## Event Types to Watch

| Event | Meaning |
|-------|---------|
| ConfigureNotify | Window moved/resized |
| MapNotify | Window became visible |
| UnmapNotify | Window became hidden |
| FocusIn/FocusOut | Keyboard focus changed |
| CreateNotify | New window created |
| DestroyNotify | Window destroyed |
| PropertyNotify | Window property changed (title, class, etc.) |

## Example Analysis

"I see rapid ConfigureNotify events on window 0x1234567:
- 607629987507497ns: (100, 100, 800, 600)
- 607629987717101ns: (100, 100, 801, 600)  ← 209μs later
- 607629987927302ns: (100, 100, 800, 600)  ← 210μs later

This is sub-millisecond resize oscillation, likely caused by a layout manager fighting with the application's size hints."
