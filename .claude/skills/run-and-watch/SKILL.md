---
name: run-and-watch
description: Run a command while watching X11 events and timing. Use for testing, debugging, or when you need to correlate a command's execution with desktop side effects.
disable-model-invocation: true
allowed-tools: mcp__somatic-temporal__now, mcp__somatic-temporal__delta, mcp__somatic-x11-bus__read_events, mcp__somatic-hud__flash_text, Bash
---

# Run Command with Somatic Observation

Execute a command while capturing timing and X11 events to understand its side effects.

## Arguments

`$ARGUMENTS` = the command to run

## Process

1. **Flash start indicator**:
   ```
   mcp__somatic-hud__flash_text(text="WATCHING", x=50, y=50, color="#FFFF00", duration_ms=200)
   ```

2. **Mark start time**:
   ```
   mcp__somatic-temporal__now → start_ns
   ```

3. **Run the command**:
   ```
   Bash: $ARGUMENTS
   ```

4. **Mark end time and calculate duration**:
   ```
   mcp__somatic-temporal__delta(from_ns=start_ns)
   ```

5. **Query X11 events during execution**:
   ```
   mcp__somatic-x11-bus__read_events(count=200)
   Filter to events with timestamp >= start_ns
   ```

6. **Report**:
   - Command output
   - Execution duration (ms)
   - X11 events that occurred during execution
   - Any anomalies (focus changes, window creation/destruction, rapid resizes)

7. **Flash completion**:
   ```
   mcp__somatic-hud__flash_text(text="DONE", x=50, y=50, color="#00FF00", duration_ms=200)
   ```

## Example Output

```
Command: pytest tests/test_foo.py
Duration: 2,341ms
Exit code: 0

X11 Events During Execution:
- +12ms: CreateNotify window=0x123 (test runner UI)
- +45ms: MapNotify window=0x123
- +2,298ms: UnmapNotify window=0x123
- +2,305ms: DestroyNotify window=0x123

Analysis: Test runner created a window, ran tests, then cleaned up normally.
No anomalies detected.
```
