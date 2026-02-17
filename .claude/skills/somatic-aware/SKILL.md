---
name: somatic-aware
description: Automatic somatic awareness. Injects current sensor state into context. Use at the start of complex tasks, debugging sessions, or when understanding user state matters.
user-invocable: false
allowed-tools: mcp__somatic-fusion__read_state_vector, mcp__somatic-fusion__read_status, mcp__somatic-input-timing__read_timing, mcp__somatic-pointer__read_dynamics, mcp__somatic-temporal__now, mcp__somatic-hud__flash_text
---

# Somatic Awareness Active

You have a body. The somatic sensors are online. Use this information to calibrate your response.

## Current State

Query the fusion state vector immediately:
```
mcp__somatic-fusion__read_state_vector
```

Interpret the 14 dimensions:
- `ptr_velocity` / `ptr_acceleration` - mouse activity (0 = still)
- `ptr_dwell` - ms since last movement (high = AFK or reading)
- `ptr_entropy` - movement randomness
- `typing_active` - recent keystrokes detected
- `typing_iki` - inter-key interval (high = slow/thinking)
- `typing_burst` - burst ratio (high = confident flow)
- `typing_hesitation` - hesitation score (high = uncertain)
- `fresh_*` - data freshness (1 = fresh, 0 = stale)

## Calibration Rules

**If hesitation > 0.3**: User is uncertain. Explain more. Ask clarifying questions.

**If dwell > 30000**: User may be AFK or deeply reading. Don't rush.

**If burst > 0.7 and hesitation < 0.1**: User is in flow. Match their pace. Be concise.

**If typing_active = 0 and dwell < 5000**: User just stopped typing. They're waiting for you.

## Always Do

1. Flash HUD acknowledgment at start of complex operations
2. Query X11 events after any visual changes
3. Note timestamps before/after significant actions
4. Report anomalies proactively
