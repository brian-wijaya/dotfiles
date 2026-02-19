---
name: sense-user
description: Sense user's current state (typing rhythm, mouse activity, engagement level) before taking significant actions. Use before complex operations, large refactors, or when user seems uncertain.
user-invocable: false
allowed-tools: mcp__gateway__SENSE_read_state_vector, mcp__gateway__SENSE_read_timing, mcp__gateway__SENSE_read_dynamics
---

# Sense User State

Query sensor sensors to understand user's current cognitive state before proceeding.

## Quick Check

Use `mcp__gateway__SENSE_read_state_vector` for a compact 14-dimension state:

| Dimension | Meaning | High Value Means |
|-----------|---------|------------------|
| ptr_velocity | Mouse speed | Active movement |
| ptr_dwell | Time since movement | AFK or reading |
| typing_iki | Inter-key interval | Slow = thinking |
| typing_burst | Burst ratio | High = confident typing |
| typing_hesitation | Hesitation score | High = uncertain |

## Interpretation

**User is confident and engaged:**
- Low hesitation (< 0.1)
- High burst ratio (> 0.6)
- Low dwell (< 5000ms)
→ Proceed normally

**User is uncertain or thinking:**
- High hesitation (> 0.3)
- Low burst ratio (< 0.4)
- Variable IKI
→ Explain more, ask for confirmation

**User may be AFK:**
- Very high dwell (> 60000ms)
- No recent typing
→ Don't rush, they'll return

**User is struggling:**
- High hesitation + low burst + high IKI variance
→ Simplify, break into steps, offer help

## Usage

Before complex operations, check state and adjust approach accordingly. Don't announce that you're checking - just use the information to calibrate your response.
