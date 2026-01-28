#!/usr/bin/env python3
"""Keystroke sound daemon — plays Material Design tap sounds on every keypress.

Polls somatic-input-capture for new keystrokes and plays audio feedback.
Uses PulseAudio/PipeWire via subprocess paplay.

Usage: keystroke-sound-daemon.py [--virtual-only]
  --virtual-only: Only sound on virtual input (xdotool, agent keystrokes)
  Default: sounds on ALL keystrokes
"""
import json
import subprocess
import sys
import os
import time
import random
import threading
from pathlib import Path

SOUND_BASE = Path.home() / ".local/share/sounds/material-design/material_product_sounds/ogg"
PRIMARY = SOUND_BASE / "03 Primary System Sounds"
SECONDARY = SOUND_BASE / "04 Secondary System Sounds"

# Tap variants for regular keys (rotated for variety)
TAP_SOUNDS = [
    PRIMARY / "ui_tap-variant-01.ogg",
    PRIMARY / "ui_tap-variant-02.ogg",
    PRIMARY / "ui_tap-variant-03.ogg",
    PRIMARY / "ui_tap-variant-04.ogg",
]

# Special key sounds
MODIFIER_SOUND = PRIMARY / "navigation_hover-tap.ogg"
RETURN_SOUND = PRIMARY / "state-change_confirm-up.ogg"
ESCAPE_SOUND = SECONDARY / "navigation-cancel.ogg"
BACKSPACE_SOUND = PRIMARY / "ui_tap-variant-02.ogg"  # Same family as letter keys
TAB_SOUND = PRIMARY / "navigation_forward-selection-minimal.ogg"

# Modifier keysyms
MODIFIER_SYMS = {
    65505, 65506,  # Shift_L, Shift_R
    65507, 65508,  # Control_L, Control_R
    65513, 65514,  # Alt_L, Alt_R
    65515, 65516,  # Super_L, Super_R
}

# Special keysyms
RETURN_SYMS = {65293, 65421}  # Return, KP_Enter
ESCAPE_SYM = 65307
BACKSPACE_SYM = 65288
TAB_SYM = 65289

# Virtual input source IDs are typically > 12
PHYSICAL_SOURCE_MAX = 12

# Agent-active flag file: touched by agent before x11_key/x11_type calls.
# If the flag is fresh (< 2s), keystrokes are considered agent-generated.
AGENT_FLAG = Path.home() / ".claude/agent-typing"

# Rate limit: don't play more than N sounds per second
MAX_SOUNDS_PER_SEC = 12
sound_times = []
sound_lock = threading.Lock()

tap_index = 0
virtual_only = "--virtual-only" in sys.argv


def play_sound(path):
    """Play a sound file asynchronously, rate-limited."""
    global sound_times
    now = time.monotonic()

    with sound_lock:
        # Prune old entries
        sound_times = [t for t in sound_times if now - t < 1.0]
        if len(sound_times) >= MAX_SOUNDS_PER_SEC:
            return  # Rate limited
        sound_times.append(now)

    if path.exists():
        subprocess.Popen(
            ["paplay", str(path)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )


def sound_for_keysym(keysym):
    """Choose sound file based on keysym."""
    global tap_index

    if keysym in MODIFIER_SYMS:
        return MODIFIER_SOUND
    if keysym in RETURN_SYMS:
        return RETURN_SOUND
    if keysym == ESCAPE_SYM:
        return ESCAPE_SOUND
    if keysym == BACKSPACE_SYM:
        return BACKSPACE_SOUND
    if keysym == TAB_SYM:
        return TAB_SOUND

    # Regular key: rotate through tap variants
    sound = TAP_SOUNDS[tap_index % len(TAP_SOUNDS)]
    tap_index += 1
    return sound


def poll_keystrokes():
    """Poll somatic-input-capture MCP for keystrokes via its JSONL log."""
    log_path = Path.home() / ".claude/keylog.jsonl"

    if not log_path.exists():
        print(f"Waiting for {log_path}...", file=sys.stderr)
        while not log_path.exists():
            time.sleep(1)

    # Tail the log file
    proc = subprocess.Popen(
        ["tail", "-n", "0", "-F", str(log_path)],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )

    print(f"Keystroke sound daemon started (virtual_only={virtual_only})", file=sys.stderr)

    for line in proc.stdout:
        line = line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue

        # Only key press events (p=1)
        if not event.get("p", 0):
            continue

        # Source filtering: use agent flag file instead of unreliable XI2 source_id
        if virtual_only:
            if not AGENT_FLAG.exists():
                continue
            flag_age = time.time() - AGENT_FLAG.stat().st_mtime
            if flag_age > 2.0:
                continue  # Flag is stale — not agent typing

        keysym = event.get("ks", 0)
        sound = sound_for_keysym(keysym)
        play_sound(sound)


if __name__ == "__main__":
    try:
        poll_keystrokes()
    except KeyboardInterrupt:
        pass
