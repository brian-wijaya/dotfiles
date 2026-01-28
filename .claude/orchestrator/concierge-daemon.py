#!/usr/bin/env python3
"""Concierge daemon — pauses agent sessions when user is active at the keyboard/mouse.

Polls xprintidle for X11 idle time. When user becomes active (idle < threshold),
pauses all WORKING orchestrator sessions. When user goes idle (idle >= threshold),
resumes them.

Sessions in E2E_RUNNING state are exempt (they aren't WORKING, so pause_all skips them).
"""

import json
import os
import socket
import subprocess
import sys
import time

ORCHESTRATOR_SOCK = os.path.expanduser("~/.claude/orchestrator/daemon.sock")
POLL_INTERVAL = 0.5  # seconds
IDLE_THRESHOLD_MS = 2000  # 2s of X11 idle before resuming agents
SOUND_SCRIPT = os.path.expanduser("~/.claude/hooks/lifecycle-sound.sh")

STATE_IDLE = "idle"
STATE_ACTIVE = "active"


def get_x11_idle_ms() -> int:
    """Get X11 idle time in milliseconds via xprintidle."""
    try:
        result = subprocess.run(
            ["xprintidle"], capture_output=True, text=True, timeout=2
        )
        return int(result.stdout.strip())
    except Exception:
        return 999999  # Assume idle on error


def orchestrator_call(method: str, params: dict = None) -> dict:
    """Call orchestrator daemon via Unix socket."""
    if params is None:
        params = {}
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(2)
        sock.connect(ORCHESTRATOR_SOCK)
        msg = json.dumps({"method": method, "params": params})
        sock.sendall(msg.encode() + b"\n")
        data = sock.recv(4096)
        sock.close()
        return json.loads(data)
    except Exception as e:
        return {"error": str(e)}


def play_sound(event: str):
    """Play lifecycle sound asynchronously."""
    try:
        subprocess.Popen([SOUND_SCRIPT, event], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except Exception:
        pass


def main():
    state = STATE_IDLE

    print(f"Concierge daemon started (poll={POLL_INTERVAL}s, idle_threshold={IDLE_THRESHOLD_MS}ms)", flush=True)

    while True:
        try:
            idle_ms = get_x11_idle_ms()
            user_active = idle_ms < IDLE_THRESHOLD_MS

            if user_active and state == STATE_IDLE:
                state = STATE_ACTIVE
                result = orchestrator_call("pause_all_sessions")
                paused = result.get("paused", 0)
                print(f"[concierge] User active (idle {idle_ms}ms) — paused {paused} sessions")
                play_sound("concierge-pause")

            elif not user_active and state == STATE_ACTIVE:
                state = STATE_IDLE
                result = orchestrator_call("resume_all_sessions")
                resumed = result.get("resumed", 0)
                print(f"[concierge] User idle ({idle_ms}ms) — resumed {resumed} sessions")
                play_sound("concierge-resume")

            time.sleep(POLL_INTERVAL)

        except KeyboardInterrupt:
            print("Concierge daemon stopped")
            sys.exit(0)
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            time.sleep(POLL_INTERVAL)


if __name__ == "__main__":
    main()
