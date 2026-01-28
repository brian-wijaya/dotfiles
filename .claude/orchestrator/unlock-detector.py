#!/usr/bin/env python3
"""Unlock detector — resumes agent sessions when screen is unlocked.

Polls loginctl for LockedHint. On locked→unlocked transition,
resumes all paused sessions and plays unlock sound.
"""

import json
import os
import socket
import subprocess
import sys
import time

ORCHESTRATOR_SOCK = os.path.expanduser("~/.claude/orchestrator/daemon.sock")
POLL_INTERVAL = 0.5
SOUND_SCRIPT = os.path.expanduser("~/.claude/hooks/lifecycle-sound.sh")
HUD_CATEGORY = "info"


def is_locked() -> bool:
    """Check if session is locked via loginctl."""
    try:
        result = subprocess.run(
            ["loginctl", "show-session", "auto", "-p", "LockedHint"],
            capture_output=True, text=True, timeout=2
        )
        return "LockedHint=yes" in result.stdout
    except Exception:
        return False


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
    try:
        subprocess.Popen(
            [SOUND_SCRIPT, event],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
    except Exception:
        pass


def main():
    was_locked = is_locked()
    print(f"Unlock detector started (locked={was_locked})")

    while True:
        try:
            locked = is_locked()

            if was_locked and not locked:
                # Transition: locked → unlocked
                result = orchestrator_call("resume_all_sessions")
                resumed = result.get("resumed", 0)
                print(f"Screen unlocked — resumed {resumed} sessions")
                play_sound("unlock")

            elif not was_locked and locked:
                # Transition: unlocked → locked
                result = orchestrator_call("pause_all_sessions")
                paused = result.get("paused", 0)
                print(f"Screen locked — paused {paused} sessions")

            was_locked = locked
            time.sleep(POLL_INTERVAL)

        except KeyboardInterrupt:
            print("Unlock detector stopped")
            sys.exit(0)
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            time.sleep(POLL_INTERVAL)


if __name__ == "__main__":
    main()
