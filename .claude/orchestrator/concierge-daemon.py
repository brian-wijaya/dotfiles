#!/usr/bin/env python3
"""Concierge daemon — pauses agent sessions when user is active at the keyboard/mouse.

Polls somatic-fusion for typing/pointer activity. When user becomes active,
pauses all WORKING orchestrator sessions. When user goes idle (2s debounce),
resumes them.

Sessions in E2E_RUNNING state are exempt from pausing.
"""

import json
import os
import socket
import subprocess
import sys
import time

ORCHESTRATOR_SOCK = os.path.expanduser("~/.claude/orchestrator/daemon.sock")
POLL_INTERVAL = 0.5  # seconds
IDLE_DEBOUNCE = 2.0  # seconds of inactivity before resuming
SOUND_SCRIPT = os.path.expanduser("~/.claude/hooks/lifecycle-sound.sh")

# State
STATE_IDLE = "idle"
STATE_ACTIVE = "active"


def get_snapshot() -> dict:
    """Get somatic fusion snapshot via MCP stdio JSON-RPC."""
    try:
        proc = subprocess.run(
            ["somatic-fusion"],
            input=json.dumps({"jsonrpc": "2.0", "id": 1, "method": "get_snapshot", "params": {}}),
            capture_output=True, text=True, timeout=2
        )
        resp = json.loads(proc.stdout)
        return resp.get("result", {})
    except Exception:
        return {}


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
    last_active_time = 0.0
    idle_count = 0  # consecutive idle readings for debounce

    print(f"Concierge daemon started (poll={POLL_INTERVAL}s, debounce={IDLE_DEBOUNCE}s)")

    while True:
        try:
            snapshot = get_snapshot()
            typing = snapshot.get("typing_active", False)
            pointer_idle = snapshot.get("pointer_idle", True)
            user_active = typing or not pointer_idle

            if user_active:
                last_active_time = time.time()
                idle_count = 0

                if state == STATE_IDLE:
                    # Transition: IDLE → ACTIVE
                    state = STATE_ACTIVE
                    result = orchestrator_call("pause_all_sessions")
                    paused = result.get("paused", 0)
                    print(f"User active — paused {paused} sessions")
                    play_sound("concierge-pause")
            else:
                idle_count += 1
                idle_duration = time.time() - last_active_time

                if state == STATE_ACTIVE and idle_duration >= IDLE_DEBOUNCE:
                    # Transition: ACTIVE → IDLE (debounced)
                    state = STATE_IDLE
                    result = orchestrator_call("resume_all_sessions")
                    resumed = result.get("resumed", 0)
                    print(f"User idle ({idle_duration:.1f}s) — resumed {resumed} sessions")
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
