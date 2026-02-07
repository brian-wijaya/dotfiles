#!/usr/bin/env python3
"""Concierge daemon — manages agent sessions and vault-rag resources based on user activity.

Manages two independent state machines based on X11 idle time:

1. Session Management (2s threshold):
   - User active (< 2s idle): Pause WORKING orchestrator sessions
   - User idle (>= 2s): Resume sessions

2. Vault-RAG Resource Limits (6h threshold):
   - User active (< 6h idle): Conservative limits (8GB RAM, 50% CPU)
   - User idle (>= 6h): Aggressive limits (32GB RAM, 100% CPU)

Sessions in E2E_RUNNING state are exempt from pausing.
"""

import json
import os
import socket
import subprocess
import sys
import time
from pathlib import Path

ORCHESTRATOR_SOCK = os.path.expanduser("~/.claude/orchestrator/daemon.sock")
POLL_INTERVAL = 0.5  # seconds
IDLE_SHORT_MS = 2000  # 2s - pause/resume sessions
IDLE_LONG_MS = 6 * 3600 * 1000  # 6 hours - relax vault-rag limits
IDLE_ACTIVE_THRESHOLD_MS = 60 * 1000  # 1 minute - restore conservative limits when user returns
SOUND_SCRIPT = os.path.expanduser("~/.claude/hooks/lifecycle-sound.sh")
MANUAL_MODE_LOCK = Path.home() / ".claude/orchestrator/vault-rag-manual-mode.lock"

STATE_IDLE = "idle"
STATE_ACTIVE = "active"
STATE_CONSERVATIVE = "conservative"
STATE_AGGRESSIVE = "aggressive"


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


def notify_desktop(summary: str, body: str):
    """Send desktop notification."""
    try:
        subprocess.run(
            ["notify-send", summary, body],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=2
        )
    except Exception:
        pass


def apply_aggressive_limits():
    """Apply aggressive vault-rag limits when user is sleeping (idle 6h+).

    Uses full system resources to power through indexing backlogs overnight.
    """
    drop_in_dir = Path.home() / ".config/systemd/user/vault-rag-watcher.service.d"
    drop_in_file = drop_in_dir / "runtime.conf"

    drop_in_dir.mkdir(parents=True, exist_ok=True)
    drop_in_file.write_text("""[Service]
Environment=VAULT_RAG_MAX_MEMORY_MB=32768
Environment=VAULT_RAG_MAX_DB_GB=500
Environment=VAULT_RAG_MAX_CHUNKS=11700000
Environment=VAULT_RAG_MAX_DOCS=1170000
MemoryMax=40G
MemoryHigh=32G
CPUQuota=100%
IOWeight=500
""")

    # Reload systemd and restart service
    subprocess.run(["systemctl", "--user", "daemon-reload"], check=False)
    subprocess.run(["systemctl", "--user", "restart", "vault-rag-watcher"], check=False)

    print("[concierge] Applied AGGRESSIVE vault-rag limits (32GB RAM, 100% CPU)", flush=True)
    notify_desktop(
        "Vault-RAG: Aggressive Mode",
        "Full system resources available for indexing"
    )


def apply_conservative_limits():
    """Apply conservative vault-rag limits when user is active.

    Limits background indexing to avoid impacting interactive work.
    """
    drop_in_dir = Path.home() / ".config/systemd/user/vault-rag-watcher.service.d"
    drop_in_file = drop_in_dir / "runtime.conf"

    drop_in_dir.mkdir(parents=True, exist_ok=True)
    drop_in_file.write_text("""[Service]
Environment=VAULT_RAG_MAX_MEMORY_MB=8192
Environment=VAULT_RAG_MAX_DB_GB=100
Environment=VAULT_RAG_MAX_CHUNKS=2340000
Environment=VAULT_RAG_MAX_DOCS=234000
MemoryMax=12G
MemoryHigh=8G
CPUQuota=50%
IOWeight=100
""")

    # Reload systemd and restart service
    subprocess.run(["systemctl", "--user", "daemon-reload"], check=False)
    subprocess.run(["systemctl", "--user", "restart", "vault-rag-watcher"], check=False)

    print("[concierge] Applied CONSERVATIVE vault-rag limits (8GB RAM, 50% CPU)", flush=True)
    notify_desktop(
        "Vault-RAG: Conservative Mode",
        "Resource limits restored for interactive use"
    )


def main():
    # Two independent state machines
    session_state = STATE_IDLE
    vault_rag_state = STATE_CONSERVATIVE

    print(f"Concierge daemon started", flush=True)
    print(f"  Session threshold: {IDLE_SHORT_MS}ms ({IDLE_SHORT_MS/1000}s)", flush=True)
    print(f"  Vault-RAG threshold: {IDLE_LONG_MS}ms ({IDLE_LONG_MS/3600000:.1f}h)", flush=True)
    print(f"  Poll interval: {POLL_INTERVAL}s", flush=True)

    # Apply conservative limits at startup
    apply_conservative_limits()

    while True:
        try:
            idle_ms = get_x11_idle_ms()
            user_active_short = idle_ms < IDLE_SHORT_MS
            user_idle_long = idle_ms >= IDLE_LONG_MS
            user_active_recent = idle_ms < IDLE_ACTIVE_THRESHOLD_MS

            # State machine 1: Session pause/resume (2s threshold)
            if user_active_short and session_state == STATE_IDLE:
                session_state = STATE_ACTIVE
                result = orchestrator_call("pause_all_sessions")
                paused = result.get("paused", 0)
                print(f"[concierge] User active (idle {idle_ms}ms) — paused {paused} sessions")

            elif not user_active_short and session_state == STATE_ACTIVE:
                session_state = STATE_IDLE
                result = orchestrator_call("resume_all_sessions")
                resumed = result.get("resumed", 0)
                print(f"[concierge] User idle ({idle_ms}ms) — resumed {resumed} sessions")

            # State machine 2: Vault-RAG resource limits (6h threshold)
            # Check if manual mode is active (user ran 'vault-rag index')
            manual_mode = MANUAL_MODE_LOCK.exists()

            # Auto-trigger aggressive mode ONLY if not in manual mode
            if user_idle_long and vault_rag_state == STATE_CONSERVATIVE and not manual_mode:
                vault_rag_state = STATE_AGGRESSIVE
                idle_hours = idle_ms / 3600000
                print(f"[concierge] User idle {idle_hours:.1f}h — switching to AGGRESSIVE vault-rag limits")
                apply_aggressive_limits()

            # Always restore conservative when user returns (even in manual mode)
            elif user_active_recent and vault_rag_state == STATE_AGGRESSIVE:
                vault_rag_state = STATE_CONSERVATIVE
                print(f"[concierge] User returned (idle {idle_ms}ms) — restoring CONSERVATIVE vault-rag limits")
                apply_conservative_limits()
                # Clear manual mode lock if it exists (user returned, back to auto)
                if manual_mode:
                    MANUAL_MODE_LOCK.unlink()
                    print(f"[concierge] Manual mode cleared — auto-switching enabled")

            time.sleep(POLL_INTERVAL)

        except KeyboardInterrupt:
            print("Concierge daemon stopped")
            sys.exit(0)
        except Exception as e:
            print(f"Error: {e}", file=sys.stderr)
            time.sleep(POLL_INTERVAL)


if __name__ == "__main__":
    main()
