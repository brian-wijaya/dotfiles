#!/usr/bin/env python3
"""Flash sensor HUD for Claude Code notifications."""
import json
import sys
import subprocess

try:
    data = json.load(sys.stdin)
    title = data.get('title', 'Claude')
    message = data.get('message', '')[:50]  # Truncate for HUD

    # Use claude CLI to call the HUD MCP
    # This is a workaround - ideally we'd call MCP directly
    # For now, use notify-send as fallback + a visual cue

    # Flash using the sensor HUD via curl to the MCP socket if available
    # Fallback to notify-send
    subprocess.run([
        'notify-send',
        '-u', 'normal',
        '-t', '3000',
        f'🧠 {title}',
        message
    ], check=False)

    # Sound feedback
    import os
    subprocess.Popen([
        os.path.expanduser('~/.claude/hooks/play-sound.sh'),
        'notification'
    ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

except Exception as e:
    print(f"Notification hook error: {e}", file=sys.stderr)
