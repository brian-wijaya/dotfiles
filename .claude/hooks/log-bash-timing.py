#!/usr/bin/env python3
"""Log Bash commands with nanosecond timestamps for sensor correlation."""
import json
import sys
import time
import os

LOG_FILE = os.path.expanduser("~/.claude/bash-timing.jsonl")

try:
    data = json.load(sys.stdin)
    tool_input = data.get('tool_input', {})

    entry = {
        "timestamp_ns": time.time_ns(),
        "command": tool_input.get('command', ''),
        "description": tool_input.get('description', ''),
        "session_id": os.environ.get('CLAUDE_SESSION_ID', 'unknown')
    }

    with open(LOG_FILE, 'a') as f:
        f.write(json.dumps(entry) + '\n')

except Exception as e:
    # Silent fail - don't interrupt Claude
    pass
