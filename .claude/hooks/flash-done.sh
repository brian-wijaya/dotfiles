#!/bin/bash
# Flash + sound when Claude finishes responding

# Visual notification
notify-send -u normal -t 1000 "🧠 DONE" "Claude finished" 2>/dev/null || true

# Sound effect (async so it doesn't block)
~/.claude/hooks/play-sound.sh session-stop &

# Log
echo "$(date +%s.%N) STOP" >> ~/.claude/action-flash.log
