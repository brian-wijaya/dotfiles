#!/bin/bash
# Play lifecycle event sounds.
# Usage: lifecycle-sound.sh <event>
# Events: concierge-pause, concierge-resume, unlock
case "$1" in
  concierge-pause)  ~/.claude/hooks/play-sound.sh concierge-pause ;;
  concierge-resume) ~/.claude/hooks/play-sound.sh concierge-resume ;;
  unlock)           ~/.claude/hooks/play-sound.sh unlock ;;
  *)                echo "Usage: lifecycle-sound.sh <concierge-pause|concierge-resume|unlock>" >&2 ;;
esac
