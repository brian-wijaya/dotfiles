#!/bin/bash
# Play lifecycle event sounds.
# Usage: lifecycle-sound.sh <event>
# Events: unlock
case "$1" in
  unlock)  ~/.claude/hooks/play-sound.sh unlock ;;
  *)       echo "Usage: lifecycle-sound.sh <unlock>" >&2 ;;
esac
