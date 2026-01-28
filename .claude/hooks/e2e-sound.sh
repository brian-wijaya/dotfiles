#!/bin/bash
# Play e2e outcome sounds. Called by e2e orchestrator.
# Usage: e2e-sound.sh <pass|fail|blocked|stagnated>
case "$1" in
  pass)      ~/.claude/hooks/play-sound.sh e2e-pass ;;
  fail)      ~/.claude/hooks/play-sound.sh e2e-fail ;;
  blocked)   ~/.claude/hooks/play-sound.sh e2e-blocked ;;
  stagnated) ~/.claude/hooks/play-sound.sh e2e-stagnated ;;
  *)         echo "Usage: e2e-sound.sh <pass|fail|blocked|stagnated>" >&2 ;;
esac
