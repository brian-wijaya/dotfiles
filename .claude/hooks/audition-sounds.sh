#!/bin/bash
# Audition all mapped sounds with spoken numbers
# Usage: audition-sounds.sh [start-from-number]

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
START="${1:-1}"

EVENTS=(
  session-start session-stop tool-complete commit
  e2e-pass e2e-fail e2e-blocked save-session
  error user-prompt hook-fire build-success
  build-fail file-write search-complete hud-post
  concierge-pause concierge-resume notification unlock
  e2e-stagnated
)

echo "Auditioning ${#EVENTS[@]} sounds (starting from $START)..."
echo "Note the number — tell Claude 'change 7 to something softer' etc."
echo ""

for i in "${!EVENTS[@]}"; do
  NUM=$((i + 1))
  if (( NUM < START )); then continue; fi
  EVENT="${EVENTS[$i]}"
  echo "  $NUM. $EVENT"
  "$SCRIPT_DIR/play-sound.sh" "$EVENT" --audition
  sleep 1.2
done

echo ""
echo "Done. Provide feedback by number."
