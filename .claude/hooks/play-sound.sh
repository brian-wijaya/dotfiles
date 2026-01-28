#!/bin/bash
# Play Material Design sound by event name
# Usage: play-sound.sh <event-name> [--audition]
# Sounds: ~/.local/share/sounds/material-design/

SOUND_BASE="$HOME/.local/share/sounds/material-design/material_product_sounds/ogg"
HERO="$SOUND_BASE/01 Hero Sounds"
ALERTS="$SOUND_BASE/02 Alerts and Notifications"
PRIMARY="$SOUND_BASE/03 Primary System Sounds"
SECONDARY="$SOUND_BASE/04 Secondary System Sounds"

declare -A SOUNDS=(
  # Event                 Sound file path
  [session-start]="$HERO/hero_simple-celebration-01.ogg"
  [session-stop]="$PRIMARY/state-change_confirm-down.ogg"
  [tool-complete]="$PRIMARY/ui_tap-variant-01.ogg"
  [commit]="$HERO/hero_simple-celebration-02.ogg"
  [e2e-pass]="$ALERTS/notification_simple-01.ogg"
  [e2e-fail]="$ALERTS/alert_high-intensity.ogg"
  [e2e-blocked]="$ALERTS/notification_ambient.ogg"
  [save-session]="$PRIMARY/state-change_confirm-up.ogg"
  [error]="$SECONDARY/alert_error-01.ogg"
  [user-prompt]="$PRIMARY/navigation_hover-tap.ogg"
  [hook-fire]="$PRIMARY/ui_tap-variant-02.ogg"
  [build-success]="$ALERTS/notification_simple-02.ogg"
  [build-fail]="$SECONDARY/alert_error-02.ogg"
  [file-write]="$PRIMARY/ui_tap-variant-03.ogg"
  [search-complete]="$PRIMARY/navigation_forward-selection-minimal.ogg"
  [hud-post]="$PRIMARY/ui_tap-variant-04.ogg"
  [concierge-pause]="$SECONDARY/navigation_transition-left.ogg"
  [concierge-resume]="$SECONDARY/navigation_transition-right.ogg"
  [notification]="$ALERTS/notification_decorative-01.ogg"
  [unlock]="$PRIMARY/ui_unlock.ogg"
  [e2e-stagnated]="$SECONDARY/alert_error-03.ogg"
)

EVENT="$1"
AUDITION="$2"

if [[ -z "$EVENT" ]]; then
  echo "Usage: play-sound.sh <event-name> [--audition]"
  echo "Events: ${!SOUNDS[*]}"
  exit 1
fi

FILE="${SOUNDS[$EVENT]}"
if [[ -z "$FILE" || ! -f "$FILE" ]]; then
  exit 0  # Unknown event, silent
fi

if [[ "$AUDITION" == "--audition" ]]; then
  # Find the event number for audition
  declare -A NUMBERS=(
    [session-start]=1 [session-stop]=2 [tool-complete]=3 [commit]=4
    [e2e-pass]=5 [e2e-fail]=6 [e2e-blocked]=7 [save-session]=8
    [error]=9 [user-prompt]=10 [hook-fire]=11 [build-success]=12
    [build-fail]=13 [file-write]=14 [search-complete]=15 [hud-post]=16
    [concierge-pause]=17 [concierge-resume]=18 [notification]=19 [unlock]=20
    [e2e-stagnated]=21
  )
  NUM="${NUMBERS[$EVENT]}"
  espeak-ng "$NUM" --stdout 2>/dev/null | paplay 2>/dev/null
  sleep 0.3
fi

paplay "$FILE" 2>/dev/null
