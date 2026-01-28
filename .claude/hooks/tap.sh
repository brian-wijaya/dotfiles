#!/bin/bash
# Play a random tap variant. Called after each e2e keystroke.
# Also signals the keystroke-sound-daemon that agent is typing.
touch "$HOME/.claude/agent-typing"
SOUNDS=(
  "$HOME/.local/share/sounds/material-design/material_product_sounds/ogg/03 Primary System Sounds/ui_tap-variant-01.ogg"
  "$HOME/.local/share/sounds/material-design/material_product_sounds/ogg/03 Primary System Sounds/ui_tap-variant-02.ogg"
  "$HOME/.local/share/sounds/material-design/material_product_sounds/ogg/03 Primary System Sounds/ui_tap-variant-03.ogg"
  "$HOME/.local/share/sounds/material-design/material_product_sounds/ogg/03 Primary System Sounds/ui_tap-variant-04.ogg"
)
IDX=$(( RANDOM % 4 ))
paplay "${SOUNDS[$IDX]}" 2>/dev/null &
