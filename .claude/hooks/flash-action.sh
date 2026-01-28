#!/bin/bash
# Flash HUD for every tool action via somatic MCP
# Called by PostToolUse hooks

DATA=$(cat)
TOOL=$(echo "$DATA" | jq -r '.tool_name // "unknown"')
RESULT=$(echo "$DATA" | jq -r '.tool_result // "" | tostring | .[0:30]')

# Map tools to colors and short names
case "$TOOL" in
  Edit)   COLOR="#FFD700"; LABEL="EDIT" ;;
  Write)  COLOR="#00FF00"; LABEL="WRITE" ;;
  Bash)   COLOR="#00FFFF"; LABEL="BASH" ;;
  Read)   COLOR="#888888"; LABEL="READ" ;;
  Grep)   COLOR="#FF00FF"; LABEL="GREP" ;;
  Glob)   COLOR="#FF00FF"; LABEL="GLOB" ;;
  *)      COLOR="#FFFFFF"; LABEL="$TOOL" ;;
esac

# Get file path if present
FILE=$(echo "$DATA" | jq -r '.tool_input.file_path // .tool_input.pattern // .tool_input.command // "" | split("/") | .[-1] | .[0:20]')

# Write to a FIFO that a background process can read and send to HUD
# For now, log to file and use notify-send as visible feedback
echo "$(date +%s.%N) $LABEL $FILE" >> ~/.claude/action-flash.log

# Quick visual flash via notify-send (low urgency, short timeout)
notify-send -u low -t 500 "$LABEL" "$FILE" 2>/dev/null || true

# Detect errors
IS_ERROR=$(echo "$DATA" | jq -r '.tool_result // "" | tostring | test("(?i)error|failed|exception|traceback")' 2>/dev/null)

# Sound feedback per tool type (async)
if [ "$IS_ERROR" = "true" ]; then
  ~/.claude/hooks/play-sound.sh error &
else
  case "$TOOL" in
    Edit|Write) ~/.claude/hooks/play-sound.sh file-write & ;;
    Grep|Glob)  ~/.claude/hooks/play-sound.sh search-complete & ;;
    *)          ~/.claude/hooks/play-sound.sh tool-complete & ;;
  esac
fi
