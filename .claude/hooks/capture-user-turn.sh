#!/bin/bash
# UserPromptSubmit hook: capture user prompts as turns in the Chronicle (Stage 14D).
#
# Receives JSON on stdin with: session_id, prompt, cwd, etc.
# Sends the turn to the gateway's MCP endpoint via curl.
# Fire-and-forget — errors are logged but don't block the prompt.

DATA=$(cat)
SESSION_ID=$(echo "$DATA" | jq -r '.session_id // "unknown"')
PROMPT=$(echo "$DATA" | jq -r '.prompt // "" | .[0:500]')

# Skip empty prompts
if [ -z "$PROMPT" ] || [ "$PROMPT" = "null" ]; then
    exit 0
fi

# Escape for SQL: double any single quotes (SQL standard escaping)
sql_escape() { printf '%s' "$1" | sed "s/'/''/g"; }

ESCAPED_PROMPT=$(sql_escape "$PROMPT")
ESCAPED_SESSION=$(sql_escape "$SESSION_ID")

VAULT_DB="$HOME/vault/data/vault.db"

if [ -f "$VAULT_DB" ]; then
    TURN_ID=$(uuidgen 2>/dev/null || python3 -c 'import uuid; print(uuid.uuid4())')
    TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%S.000Z")

    sqlite3 "$VAULT_DB" "INSERT OR IGNORE INTO context_turns (turn_id, session_id, pass_number, role, content, timestamp) VALUES ('$TURN_ID', '$ESCAPED_SESSION', 1, 'user', '$ESCAPED_PROMPT', '$TIMESTAMP');" 2>/dev/null
fi

exit 0
