#!/usr/bin/env python3
"""Stop hook: auto-save session to vault-rag.

Reads the current session transcript from Claude Code's JSONL log,
extracts a summary, topics, and key facts, then saves to vault-rag.
Also posts a notification to sensor-hud chat overlay.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

# Read hook input from stdin
try:
    hook_input = json.loads(sys.stdin.read())
except Exception:
    hook_input = {}

session_id = hook_input.get("session_id") or os.environ.get("CLAUDE_SESSION_ID", "")
project_dir = hook_input.get("cwd") or os.environ.get("CLAUDE_PROJECT_DIR", os.path.expanduser("~"))

# Find the session JSONL file
projects_dir = Path.home() / ".claude" / "projects"
jsonl_path = None

if session_id:
    # Search all project dirs for the session file
    for pdir in projects_dir.iterdir():
        candidate = pdir / f"{session_id}.jsonl"
        if candidate.exists():
            jsonl_path = candidate
            break

if not jsonl_path or not jsonl_path.exists():
    # Can't find transcript, save a minimal session
    sys.exit(0)

# Parse transcript
user_messages = []
assistant_messages = []
tool_names = set()

try:
    with open(jsonl_path, "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue

            msg_type = entry.get("type", "")
            message = entry.get("message", {})
            if not isinstance(message, dict):
                continue

            role = message.get("role", "")
            content = message.get("content", "")

            if role == "user" and isinstance(content, str):
                user_messages.append(content[:500])
            elif role == "assistant":
                if isinstance(content, str):
                    assistant_messages.append(content[:500])
                elif isinstance(content, list):
                    for block in content:
                        if isinstance(block, dict):
                            if block.get("type") == "text":
                                assistant_messages.append(block.get("text", "")[:500])
                            elif block.get("type") == "tool_use":
                                tool_names.add(block.get("name", ""))
except Exception:
    sys.exit(0)

if not user_messages and not assistant_messages:
    sys.exit(0)

# Extract topics from user messages (simple keyword extraction)
topics = set()
topic_keywords = {
    "emacs": "emacs", "e2e": "e2e-test", "sensor": "sensor",
    "vault": "vault-rag", "dotfile": "dotfiles", "backup": "backup",
    "hud": "sensor-hud", "attention": "sensor-attention",
    "mcp": "mcp", "build": "build", "hook": "hooks",
    "picom": "picom", "i3": "i3", "chrome": "browser",
}
all_text = " ".join(user_messages).lower()
for keyword, topic in topic_keywords.items():
    if keyword in all_text:
        topics.add(topic)

# Build summary from first and last user messages
first_msg = user_messages[0][:200] if user_messages else "No user messages"
last_msg = user_messages[-1][:200] if len(user_messages) > 1 else ""
summary = f"Session with {len(user_messages)} user messages, {len(assistant_messages)} assistant messages. "
summary += f"Started with: {first_msg}"
if last_msg:
    summary += f" — Ended with: {last_msg}"

# Key facts
key_facts = []
if tool_names:
    key_facts.append(f"tools-used: {', '.join(sorted(tool_names)[:15])}")
key_facts.append(f"message-count: {len(user_messages) + len(assistant_messages)}")
key_facts.append(f"user-message-count: {len(user_messages)}")

# Save via vault-rag Python API directly
vault_rag_path = Path.home() / "vault" / "programs" / "vault-rag" / "src"
sys.path.insert(0, str(vault_rag_path))

os.environ.setdefault("VAULT_ROOT", str(Path.home() / "vault"))

try:
    from vault_rag.db.sqlite import VaultDB
    db_path = Path.home() / "vault" / "data" / "vault.db"
    db = VaultDB(str(db_path))
    session_db_id = db.save_session(
        summary=summary,
        topics=sorted(topics) if topics else ["general"],
        key_facts=key_facts,
        word_count=sum(len(m.split()) for m in user_messages + assistant_messages),
        message_count=len(user_messages) + len(assistant_messages)
    )

    # Post to sensor-hud chat overlay via MCP stdin (won't work directly)
    # Instead use notify-send as fallback
    fact_count = len(key_facts)
    topic_count = len(topics)
    subprocess.run(
        ["notify-send", "-u", "low", "-t", "3000",
         "Session Saved",
         f"vault-rag #{session_db_id}: {topic_count} topics, {fact_count} facts"],
        capture_output=True, timeout=5
    )
    subprocess.Popen(
        [os.path.expanduser("~/.claude/hooks/play-sound.sh"), "save-session"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )

except Exception as e:
    # Log failure but don't block session exit
    log_path = Path.home() / ".claude" / "save-session-errors.log"
    with open(log_path, "a") as f:
        f.write(f"{session_id}: {e}\n")
