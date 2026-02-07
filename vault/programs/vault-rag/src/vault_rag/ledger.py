"""Work ledger for tracking tasks, tangents, and loose ends across sessions."""

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any
import jsonschema


# Ledger file path
LEDGER_PATH = Path.home() / "vault" / "WORK_LEDGER.jsonl"

# Schema path
SCHEMA_PATH = Path(__file__).parent / "ledger_schema.json"


def _load_schema() -> Dict[str, Any]:
    """Load JSON schema for validation."""
    with open(SCHEMA_PATH, "r") as f:
        return json.load(f)


def _get_session_id() -> str:
    """Get current Claude Code session ID from environment or generate placeholder."""
    # Try to read from Claude Code environment
    session_id = os.environ.get("CLAUDE_SESSION_ID")
    if not session_id:
        # Fallback: generate based on timestamp (for testing)
        session_id = datetime.utcnow().isoformat().replace(":", "-").replace(".", "-")
    return session_id


def append_event(
    event_type: str,
    event_id: str,
    description: Optional[str] = None,
    context: Optional[Dict[str, Any]] = None,
    trigger: Optional[str] = None,
    parent_task: Optional[str] = None,
    resolution: Optional[str] = None,
    question: Optional[str] = None,
    choice: Optional[str] = None,
    rationale: Optional[str] = None,
    output: Optional[str] = None,
    priority: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Append event to WORK_LEDGER.jsonl with validation.

    Args:
        event_type: One of: task_start, task_complete, tangent_begin, tangent_end,
                   decision, loose_end, question, blocker
        event_id: Unique ID (T1, TG1, D1, LE1, Q1, B1)
        description: Task/blocker description
        context: Context dict (plan, phase, files)
        trigger: Tangent trigger description
        parent_task: Parent task ID for tangent
        resolution: Tangent resolution description
        question: Question text
        choice: Decision choice
        rationale: Decision rationale
        output: Task completion output
        priority: Priority (high, medium, low)

    Returns:
        Dict with event data written

    Raises:
        jsonschema.ValidationError: If event doesn't match schema
        IOError: If write fails
    """
    # Build event dict
    event = {
        "type": event_type,
        "id": event_id,
        "session": _get_session_id(),
        "timestamp": datetime.utcnow().isoformat() + "Z",
    }

    # Add type-specific fields
    if description is not None:
        event["description"] = description
    if context is not None:
        event["context"] = context
    if trigger is not None:
        event["trigger"] = trigger
    if parent_task is not None:
        event["parent_task"] = parent_task
    if resolution is not None:
        event["resolution"] = resolution
    if question is not None:
        event["question"] = question
    if choice is not None:
        event["choice"] = choice
    if rationale is not None:
        event["rationale"] = rationale
    if output is not None:
        event["output"] = output
    if priority is not None:
        event["priority"] = priority

    # Validate against schema
    schema = _load_schema()
    jsonschema.validate(instance=event, schema=schema)

    # Ensure ledger file exists
    LEDGER_PATH.parent.mkdir(parents=True, exist_ok=True)
    LEDGER_PATH.touch(exist_ok=True)

    # Append with fsync (crash-safe)
    with open(LEDGER_PATH, "a") as f:
        json.dump(event, f, separators=(",", ":"))
        f.write("\n")
        f.flush()
        os.fsync(f.fileno())

    return event


def read_events(
    session_ids: Optional[list[str]] = None,
    event_types: Optional[list[str]] = None,
    limit: Optional[int] = None,
) -> list[Dict[str, Any]]:
    """
    Read events from WORK_LEDGER.jsonl with optional filtering.

    Args:
        session_ids: Filter by session IDs (None = all)
        event_types: Filter by event types (None = all)
        limit: Maximum events to return (None = all)

    Returns:
        List of event dicts in chronological order
    """
    if not LEDGER_PATH.exists():
        return []

    events = []
    with open(LEDGER_PATH, "r", encoding="utf-8", errors="replace") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue

            try:
                event = json.loads(line)

                # Apply filters
                if session_ids and event.get("session") not in session_ids:
                    continue
                if event_types and event.get("type") not in event_types:
                    continue

                events.append(event)

                # Check limit
                if limit and len(events) >= limit:
                    break

            except json.JSONDecodeError:
                # Skip malformed lines
                continue

    return events


def get_active_tasks() -> list[Dict[str, Any]]:
    """
    Get all active tasks (started but not completed).

    Returns:
        List of task_start events that don't have matching task_complete
    """
    events = read_events()

    # Build task state
    tasks = {}
    for event in events:
        if event["type"] == "task_start":
            tasks[event["id"]] = event
        elif event["type"] == "task_complete":
            # Remove from active
            tasks.pop(event["id"], None)

    return list(tasks.values())


def get_loose_ends() -> list[Dict[str, Any]]:
    """
    Get all loose ends (unresolved items).

    Returns:
        List of loose_end events
    """
    return read_events(event_types=["loose_end"])


def get_open_questions() -> list[Dict[str, Any]]:
    """
    Get all open questions.

    Returns:
        List of question events
    """
    return read_events(event_types=["question"])


def get_blockers() -> list[Dict[str, Any]]:
    """
    Get all blockers.

    Returns:
        List of blocker events
    """
    return read_events(event_types=["blocker"])
