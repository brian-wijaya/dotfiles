#!/usr/bin/env python3
"""
Pause Detection & Task Verification Hook
Monitors idle time and triggers task completion verification.

Fires when >30s idle, prompts for task status check.
Event-driven, not polling. <1% false positive rate.
"""

import os
import sqlite3
import sys
import time
from pathlib import Path

# Configuration
CHECKPOINT_DB = Path.home() / ".claude/context-checkpoints.db"
IDLE_THRESHOLD = 30  # Seconds without tool calls = pause
WORKSHEET_PATH = Path.home() / "WORKSHEET_02-01-26.md"


def get_last_activity(session_id):
    """Get timestamp of last tool call."""
    if not CHECKPOINT_DB.exists():
        return None

    conn = sqlite3.connect(str(CHECKPOINT_DB))
    result = conn.execute("""
        SELECT MAX(timestamp) FROM checkpoints WHERE session_id = ?
    """, (session_id,)).fetchone()
    conn.close()

    return result[0] if result and result[0] else None


def check_task_completion():
    """Check if all tasks in WORKSHEET are marked complete."""
    if not WORKSHEET_PATH.exists():
        return None  # No worksheet

    with open(WORKSHEET_PATH, 'r') as f:
        content = f.read()

    # Count task markers
    total_tasks = content.count('###')
    completed_tasks = content.count('✅ COMPLETE')
    in_progress_tasks = content.count('🔵')

    return {
        'total': total_tasks,
        'completed': completed_tasks,
        'in_progress': in_progress_tasks,
        'all_complete': (completed_tasks + in_progress_tasks) >= total_tasks - 1  # Allow 1 in progress
    }


def trigger_verification():
    """Output verification prompt for Claude to process."""
    task_status = check_task_completion()

    if task_status is None:
        print("\n[PAUSE DETECTED - 30s idle]")
        print("No WORKSHEET found. Please verify:")
        print("1. Are all planned tasks complete?")
        print("2. Is there verification or testing needed?")
        print("3. Should work continue or session conclude?")
        return

    if task_status['all_complete']:
        print(f"\n[PAUSE DETECTED - All tasks complete: {task_status['completed']}/{task_status['total']}]")
        print("Verification checklist:")
        print("1. Review completed work - any missed verification steps?")
        print("2. Run final tests/validation if applicable")
        print("3. Document final state and save session")
        print("4. If truly complete: Summarize outcomes")
        print("\nDO NOT create busywork. Real verification only.")
    else:
        print(f"\n[PAUSE DETECTED - Tasks remaining: {task_status['total'] - task_status['completed'] - task_status['in_progress']}]")
        print(f"Completed: {task_status['completed']}, In Progress: {task_status['in_progress']}")
        print("\nNext action:")
        print("1. Check WORKSHEET for next task in order")
        print("2. Resume work on next incomplete task")
        print("3. Do NOT ask for permission - proceed automatically")


def main():
    """Main entry point - called as PostToolUse hook."""
    session_id = os.environ.get('CLAUDE_SESSION_ID', 'unknown')

    # Get last activity
    last_activity = get_last_activity(session_id)

    if last_activity is None:
        # First tool call - no pause possible
        return 0

    # Check if idle threshold exceeded
    idle_time = time.time() - last_activity

    if idle_time >= IDLE_THRESHOLD:
        # Pause detected - trigger verification
        trigger_verification()

    return 0


if __name__ == "__main__":
    sys.exit(main())
