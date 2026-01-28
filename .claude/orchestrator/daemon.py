#!/usr/bin/env python3
"""
Claude Orchestrator Daemon

Multi-session coordinator with task queue, resource arbitration,
stagnation detection, and local LLM integration.

Usage:
    python daemon.py [--socket PATH] [--db PATH]
"""

import asyncio
import hashlib
import json
import logging
import os
import signal
import sqlite3
import subprocess
import sys
import time
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Optional

# ============================================================================
# Configuration
# ============================================================================

DEFAULT_SOCKET = Path.home() / ".claude" / "orchestrator" / "daemon.sock"
DEFAULT_DB = Path.home() / ".claude" / "orchestrator" / "state.db"
DEFAULT_LOG = Path.home() / ".claude" / "orchestrator" / "daemon.log"

HEARTBEAT_TIMEOUT_SEC = 60
STAGNATION_THRESHOLD = 3  # identical failure sets to trigger circuit breaker
MAX_SESSIONS = 20  # Claude Max limit

# ============================================================================
# Enums and Data Classes
# ============================================================================

class SessionState(Enum):
    IDLE = "idle"
    WORKING = "working"
    BLOCKED = "blocked"
    STAGNATED = "stagnated"
    CRASHED = "crashed"
    PAUSED = "paused"
    E2E_RUNNING = "e2e_running"

class TaskState(Enum):
    PENDING = "pending"
    CLAIMED = "claimed"
    COMPLETED = "completed"
    FAILED = "failed"
    STAGNATED = "stagnated"

class TaskType(Enum):
    ONE_SHOT = "one_shot"
    RECURRING = "recurring"
    PERPETUAL = "perpetual"

class ResourceType(Enum):
    SCREEN_FOCUS = "screen_focus"
    FILE_LOCK = "file_lock"
    GPU_SLOT = "gpu_slot"

@dataclass
class Session:
    id: str
    pid: Optional[int] = None
    state: SessionState = SessionState.IDLE
    current_task_id: Optional[str] = None
    started_at: Optional[str] = None
    last_heartbeat: Optional[str] = None
    capabilities: list = field(default_factory=list)

@dataclass
class Task:
    id: str
    type: TaskType
    priority: int
    definition: dict
    state: TaskState = TaskState.PENDING
    assigned_session: Optional[str] = None
    created_at: Optional[str] = None
    completed_at: Optional[str] = None
    required_capabilities: list = field(default_factory=list)

@dataclass
class Iteration:
    session_id: str
    task_id: str
    attempt: int
    result: str  # pass, fail, blocked
    failure_hash: Optional[str] = None
    duration_ms: int = 0
    timestamp: Optional[str] = None

# ============================================================================
# Database
# ============================================================================

SCHEMA = """
CREATE TABLE IF NOT EXISTS sessions (
    id TEXT PRIMARY KEY,
    pid INTEGER,
    state TEXT NOT NULL DEFAULT 'idle',
    current_task_id TEXT,
    started_at TEXT,
    last_heartbeat TEXT,
    capabilities TEXT DEFAULT '[]'
);

CREATE TABLE IF NOT EXISTS tasks (
    id TEXT PRIMARY KEY,
    type TEXT NOT NULL,
    priority INTEGER NOT NULL DEFAULT 0,
    definition TEXT NOT NULL,
    state TEXT NOT NULL DEFAULT 'pending',
    assigned_session TEXT,
    created_at TEXT,
    completed_at TEXT,
    required_capabilities TEXT DEFAULT '[]'
);

CREATE TABLE IF NOT EXISTS iterations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    session_id TEXT NOT NULL,
    task_id TEXT NOT NULL,
    attempt INTEGER NOT NULL,
    result TEXT NOT NULL,
    failure_hash TEXT,
    duration_ms INTEGER DEFAULT 0,
    timestamp TEXT
);

CREATE TABLE IF NOT EXISTS resources (
    name TEXT PRIMARY KEY,
    type TEXT NOT NULL,
    holder_session TEXT,
    acquired_at TEXT
);

CREATE INDEX IF NOT EXISTS idx_tasks_state ON tasks(state);
CREATE INDEX IF NOT EXISTS idx_tasks_priority ON tasks(priority DESC);
CREATE INDEX IF NOT EXISTS idx_iterations_task ON iterations(task_id);
"""

class Database:
    def __init__(self, path: Path):
        self.path = path
        self.conn = sqlite3.connect(str(path), check_same_thread=False)
        self.conn.row_factory = sqlite3.Row
        self._init_schema()

    def _init_schema(self):
        self.conn.executescript(SCHEMA)
        self.conn.commit()

    @contextmanager
    def transaction(self):
        try:
            yield
            self.conn.commit()
        except Exception:
            self.conn.rollback()
            raise

    def now(self) -> str:
        return datetime.utcnow().isoformat() + "Z"

    # Sessions
    def upsert_session(self, session: Session):
        with self.transaction():
            self.conn.execute("""
                INSERT INTO sessions (id, pid, state, current_task_id, started_at, last_heartbeat, capabilities)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(id) DO UPDATE SET
                    pid = excluded.pid,
                    state = excluded.state,
                    current_task_id = excluded.current_task_id,
                    last_heartbeat = excluded.last_heartbeat,
                    capabilities = excluded.capabilities
            """, (session.id, session.pid, session.state.value, session.current_task_id,
                  session.started_at, session.last_heartbeat, json.dumps(session.capabilities)))

    def get_session(self, session_id: str) -> Optional[Session]:
        row = self.conn.execute("SELECT * FROM sessions WHERE id = ?", (session_id,)).fetchone()
        if row:
            return Session(
                id=row["id"],
                pid=row["pid"],
                state=SessionState(row["state"]),
                current_task_id=row["current_task_id"],
                started_at=row["started_at"],
                last_heartbeat=row["last_heartbeat"],
                capabilities=json.loads(row["capabilities"] or "[]")
            )
        return None

    def get_all_sessions(self) -> list[Session]:
        rows = self.conn.execute("SELECT * FROM sessions").fetchall()
        return [Session(
            id=r["id"], pid=r["pid"], state=SessionState(r["state"]),
            current_task_id=r["current_task_id"], started_at=r["started_at"],
            last_heartbeat=r["last_heartbeat"],
            capabilities=json.loads(r["capabilities"] or "[]")
        ) for r in rows]

    def delete_session(self, session_id: str):
        with self.transaction():
            self.conn.execute("DELETE FROM sessions WHERE id = ?", (session_id,))

    # Tasks
    def insert_task(self, task: Task):
        with self.transaction():
            self.conn.execute("""
                INSERT INTO tasks (id, type, priority, definition, state, assigned_session, created_at, required_capabilities)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """, (task.id, task.type.value, task.priority, json.dumps(task.definition),
                  task.state.value, task.assigned_session, task.created_at or self.now(),
                  json.dumps(task.required_capabilities)))

    def update_task(self, task: Task):
        with self.transaction():
            self.conn.execute("""
                UPDATE tasks SET state = ?, assigned_session = ?, completed_at = ?
                WHERE id = ?
            """, (task.state.value, task.assigned_session, task.completed_at, task.id))

    def get_task(self, task_id: str) -> Optional[Task]:
        row = self.conn.execute("SELECT * FROM tasks WHERE id = ?", (task_id,)).fetchone()
        if row:
            return Task(
                id=row["id"],
                type=TaskType(row["type"]),
                priority=row["priority"],
                definition=json.loads(row["definition"]),
                state=TaskState(row["state"]),
                assigned_session=row["assigned_session"],
                created_at=row["created_at"],
                completed_at=row["completed_at"],
                required_capabilities=json.loads(row["required_capabilities"] or "[]")
            )
        return None

    def get_pending_tasks(self) -> list[Task]:
        rows = self.conn.execute(
            "SELECT * FROM tasks WHERE state = 'pending' ORDER BY priority DESC"
        ).fetchall()
        return [Task(
            id=r["id"], type=TaskType(r["type"]), priority=r["priority"],
            definition=json.loads(r["definition"]), state=TaskState(r["state"]),
            assigned_session=r["assigned_session"], created_at=r["created_at"],
            completed_at=r["completed_at"],
            required_capabilities=json.loads(r["required_capabilities"] or "[]")
        ) for r in rows]

    # Iterations
    def insert_iteration(self, it: Iteration):
        with self.transaction():
            self.conn.execute("""
                INSERT INTO iterations (session_id, task_id, attempt, result, failure_hash, duration_ms, timestamp)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (it.session_id, it.task_id, it.attempt, it.result, it.failure_hash,
                  it.duration_ms, it.timestamp or self.now()))

    def get_iterations_for_task(self, task_id: str) -> list[Iteration]:
        rows = self.conn.execute(
            "SELECT * FROM iterations WHERE task_id = ? ORDER BY attempt", (task_id,)
        ).fetchall()
        return [Iteration(
            session_id=r["session_id"], task_id=r["task_id"], attempt=r["attempt"],
            result=r["result"], failure_hash=r["failure_hash"],
            duration_ms=r["duration_ms"], timestamp=r["timestamp"]
        ) for r in rows]

    def get_recent_failure_hashes(self, task_id: str, limit: int = 3) -> list[str]:
        rows = self.conn.execute("""
            SELECT failure_hash FROM iterations
            WHERE task_id = ? AND result = 'fail' AND failure_hash IS NOT NULL
            ORDER BY id DESC LIMIT ?
        """, (task_id, limit)).fetchall()
        return [r["failure_hash"] for r in rows]

    # Resources
    def acquire_resource(self, name: str, resource_type: ResourceType, session_id: str) -> bool:
        try:
            with self.transaction():
                self.conn.execute("""
                    INSERT INTO resources (name, type, holder_session, acquired_at)
                    VALUES (?, ?, ?, ?)
                """, (name, resource_type.value, session_id, self.now()))
            return True
        except sqlite3.IntegrityError:
            return False

    def release_resource(self, name: str, session_id: str) -> bool:
        with self.transaction():
            cursor = self.conn.execute(
                "DELETE FROM resources WHERE name = ? AND holder_session = ?",
                (name, session_id)
            )
            return cursor.rowcount > 0

    def release_all_resources(self, session_id: str):
        with self.transaction():
            self.conn.execute("DELETE FROM resources WHERE holder_session = ?", (session_id,))

    def get_resource_holder(self, name: str) -> Optional[str]:
        row = self.conn.execute(
            "SELECT holder_session FROM resources WHERE name = ?", (name,)
        ).fetchone()
        return row["holder_session"] if row else None

    # Stats
    def get_stats(self) -> dict:
        sessions = self.conn.execute("SELECT state, COUNT(*) as c FROM sessions GROUP BY state").fetchall()
        tasks = self.conn.execute("SELECT state, COUNT(*) as c FROM tasks GROUP BY state").fetchall()
        resources = self.conn.execute("SELECT COUNT(*) as c FROM resources").fetchone()
        return {
            "sessions": {r["state"]: r["c"] for r in sessions},
            "tasks": {r["state"]: r["c"] for r in tasks},
            "resources_held": resources["c"]
        }

# ============================================================================
# Orchestrator
# ============================================================================

class Orchestrator:
    def __init__(self, db: Database):
        self.db = db
        self.logger = logging.getLogger("orchestrator")
        self._shutdown = False

    def register_session(self, session_id: str, capabilities: list) -> dict:
        """Register a new session with the orchestrator."""
        session = Session(
            id=session_id,
            state=SessionState.IDLE,
            started_at=self.db.now(),
            last_heartbeat=self.db.now(),
            capabilities=capabilities
        )
        self.db.upsert_session(session)
        self.logger.info(f"Session registered: {session_id} with capabilities {capabilities}")
        return {"session_id": session_id, "status": "registered"}

    def heartbeat(self, session_id: str) -> dict:
        """Update session heartbeat and return pending task count."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}

        session.last_heartbeat = self.db.now()
        self.db.upsert_session(session)

        # Count tasks this session could claim
        pending = self.db.get_pending_tasks()
        claimable = sum(1 for t in pending
                       if not t.required_capabilities or
                       all(c in session.capabilities for c in t.required_capabilities))

        return {"session_id": session_id, "pending_tasks": claimable}

    def unregister_session(self, session_id: str) -> dict:
        """Unregister a session and release its resources."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}

        # Release any held resources
        self.db.release_all_resources(session_id)

        # Fail any claimed task
        if session.current_task_id:
            task = self.db.get_task(session.current_task_id)
            if task and task.state == TaskState.CLAIMED:
                task.state = TaskState.PENDING
                task.assigned_session = None
                self.db.update_task(task)

        self.db.delete_session(session_id)
        self.logger.info(f"Session unregistered: {session_id}")
        return {"session_id": session_id, "status": "unregistered"}

    def submit_task(self, submitter_id: str, task_type: str,
                    required_capabilities: list, payload: dict,
                    priority: int = 0, timeout_minutes: int = 60) -> dict:
        """Submit a new task to the queue."""
        import uuid
        task_id = str(uuid.uuid4())[:8]

        task = Task(
            id=task_id,
            type=TaskType(task_type) if task_type in [t.value for t in TaskType] else TaskType.ONE_SHOT,
            priority=priority,
            definition={
                "submitter": submitter_id,
                "payload": payload,
                "timeout_minutes": timeout_minutes
            },
            required_capabilities=required_capabilities,
            created_at=self.db.now()
        )
        self.db.insert_task(task)
        self.logger.info(f"Task submitted: {task_id} by {submitter_id}")
        return {"task_id": task_id, "status": "pending"}

    def claim_task(self, session_id: str) -> Optional[dict]:
        """Claim the highest priority task matching session capabilities."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}

        if session.state in (SessionState.WORKING, SessionState.PAUSED, SessionState.E2E_RUNNING):
            return {"error": f"session cannot claim tasks in state {session.state.value}"}

        pending = self.db.get_pending_tasks()
        for task in pending:
            # Check capability match
            if task.required_capabilities:
                if not all(c in session.capabilities for c in task.required_capabilities):
                    continue

            # Claim it
            task.state = TaskState.CLAIMED
            task.assigned_session = session_id
            self.db.update_task(task)

            session.state = SessionState.WORKING
            session.current_task_id = task.id
            self.db.upsert_session(session)

            self.logger.info(f"Task {task.id} claimed by session {session_id}")
            return {
                "task_id": task.id,
                "type": task.type.value,
                "payload": task.definition.get("payload", {})
            }

        return None  # No matching tasks

    def complete_task(self, session_id: str, task_id: str, result: dict) -> dict:
        """Mark a task as completed."""
        task = self.db.get_task(task_id)
        if not task:
            return {"error": "task not found"}
        if task.assigned_session != session_id:
            return {"error": "task not assigned to this session"}

        task.state = TaskState.COMPLETED
        task.completed_at = self.db.now()
        self.db.update_task(task)

        # Record iteration
        iterations = self.db.get_iterations_for_task(task_id)
        self.db.insert_iteration(Iteration(
            session_id=session_id,
            task_id=task_id,
            attempt=len(iterations) + 1,
            result="pass"
        ))

        # Update session
        session = self.db.get_session(session_id)
        if session:
            session.state = SessionState.IDLE
            session.current_task_id = None
            self.db.upsert_session(session)

        self.logger.info(f"Task {task_id} completed by session {session_id}")
        return {"task_id": task_id, "status": "completed"}

    def fail_task(self, session_id: str, task_id: str, error: str) -> dict:
        """Mark a task as failed and check for stagnation."""
        task = self.db.get_task(task_id)
        if not task:
            return {"error": "task not found"}
        if task.assigned_session != session_id:
            return {"error": "task not assigned to this session"}

        # Compute failure hash for stagnation detection
        failure_hash = hashlib.md5(error.encode()).hexdigest()[:16]

        # Record iteration
        iterations = self.db.get_iterations_for_task(task_id)
        self.db.insert_iteration(Iteration(
            session_id=session_id,
            task_id=task_id,
            attempt=len(iterations) + 1,
            result="fail",
            failure_hash=failure_hash
        ))

        # Check for stagnation
        recent_hashes = self.db.get_recent_failure_hashes(task_id, STAGNATION_THRESHOLD)
        if len(recent_hashes) >= STAGNATION_THRESHOLD and len(set(recent_hashes)) == 1:
            task.state = TaskState.STAGNATED
            self.logger.warning(f"Task {task_id} stagnated after {STAGNATION_THRESHOLD} identical failures")
        else:
            # Return to pending for retry
            task.state = TaskState.PENDING
            task.assigned_session = None

        self.db.update_task(task)

        # Update session
        session = self.db.get_session(session_id)
        if session:
            session.state = SessionState.IDLE
            session.current_task_id = None
            self.db.upsert_session(session)

        self.logger.info(f"Task {task_id} failed by session {session_id}: {error[:100]}")
        return {
            "task_id": task_id,
            "status": "stagnated" if task.state == TaskState.STAGNATED else "pending"
        }

    def get_task_status(self, task_id: str) -> dict:
        """Get current status of a task."""
        task = self.db.get_task(task_id)
        if not task:
            return {"error": "task not found"}

        iterations = self.db.get_iterations_for_task(task_id)
        return {
            "task_id": task_id,
            "state": task.state.value,
            "type": task.type.value,
            "priority": task.priority,
            "assigned_session": task.assigned_session,
            "attempts": len(iterations),
            "created_at": task.created_at,
            "completed_at": task.completed_at
        }

    def get_status(self) -> dict:
        """Get overall orchestrator status."""
        stats = self.db.get_stats()
        sessions = self.db.get_all_sessions()
        return {
            "stats": stats,
            "sessions": [
                {
                    "id": s.id,
                    "state": s.state.value,
                    "current_task": s.current_task_id,
                    "capabilities": s.capabilities
                }
                for s in sessions
            ]
        }

    def should_session_continue(self, session_id: str) -> dict:
        """Called by Stop hook to decide if session should continue."""
        session = self.db.get_session(session_id)
        if not session:
            return {"decision": "allow", "reason": "unknown session"}

        # Check if there's a current task that needs retry
        if session.current_task_id:
            task = self.db.get_task(session.current_task_id)
            if task and task.state == TaskState.CLAIMED:
                # Check stagnation
                recent_hashes = self.db.get_recent_failure_hashes(task.id, STAGNATION_THRESHOLD)
                if len(recent_hashes) >= STAGNATION_THRESHOLD - 1:
                    if len(set(recent_hashes)) == 1:
                        return {"decision": "allow", "reason": "stagnation detected, allow exit"}

                # Task still in progress, block exit
                return {"decision": "block", "reason": f"task {task.id} in progress"}

        # Check if there are pending tasks this session could handle
        pending = self.db.get_pending_tasks()
        claimable = [t for t in pending
                    if not t.required_capabilities or
                    all(c in session.capabilities for c in t.required_capabilities)]

        if claimable:
            return {
                "decision": "block",
                "reason": f"{len(claimable)} pending tasks available",
                "next_task": claimable[0].id if claimable else None
            }

        return {"decision": "allow", "reason": "no pending work"}

    # ------------------------------------------------------------------
    # Concierge / lifecycle methods
    # ------------------------------------------------------------------

    def pause_session(self, session_id: str) -> dict:
        """Pause a WORKING session (preserves current_task_id)."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}
        if session.state != SessionState.WORKING:
            return {"error": f"cannot pause session in state {session.state.value}"}
        session.state = SessionState.PAUSED
        self.db.upsert_session(session)
        self.logger.info(f"Session {session_id} paused")
        return {"session_id": session_id, "state": "paused"}

    def resume_session(self, session_id: str) -> dict:
        """Resume a PAUSED session back to WORKING."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}
        if session.state != SessionState.PAUSED:
            return {"error": f"cannot resume session in state {session.state.value}"}
        session.state = SessionState.WORKING
        self.db.upsert_session(session)
        self.logger.info(f"Session {session_id} resumed")
        return {"session_id": session_id, "state": "working"}

    def pause_all_sessions(self) -> dict:
        """Pause all WORKING sessions (e.g., on screen lock)."""
        sessions = self.db.get_all_sessions()
        paused = 0
        for s in sessions:
            if s.state == SessionState.WORKING:
                s.state = SessionState.PAUSED
                self.db.upsert_session(s)
                paused += 1
        self.logger.info(f"Paused {paused} sessions")
        return {"paused": paused}

    def resume_all_sessions(self) -> dict:
        """Resume all PAUSED sessions (e.g., on screen unlock)."""
        sessions = self.db.get_all_sessions()
        resumed = 0
        for s in sessions:
            if s.state == SessionState.PAUSED:
                s.state = SessionState.WORKING
                self.db.upsert_session(s)
                resumed += 1
        self.logger.info(f"Resumed {resumed} sessions")
        return {"resumed": resumed}

    def set_session_state(self, session_id: str, state: str) -> dict:
        """Manually set session state (for e2e exemption, etc.)."""
        session = self.db.get_session(session_id)
        if not session:
            return {"error": "session not found"}
        try:
            session.state = SessionState(state)
        except ValueError:
            return {"error": f"invalid state: {state}"}
        self.db.upsert_session(session)
        self.logger.info(f"Session {session_id} state set to {state}")
        return {"session_id": session_id, "state": state}

# ============================================================================
# Server
# ============================================================================

class Server:
    def __init__(self, orchestrator: Orchestrator, socket_path: Path):
        self.orchestrator = orchestrator
        self.socket_path = socket_path
        self.logger = logging.getLogger("server")
        self._server = None

    async def handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        try:
            data = await asyncio.wait_for(reader.read(65536), timeout=5.0)
            if not data:
                return

            request = json.loads(data.decode())
            method = request.get("method", "")
            params = request.get("params", {})

            # Route to orchestrator methods
            result = self._dispatch(method, params)

            response = json.dumps({"result": result})
            writer.write(response.encode())
            await writer.drain()
        except asyncio.TimeoutError:
            self.logger.warning("Client timeout")
        except json.JSONDecodeError:
            self.logger.warning("Invalid JSON from client")
        except Exception as e:
            self.logger.error(f"Error handling client: {e}")
            try:
                writer.write(json.dumps({"error": str(e)}).encode())
                await writer.drain()
            except Exception:
                pass
        finally:
            writer.close()
            try:
                await writer.wait_closed()
            except Exception:
                pass

    def _dispatch(self, method: str, params: dict) -> Any:
        methods = {
            "register_session": lambda p: self.orchestrator.register_session(
                p["session_id"], p.get("capabilities", [])
            ),
            "heartbeat": lambda p: self.orchestrator.heartbeat(p["session_id"]),
            "unregister_session": lambda p: self.orchestrator.unregister_session(p["session_id"]),
            "submit_task": lambda p: self.orchestrator.submit_task(
                p["submitter_id"], p.get("type", "one_shot"),
                p.get("required_capabilities", []), p.get("payload", {}),
                p.get("priority", 0), p.get("timeout_minutes", 60)
            ),
            "claim_task": lambda p: self.orchestrator.claim_task(p["session_id"]),
            "complete_task": lambda p: self.orchestrator.complete_task(
                p["session_id"], p["task_id"], p.get("result", {})
            ),
            "fail_task": lambda p: self.orchestrator.fail_task(
                p["session_id"], p["task_id"], p.get("error", "unknown")
            ),
            "get_task": lambda p: self.orchestrator.get_task_status(p["task_id"]),
            "get_status": lambda _: self.orchestrator.get_status(),
            "should_continue": lambda p: self.orchestrator.should_session_continue(p["session_id"]),
            "pause_session": lambda p: self.orchestrator.pause_session(p["session_id"]),
            "resume_session": lambda p: self.orchestrator.resume_session(p["session_id"]),
            "pause_all_sessions": lambda _: self.orchestrator.pause_all_sessions(),
            "resume_all_sessions": lambda _: self.orchestrator.resume_all_sessions(),
            "set_session_state": lambda p: self.orchestrator.set_session_state(p["session_id"], p["state"]),
        }

        if method not in methods:
            return {"error": f"unknown method: {method}"}

        return methods[method](params)

    async def start(self):
        # Remove stale socket
        if self.socket_path.exists():
            self.socket_path.unlink()

        self._server = await asyncio.start_unix_server(
            self.handle_client, path=str(self.socket_path)
        )
        self.socket_path.chmod(0o600)
        self.logger.info(f"Server listening on {self.socket_path}")

        async with self._server:
            await self._server.serve_forever()

    async def stop(self):
        if self._server:
            self._server.close()
            await self._server.wait_closed()
        if self.socket_path.exists():
            self.socket_path.unlink()

# ============================================================================
# Main
# ============================================================================

def setup_logging(log_path: Path):
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
        handlers=[
            logging.FileHandler(log_path),
            logging.StreamHandler()
        ]
    )

async def main():
    import argparse
    parser = argparse.ArgumentParser(description="Claude Orchestrator Daemon")
    parser.add_argument("--socket", type=Path, default=DEFAULT_SOCKET)
    parser.add_argument("--db", type=Path, default=DEFAULT_DB)
    parser.add_argument("--log", type=Path, default=DEFAULT_LOG)
    args = parser.parse_args()

    # Ensure directories exist
    args.socket.parent.mkdir(parents=True, exist_ok=True)
    args.db.parent.mkdir(parents=True, exist_ok=True)
    args.log.parent.mkdir(parents=True, exist_ok=True)

    setup_logging(args.log)
    logger = logging.getLogger("main")

    db = Database(args.db)
    orchestrator = Orchestrator(db)
    server = Server(orchestrator, args.socket)

    # Handle shutdown
    loop = asyncio.get_event_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, lambda: asyncio.create_task(shutdown(server, logger)))

    logger.info("Starting orchestrator daemon...")
    try:
        await server.start()
    except asyncio.CancelledError:
        pass
    finally:
        await server.stop()
        logger.info("Daemon stopped")

async def shutdown(server: Server, logger: logging.Logger):
    logger.info("Shutting down...")
    await server.stop()
    tasks = [t for t in asyncio.all_tasks() if t is not asyncio.current_task()]
    for task in tasks:
        task.cancel()

if __name__ == "__main__":
    asyncio.run(main())
