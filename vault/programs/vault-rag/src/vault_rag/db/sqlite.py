"""SQLite database operations with FTS5 support."""

import sqlite3
import json
from pathlib import Path
from datetime import datetime
from typing import Optional, Any
from dataclasses import dataclass


SCHEMA = """
-- Enable WAL mode for better concurrency
PRAGMA journal_mode = WAL;
PRAGMA foreign_keys = ON;

-- Core documents table
CREATE TABLE IF NOT EXISTS documents (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_type TEXT NOT NULL,          -- 'org', 'chatgpt', 'onenote'
    source_path TEXT NOT NULL UNIQUE,   -- Relative path from vault root
    title TEXT,
    created_at TEXT,
    updated_at TEXT,
    indexed_at TEXT DEFAULT (datetime('now')),
    content_hash TEXT NOT NULL,
    word_count INTEGER,
    metadata TEXT                       -- JSON metadata
);

-- Chunks for embedding
CREATE TABLE IF NOT EXISTS chunks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    doc_id INTEGER NOT NULL REFERENCES documents(id) ON DELETE CASCADE,
    chunk_index INTEGER NOT NULL,
    content TEXT NOT NULL,
    content_hash TEXT,
    chroma_id TEXT,
    start_offset INTEGER,
    end_offset INTEGER,
    heading_context TEXT,
    UNIQUE(doc_id, chunk_index)
);

-- ChatGPT conversations
CREATE TABLE IF NOT EXISTS conversations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    doc_id INTEGER REFERENCES documents(id) ON DELETE CASCADE,
    chatgpt_id TEXT UNIQUE,
    title TEXT,
    created_at TEXT,
    updated_at TEXT,
    model_slug TEXT,
    message_count INTEGER DEFAULT 0
);

-- ChatGPT messages
CREATE TABLE IF NOT EXISTS messages (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    conv_id INTEGER NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    chatgpt_msg_id TEXT,
    role TEXT NOT NULL,
    content TEXT,
    parent_msg_id INTEGER REFERENCES messages(id),
    created_at TEXT,
    model_slug TEXT
);

-- Emails
CREATE TABLE IF NOT EXISTS emails (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    doc_id INTEGER REFERENCES documents(id) ON DELETE CASCADE,
    message_id TEXT UNIQUE,              -- Email Message-ID header
    account TEXT NOT NULL,               -- e.g., 'gmail', 'hotmail-wijaya193'
    folder TEXT,                         -- e.g., 'Inbox', 'Sent'
    from_addr TEXT,
    to_addrs TEXT,                       -- JSON array
    cc_addrs TEXT,                       -- JSON array
    subject TEXT,
    body_text TEXT,
    date TEXT,
    thread_id TEXT,
    in_reply_to TEXT,
    is_read BOOLEAN DEFAULT 1,
    is_flagged BOOLEAN DEFAULT 0,
    attachments TEXT,                    -- JSON array of filenames
    content_hash TEXT NOT NULL,
    indexed_at TEXT DEFAULT (datetime('now'))
);

-- Tags
CREATE TABLE IF NOT EXISTS tags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS doc_tags (
    doc_id INTEGER REFERENCES documents(id) ON DELETE CASCADE,
    tag_id INTEGER REFERENCES tags(id) ON DELETE CASCADE,
    PRIMARY KEY (doc_id, tag_id)
);

-- FTS5 virtual tables
CREATE VIRTUAL TABLE IF NOT EXISTS documents_fts USING fts5(
    title,
    content,
    content='',
    tokenize='porter unicode61'
);

CREATE VIRTUAL TABLE IF NOT EXISTS chunks_fts USING fts5(
    content,
    heading_context,
    content='',
    tokenize='porter unicode61'
);

CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
    content,
    content='',
    tokenize='porter unicode61'
);

-- FTS for emails
CREATE VIRTUAL TABLE IF NOT EXISTS emails_fts USING fts5(
    subject,
    body_text,
    from_addr,
    content='emails',
    content_rowid='id',
    tokenize='porter unicode61'
);

-- Triggers to keep emails_fts in sync
CREATE TRIGGER IF NOT EXISTS emails_ai AFTER INSERT ON emails BEGIN
    INSERT INTO emails_fts(rowid, subject, body_text, from_addr)
    VALUES (new.id, COALESCE(new.subject, ''), COALESCE(new.body_text, ''), COALESCE(new.from_addr, ''));
END;

CREATE TRIGGER IF NOT EXISTS emails_ad AFTER DELETE ON emails BEGIN
    INSERT INTO emails_fts(emails_fts, rowid, subject, body_text, from_addr)
    VALUES('delete', old.id, COALESCE(old.subject, ''), COALESCE(old.body_text, ''), COALESCE(old.from_addr, ''));
END;

CREATE TRIGGER IF NOT EXISTS emails_au AFTER UPDATE ON emails BEGIN
    INSERT INTO emails_fts(emails_fts, rowid, subject, body_text, from_addr)
    VALUES('delete', old.id, COALESCE(old.subject, ''), COALESCE(old.body_text, ''), COALESCE(old.from_addr, ''));
    INSERT INTO emails_fts(rowid, subject, body_text, from_addr)
    VALUES (new.id, COALESCE(new.subject, ''), COALESCE(new.body_text, ''), COALESCE(new.from_addr, ''));
END;

-- Sessions table for Claude Code session memory
CREATE TABLE IF NOT EXISTS sessions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    created_at TEXT DEFAULT (datetime('now')),
    updated_at TEXT DEFAULT (datetime('now')),
    summary TEXT NOT NULL,
    topics TEXT,                              -- JSON array of key topics
    key_facts TEXT,                           -- JSON array of facts/decisions
    topics_text TEXT,                         -- Space-separated for FTS
    key_facts_text TEXT,                      -- Newline-separated for FTS
    word_count INTEGER,
    message_count INTEGER
);

-- FTS for sessions (with content sync triggers for snippet support)
CREATE VIRTUAL TABLE IF NOT EXISTS sessions_fts USING fts5(
    summary,
    topics_text,
    key_facts_text,
    content='sessions',
    content_rowid='id',
    tokenize='porter unicode61'
);

-- Triggers to keep sessions_fts in sync with sessions table
CREATE TRIGGER IF NOT EXISTS sessions_ai AFTER INSERT ON sessions BEGIN
    INSERT INTO sessions_fts(rowid, summary, topics_text, key_facts_text)
    VALUES (new.id, new.summary, COALESCE(new.topics_text, ''), COALESCE(new.key_facts_text, ''));
END;

CREATE TRIGGER IF NOT EXISTS sessions_ad AFTER DELETE ON sessions BEGIN
    INSERT INTO sessions_fts(sessions_fts, rowid, summary, topics_text, key_facts_text)
    VALUES('delete', old.id, old.summary, COALESCE(old.topics_text, ''), COALESCE(old.key_facts_text, ''));
END;

CREATE TRIGGER IF NOT EXISTS sessions_au AFTER UPDATE ON sessions BEGIN
    INSERT INTO sessions_fts(sessions_fts, rowid, summary, topics_text, key_facts_text)
    VALUES('delete', old.id, old.summary, COALESCE(old.topics_text, ''), COALESCE(old.key_facts_text, ''));
    INSERT INTO sessions_fts(rowid, summary, topics_text, key_facts_text)
    VALUES (new.id, new.summary, COALESCE(new.topics_text, ''), COALESCE(new.key_facts_text, ''));
END;

-- Claude Code sources for conversation import
CREATE TABLE IF NOT EXISTS claude_sources (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    path TEXT NOT NULL UNIQUE,        -- e.g., ~/.claude/projects
    name TEXT,                         -- friendly name
    enabled BOOLEAN DEFAULT 1,
    last_sync TEXT,
    created_at TEXT DEFAULT (datetime('now'))
);

-- Track imported Claude conversations to avoid duplicates
CREATE TABLE IF NOT EXISTS claude_conversations (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_id INTEGER REFERENCES claude_sources(id) ON DELETE CASCADE,
    session_id TEXT NOT NULL,          -- UUID from filename
    project TEXT NOT NULL,             -- Project directory name
    content_hash TEXT NOT NULL,        -- For change detection
    doc_id INTEGER REFERENCES documents(id) ON DELETE SET NULL,
    imported_at TEXT DEFAULT (datetime('now')),
    UNIQUE(source_id, session_id)
);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_documents_source_type ON documents(source_type);
CREATE INDEX IF NOT EXISTS idx_documents_created_at ON documents(created_at);
CREATE INDEX IF NOT EXISTS idx_chunks_doc_id ON chunks(doc_id);
-- idx_chunks_content_hash created in _migrate() after column exists
CREATE INDEX IF NOT EXISTS idx_conversations_created_at ON conversations(created_at);
CREATE INDEX IF NOT EXISTS idx_messages_conv_id ON messages(conv_id);
CREATE INDEX IF NOT EXISTS idx_sessions_created_at ON sessions(created_at);
CREATE INDEX IF NOT EXISTS idx_claude_conversations_source ON claude_conversations(source_id);
CREATE INDEX IF NOT EXISTS idx_emails_account ON emails(account);
CREATE INDEX IF NOT EXISTS idx_emails_date ON emails(date);
CREATE INDEX IF NOT EXISTS idx_emails_thread_id ON emails(thread_id);

-- Embedding cache (replaces per-file JSON cache)
CREATE TABLE IF NOT EXISTS embedding_cache (
    content_hash TEXT NOT NULL,
    model TEXT NOT NULL,
    embedding BLOB NOT NULL,
    created_at TEXT DEFAULT (datetime('now')),
    PRIMARY KEY (content_hash, model)
);

-- E2E Test Results Storage (implements SKILL.md specification)
-- Main results table with enriched sensor metrics
CREATE TABLE IF NOT EXISTS e2e_results (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    run_id TEXT NOT NULL,
    feature TEXT NOT NULL,
    story TEXT NOT NULL,
    result TEXT NOT NULL CHECK(result IN ('PASS', 'FAIL', 'BLOCKED', 'SKIP')),

    -- Timing (quantitative)
    started_at TEXT NOT NULL,
    completed_at TEXT NOT NULL,
    duration_ms INTEGER,
    step_durations_json TEXT,

    -- Failure details (qualitative)
    error_message TEXT,
    stack_trace TEXT,
    expected TEXT,
    actual TEXT,
    blocked_reason TEXT,

    -- Sensor differentials (quantitative)
    geometry_anomaly_count INTEGER,
    x11_event_count INTEGER,
    attention_shifts INTEGER,
    input_pause_duration_ms INTEGER,
    mouse_fallback_count INTEGER,

    -- Visual evidence (qualitative)
    baseline_screenshot_path TEXT,
    result_screenshot_path TEXT,
    diff_screenshot_path TEXT,

    -- Retry tracking (quantitative)
    retry_count INTEGER DEFAULT 0,

    -- Story metadata
    tags TEXT,
    estimated_duration_ms INTEGER,
    duration_variance_pct REAL,

    timestamp TEXT DEFAULT (datetime('now'))
);

-- Test run metadata (groups individual story results)
CREATE TABLE IF NOT EXISTS e2e_test_runs (
    run_id TEXT PRIMARY KEY,
    started_at TEXT NOT NULL,
    completed_at TEXT,
    total_stories INTEGER DEFAULT 0,
    passed INTEGER DEFAULT 0,
    failed INTEGER DEFAULT 0,
    blocked INTEGER DEFAULT 0,
    skipped INTEGER DEFAULT 0,
    total_retries INTEGER DEFAULT 0,
    trigger TEXT,
    git_commit TEXT,

    -- Regression tracking
    regressions_detected INTEGER DEFAULT 0,
    fixes_detected INTEGER DEFAULT 0,

    -- Reports location
    report_markdown_path TEXT,
    screenshots_dir TEXT DEFAULT '/home/bw/e2e-reports/screenshots'
);

-- Screenshot metadata
CREATE TABLE IF NOT EXISTS e2e_screenshots (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    result_id INTEGER NOT NULL,
    path TEXT NOT NULL,
    type TEXT CHECK(type IN ('baseline', 'step', 'result', 'diff')),
    step_name TEXT,
    width INTEGER,
    height INTEGER,
    file_size_bytes INTEGER,
    timestamp TEXT DEFAULT (datetime('now')),

    FOREIGN KEY (result_id) REFERENCES e2e_results(id) ON DELETE CASCADE
);

-- Full-text search on error messages
CREATE VIRTUAL TABLE IF NOT EXISTS e2e_results_fts USING fts5(
    error_message,
    stack_trace,
    content=e2e_results,
    content_rowid=id
);

-- Triggers to keep FTS index in sync
CREATE TRIGGER IF NOT EXISTS e2e_results_ai AFTER INSERT ON e2e_results BEGIN
    INSERT INTO e2e_results_fts(rowid, error_message, stack_trace)
    VALUES (new.id, COALESCE(new.error_message, ''), COALESCE(new.stack_trace, ''));
END;

CREATE TRIGGER IF NOT EXISTS e2e_results_ad AFTER DELETE ON e2e_results BEGIN
    INSERT INTO e2e_results_fts(e2e_results_fts, rowid, error_message, stack_trace)
    VALUES('delete', old.id, COALESCE(old.error_message, ''), COALESCE(old.stack_trace, ''));
END;

CREATE TRIGGER IF NOT EXISTS e2e_results_au AFTER UPDATE ON e2e_results BEGIN
    INSERT INTO e2e_results_fts(e2e_results_fts, rowid, error_message, stack_trace)
    VALUES('delete', old.id, COALESCE(old.error_message, ''), COALESCE(old.stack_trace, ''));
    INSERT INTO e2e_results_fts(rowid, error_message, stack_trace)
    VALUES (new.id, COALESCE(new.error_message, ''), COALESCE(new.stack_trace, ''));
END;

-- Indexes for e2e tables
CREATE INDEX IF NOT EXISTS idx_e2e_feature ON e2e_results(feature);
CREATE INDEX IF NOT EXISTS idx_e2e_story ON e2e_results(story);
CREATE INDEX IF NOT EXISTS idx_e2e_result ON e2e_results(result);
CREATE INDEX IF NOT EXISTS idx_e2e_run_id ON e2e_results(run_id);
CREATE INDEX IF NOT EXISTS idx_e2e_started_at ON e2e_results(started_at DESC);
CREATE INDEX IF NOT EXISTS idx_e2e_tags ON e2e_results(tags);
CREATE INDEX IF NOT EXISTS idx_screenshots_result ON e2e_screenshots(result_id);
CREATE INDEX IF NOT EXISTS idx_screenshots_type ON e2e_screenshots(type);
CREATE INDEX IF NOT EXISTS idx_runs_started_at ON e2e_test_runs(started_at DESC);
"""


@dataclass
class Document:
    id: int
    source_type: str
    source_path: str
    title: Optional[str]
    created_at: Optional[str]
    updated_at: Optional[str]
    indexed_at: str
    content_hash: str
    word_count: Optional[int]
    metadata: dict


@dataclass
class SearchResult:
    doc_id: int
    title: str
    source_path: str
    snippet: str
    score: float
    source_type: str
    created_at: Optional[str]


@dataclass
class Session:
    id: int
    created_at: str
    updated_at: str
    summary: str
    topics: list[str]
    key_facts: list[str]
    word_count: Optional[int]
    message_count: Optional[int]


@dataclass
class SessionSearchResult:
    session_id: int
    summary: str
    snippet: str
    score: float
    created_at: str
    topics: list[str]


class VaultDB:
    """SQLite database for vault metadata and full-text search."""

    def __init__(self, db_path: Path):
        self.db_path = db_path
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self._init_db()

    def _init_db(self):
        """Initialize database with schema."""
        with sqlite3.connect(self.db_path) as conn:
            self._migrate(conn)
            conn.executescript(SCHEMA)

    def _migrate(self, conn):
        """Run schema migrations for existing databases."""
        # Check if chunks table exists at all
        tables = [r[0] for r in conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='chunks'"
        ).fetchall()]
        if not tables:
            return  # Fresh DB, SCHEMA will create everything

        # Add content_hash column to chunks if missing
        cols = [row[1] for row in conn.execute("PRAGMA table_info(chunks)").fetchall()]
        if "content_hash" not in cols:
            conn.execute("ALTER TABLE chunks ADD COLUMN content_hash TEXT")

        # Create index after column exists
        conn.execute("CREATE INDEX IF NOT EXISTS idx_chunks_content_hash ON chunks(content_hash)")

        # Migration 007: Add e2e test results tables
        e2e_tables = [r[0] for r in conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='e2e_results'"
        ).fetchall()]
        if not e2e_tables:
            # Read and execute migration from file
            from pathlib import Path
            migration_path = Path(__file__).parent.parent.parent.parent / "migrations" / "007_e2e_results.sql"
            if migration_path.exists():
                with open(migration_path) as f:
                    conn.executescript(f.read())

        # Migration 008: Add claude_messages table for message-level tracking
        claude_msg_tables = [r[0] for r in conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='claude_messages'"
        ).fetchall()]
        if not claude_msg_tables:
            from pathlib import Path
            migration_path = Path(__file__).parent.parent.parent.parent / "migrations" / "008_claude_messages.sql"
            if migration_path.exists():
                with open(migration_path) as f:
                    conn.executescript(f.read())

    def _conn(self) -> sqlite3.Connection:
        """Get a database connection."""
        conn = sqlite3.connect(self.db_path, timeout=30.0)  # 30 second busy timeout
        conn.text_factory = lambda b: b.decode('utf-8', errors='replace')
        conn.row_factory = sqlite3.Row
        return conn

    def get_document_by_path(self, source_path: str) -> Optional[Document]:
        """Get document by source path."""
        with self._conn() as conn:
            row = conn.execute(
                "SELECT * FROM documents WHERE source_path = ?",
                (source_path,)
            ).fetchone()
            if row:
                return Document(
                    id=row["id"],
                    source_type=row["source_type"],
                    source_path=row["source_path"],
                    title=row["title"],
                    created_at=row["created_at"],
                    updated_at=row["updated_at"],
                    indexed_at=row["indexed_at"],
                    content_hash=row["content_hash"],
                    word_count=row["word_count"],
                    metadata=json.loads(row["metadata"]) if row["metadata"] else {}
                )
        return None

    def upsert_document(
        self,
        source_type: str,
        source_path: str,
        content_hash: str,
        title: Optional[str] = None,
        created_at: Optional[str] = None,
        updated_at: Optional[str] = None,
        word_count: Optional[int] = None,
        metadata: Optional[dict] = None,
        full_text: Optional[str] = None
    ) -> int:
        """Insert or update a document, returns doc_id."""
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO documents (source_type, source_path, title, created_at,
                                       updated_at, content_hash, word_count, metadata)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(source_path) DO UPDATE SET
                    title = excluded.title,
                    updated_at = excluded.updated_at,
                    indexed_at = datetime('now'),
                    content_hash = excluded.content_hash,
                    word_count = excluded.word_count,
                    metadata = excluded.metadata
                RETURNING id
            """, (
                source_type, source_path, title, created_at, updated_at,
                content_hash, word_count, json.dumps(metadata) if metadata else None
            ))
            doc_id = cursor.fetchone()[0]

            # Update FTS
            if full_text:
                conn.execute(
                    "INSERT INTO documents_fts(rowid, title, content) VALUES (?, ?, ?)",
                    (doc_id, title or "", full_text)
                )

            return doc_id

    def add_chunk(
        self,
        doc_id: int,
        chunk_index: int,
        content: str,
        chroma_id: Optional[str] = None,
        heading_context: Optional[str] = None,
        start_offset: Optional[int] = None,
        end_offset: Optional[int] = None,
        content_hash: Optional[str] = None,
        metadata: Optional[dict] = None
    ) -> int:
        """Add a chunk for a document."""
        import json as json_lib

        metadata_json = json_lib.dumps(metadata) if metadata else None

        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO chunks (doc_id, chunk_index, content, content_hash, chroma_id,
                                   heading_context, start_offset, end_offset, metadata)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(doc_id, chunk_index) DO UPDATE SET
                    content = excluded.content,
                    content_hash = excluded.content_hash,
                    chroma_id = excluded.chroma_id,
                    heading_context = excluded.heading_context,
                    metadata = excluded.metadata
                RETURNING id
            """, (doc_id, chunk_index, content, content_hash, chroma_id, heading_context,
                  start_offset, end_offset, metadata_json))
            chunk_id = cursor.fetchone()[0]

            # Update FTS
            conn.execute(
                "INSERT INTO chunks_fts(rowid, content, heading_context) VALUES (?, ?, ?)",
                (chunk_id, content, heading_context or "")
            )

            return chunk_id

    def delete_chunks_for_doc(self, doc_id: int):
        """Delete all chunks for a document."""
        with self._conn() as conn:
            # Get chunk IDs for FTS cleanup
            chunks = conn.execute(
                "SELECT id FROM chunks WHERE doc_id = ?", (doc_id,)
            ).fetchall()

            for chunk in chunks:
                conn.execute(
                    "INSERT INTO chunks_fts(chunks_fts, rowid, content, heading_context) "
                    "VALUES('delete', ?, '', '')",
                    (chunk["id"],)
                )

            conn.execute("DELETE FROM chunks WHERE doc_id = ?", (doc_id,))

    def add_conversation(
        self,
        doc_id: int,
        chatgpt_id: str,
        title: str,
        created_at: str,
        updated_at: Optional[str] = None,
        model_slug: Optional[str] = None,
        message_count: int = 0
    ) -> int:
        """Add a ChatGPT conversation."""
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO conversations (doc_id, chatgpt_id, title, created_at,
                                          updated_at, model_slug, message_count)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(chatgpt_id) DO UPDATE SET
                    title = excluded.title,
                    updated_at = excluded.updated_at,
                    model_slug = excluded.model_slug,
                    message_count = excluded.message_count
                RETURNING id
            """, (doc_id, chatgpt_id, title, created_at, updated_at,
                  model_slug, message_count))
            return cursor.fetchone()[0]

    def add_message(
        self,
        conv_id: int,
        role: str,
        content: str,
        chatgpt_msg_id: Optional[str] = None,
        parent_msg_id: Optional[int] = None,
        created_at: Optional[str] = None,
        model_slug: Optional[str] = None
    ) -> int:
        """Add a message to a conversation."""
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO messages (conv_id, chatgpt_msg_id, role, content,
                                     parent_msg_id, created_at, model_slug)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                RETURNING id
            """, (conv_id, chatgpt_msg_id, role, content, parent_msg_id,
                  created_at, model_slug))
            msg_id = cursor.fetchone()[0]

            # Update FTS
            conn.execute(
                "INSERT INTO messages_fts(rowid, content) VALUES (?, ?)",
                (msg_id, content)
            )

            return msg_id

    def search_fts(
        self,
        query: str,
        source_types: Optional[list[str]] = None,
        doc_ids: Optional[list[int]] = None,
        limit: int = 20
    ) -> list[SearchResult]:
        """Full-text search across documents and chunks."""
        with self._conn() as conn:
            # Build filters
            filters = []
            params = [query]

            if source_types:
                placeholders = ",".join("?" * len(source_types))
                filters.append(f"d.source_type IN ({placeholders})")
                params.extend(source_types)

            if doc_ids:
                placeholders = ",".join("?" * len(doc_ids))
                filters.append(f"d.id IN ({placeholders})")
                params.extend(doc_ids)

            filter_clause = ""
            if filters:
                filter_clause = "AND " + " AND ".join(filters)

            params.append(limit)

            rows = conn.execute(f"""
                SELECT d.id, d.title, d.source_path,
                       snippet(chunks_fts, 0, '**', '**', '...', 32) as snippet,
                       bm25(chunks_fts) * -1 as score,
                       d.source_type, d.created_at
                FROM chunks_fts
                JOIN chunks c ON chunks_fts.rowid = c.id
                JOIN documents d ON c.doc_id = d.id
                WHERE chunks_fts MATCH ?
                {filter_clause}
                ORDER BY score DESC
                LIMIT ?
            """, params).fetchall()

            return [
                SearchResult(
                    doc_id=row["id"],
                    title=row["title"] or "Untitled",
                    source_path=row["source_path"],
                    snippet=row["snippet"],
                    score=row["score"],
                    source_type=row["source_type"],
                    created_at=row["created_at"]
                )
                for row in rows
            ]

    def get_stats(self) -> dict:
        """Get database statistics."""
        with self._conn() as conn:
            stats = {}

            # Document counts by type
            rows = conn.execute("""
                SELECT source_type, COUNT(*) as count
                FROM documents GROUP BY source_type
            """).fetchall()
            stats["documents_by_type"] = {row["source_type"]: row["count"] for row in rows}

            # Total chunks
            stats["total_chunks"] = conn.execute(
                "SELECT COUNT(*) FROM chunks"
            ).fetchone()[0]

            # Total conversations
            stats["total_conversations"] = conn.execute(
                "SELECT COUNT(*) FROM conversations"
            ).fetchone()[0]

            # Total messages
            stats["total_messages"] = conn.execute(
                "SELECT COUNT(*) FROM messages"
            ).fetchone()[0]

            return stats

    def execute_query(self, sql: str, params: Optional[list] = None) -> list[dict]:
        """Execute a read-only query (for vault_query tool)."""
        # Basic safety check
        sql_upper = sql.upper().strip()
        if not sql_upper.startswith("SELECT"):
            raise ValueError("Only SELECT queries allowed")

        dangerous = ["DROP", "DELETE", "UPDATE", "INSERT", "ALTER", "CREATE", "ATTACH"]
        for word in dangerous:
            if word in sql_upper:
                raise ValueError(f"Query contains forbidden keyword: {word}")

        with self._conn() as conn:
            rows = conn.execute(sql, params or []).fetchall()
            return [dict(row) for row in rows]

    # =========================================================================
    # Session Methods
    # =========================================================================

    def save_session(
        self,
        summary: str,
        topics: Optional[list[str]] = None,
        key_facts: Optional[list[str]] = None,
        word_count: Optional[int] = None,
        message_count: Optional[int] = None
    ) -> int:
        """Save a session summary, returns session_id.

        FTS is automatically updated via triggers.
        """
        # Sanitize to prevent non-UTF-8 bytes in the database
        summary = summary.encode('utf-8', errors='replace').decode('utf-8')

        # Store JSON for structured retrieval
        topics_json = json.dumps(topics) if topics else None
        key_facts_json = json.dumps(key_facts) if key_facts else None

        # Store text versions for FTS search
        topics_text = " ".join(topics) if topics else ""
        key_facts_text = "\n".join(key_facts) if key_facts else ""

        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO sessions (summary, topics, key_facts, topics_text, key_facts_text,
                                     word_count, message_count)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                RETURNING id
            """, (
                summary,
                topics_json,
                key_facts_json,
                topics_text,
                key_facts_text,
                word_count,
                message_count
            ))
            session_id = cursor.fetchone()[0]
            # FTS is updated automatically via trigger
            return session_id

    def _parse_json_list(self, value: Optional[str]) -> list[str]:
        """Parse JSON array from storage."""
        if not value:
            return []
        try:
            return json.loads(value)
        except json.JSONDecodeError:
            return []

    def _deduplicate_sessions(self, sessions: list[Session]) -> list[Session]:
        """Collapse incremental snapshots into distinct conversations.

        Groups by conversation seed (Started with: content), then within
        each group clusters sessions by message_count proximity and temporal
        locality. Handles interleaved snapshots of concurrent conversations.
        """
        from collections import defaultdict

        groups: dict[str, list[Session]] = defaultdict(list)
        for s in sessions:
            seed = self._extract_conversation_seed(s.summary or "")
            groups[seed].append(s)

        result = []
        for group in groups.values():
            if len(group) == 1:
                result.append(group[0])
                continue

            # Sort chronologically for temporal locality checks
            group.sort(key=lambda s: s.created_at or "")

            # Multi-chain matching: each session finds the best existing
            # chain or starts a new one. This handles interleaved snapshots
            # of concurrent conversations with the same seed.
            chains: list[list[Session]] = []
            for s in group:
                mc = s.message_count or 0
                best_chain = None
                best_distance = float('inf')

                for chain in chains:
                    chain_max = max(c.message_count or 0 for c in chain)
                    chain_last = chain[-1]

                    # Time gap to chain's most recent session
                    try:
                        t_curr = datetime.fromisoformat(s.created_at)
                        t_last = datetime.fromisoformat(chain_last.created_at)
                        gap_hours = abs((t_curr - t_last).total_seconds()) / 3600
                    except (ValueError, TypeError):
                        gap_hours = 999

                    if gap_hours >= 6:
                        continue

                    # Message count must be within 2x of chain's max
                    ratio = mc / max(chain_max, 1)
                    if ratio < 0.5 or ratio > 2.0:
                        continue

                    # Pick chain with nearest max message_count
                    distance = abs(mc - chain_max)
                    if distance < best_distance:
                        best_distance = distance
                        best_chain = chain

                if best_chain is not None:
                    best_chain.append(s)
                else:
                    chains.append([s])

            # Keep the most complete session per chain, but merge
            # topics/key_facts from ALL snapshots so no information is lost.
            for chain in chains:
                best = max(chain, key=lambda s: s.message_count or 0)

                if len(chain) == 1:
                    result.append(best)
                    continue

                # Merge topics from all snapshots
                all_topics = list(dict.fromkeys(
                    t for s in chain for t in (s.topics or [])
                ))
                # Merge key_facts from all snapshots
                all_facts = list(dict.fromkeys(
                    f for s in chain for f in (s.key_facts or [])
                ))
                # Extract unique "Ended with:" mentions from collapsed
                # snapshots — these capture different conversation moments
                # that would otherwise be lost
                endpoints = set()
                for s in chain:
                    if s is best:
                        continue
                    summary = s.summary or ""
                    marker = '\u2014 Ended with:'
                    idx = summary.find(marker)
                    if idx >= 0:
                        endpoint = summary[idx + len(marker):].strip()
                        if endpoint and endpoint != 'tool_use_id':
                            endpoints.add(endpoint[:200])
                if endpoints:
                    for ep in endpoints:
                        fact = f"[conversation endpoint] {ep}"
                        if fact not in all_facts:
                            all_facts.append(fact)

                result.append(Session(
                    id=best.id,
                    created_at=best.created_at,
                    updated_at=best.updated_at,
                    summary=best.summary,
                    topics=all_topics or best.topics,
                    key_facts=all_facts or best.key_facts,
                    word_count=best.word_count,
                    message_count=best.message_count
                ))

        result.sort(key=lambda s: s.created_at or "", reverse=True)
        return result

    def get_recent_sessions(self, limit: int = 5) -> list[Session]:
        """Get recent distinct sessions ordered by creation time.

        Deduplicates incremental snapshots of the same conversation.
        The limit applies after dedup, so limit=15 means 15 distinct
        conversations, not 15 raw rows.
        """
        with self._conn() as conn:
            rows = conn.execute("""
                SELECT id, created_at, updated_at, summary, topics, key_facts,
                       word_count, message_count
                FROM sessions
                ORDER BY created_at DESC
            """).fetchall()

            sessions = [
                Session(
                    id=row["id"],
                    created_at=row["created_at"],
                    updated_at=row["updated_at"],
                    summary=row["summary"],
                    topics=self._parse_json_list(row["topics"]),
                    key_facts=self._parse_json_list(row["key_facts"]),
                    word_count=row["word_count"],
                    message_count=row["message_count"]
                )
                for row in rows
            ]

            deduped = self._deduplicate_sessions(sessions)
            return deduped[:limit]

    def get_sessions_since(self, days_ago: int = 10) -> list[Session]:
        """Get distinct sessions from the last N days.

        Deduplicates incremental snapshots of the same conversation.

        Args:
            days_ago: Number of days to look back (default: 10)

        Returns:
            List of distinct sessions within date range, ordered by creation DESC
        """
        with self._conn() as conn:
            rows = conn.execute("""
                SELECT id, created_at, updated_at, summary, topics, key_facts,
                       word_count, message_count
                FROM sessions
                WHERE datetime(created_at) >= datetime('now', ? || ' days')
                ORDER BY created_at DESC
            """, (f'-{days_ago}',)).fetchall()

            sessions = [
                Session(
                    id=row["id"],
                    created_at=row["created_at"],
                    updated_at=row["updated_at"],
                    summary=row["summary"],
                    topics=self._parse_json_list(row["topics"]),
                    key_facts=self._parse_json_list(row["key_facts"]),
                    word_count=row["word_count"],
                    message_count=row["message_count"]
                )
                for row in rows
            ]

            return self._deduplicate_sessions(sessions)

    def get_session_by_id(self, session_id: int) -> Optional[Session]:
        """Get a session by ID."""
        with self._conn() as conn:
            row = conn.execute(
                "SELECT * FROM sessions WHERE id = ?",
                (session_id,)
            ).fetchone()
            if row:
                return Session(
                    id=row["id"],
                    created_at=row["created_at"],
                    updated_at=row["updated_at"],
                    summary=row["summary"],
                    topics=self._parse_json_list(row["topics"]),
                    key_facts=self._parse_json_list(row["key_facts"]),
                    word_count=row["word_count"],
                    message_count=row["message_count"]
                )
        return None

    @staticmethod
    def _extract_conversation_seed(summary: str) -> str:
        """Extract conversation identity from session summary.

        Sessions follow: 'Session with N user messages... Started with: <seed>'
        The seed uniquely identifies a conversation; multiple rows with the
        same seed are incremental snapshots of the same conversation.
        """
        idx = summary.find('Started with:')
        if idx >= 0:
            seed = summary[idx + 14:idx + 114]
            end_idx = seed.find('\u2014 Ended with:')  # em dash
            if end_idx < 0:
                end_idx = seed.find(' — Ended with:')  # double-byte dash
            if end_idx > 0:
                seed = seed[:end_idx]
            return seed.strip()
        return summary[:100]

    def search_sessions_fts(
        self,
        query: str,
        limit: int = 10,
        recency_bias: float = 0.0
    ) -> list[SessionSearchResult]:
        """Full-text search over sessions with optional temporal weighting.

        Deduplicates results so only the highest-scoring snapshot per
        conversation is returned.

        Args:
            query: FTS5 search query
            limit: Maximum distinct results
            recency_bias: Temporal recency weight 0.0-1.0
                         0.0 = pure BM25 (default)
                         0.7 = resumption queries (favor recent)

        Hybrid scoring: BM25 * ((1-α) + α * decay)
        Decay function: 1.0 / (1.0 + days_ago)
        """
        # Over-fetch to account for duplicates that will be collapsed
        fetch_limit = limit * 5
        with self._conn() as conn:
            rows = conn.execute("""
                SELECT s.id, s.summary, s.created_at, s.topics,
                       snippet(sessions_fts, 0, '**', '**', '...', 32) as snippet,
                       CASE
                           WHEN ? > 0 THEN
                               (bm25(sessions_fts) * -1) *
                               ((1.0 - ?) + ? * (1.0 / (1.0 + (julianday('now') - julianday(s.created_at)))))
                           ELSE
                               bm25(sessions_fts) * -1
                       END as score
                FROM sessions_fts
                JOIN sessions s ON sessions_fts.rowid = s.id
                WHERE sessions_fts MATCH ?
                ORDER BY score DESC
                LIMIT ?
            """, (recency_bias, recency_bias, recency_bias, query, fetch_limit)).fetchall()

            # Deduplicate: keep highest-scoring result per conversation seed
            seen_seeds: dict[str, int] = {}
            deduped: list[SessionSearchResult] = []
            for row in rows:
                seed = self._extract_conversation_seed(row["summary"] or "")
                if seed in seen_seeds:
                    continue
                seen_seeds[seed] = row["id"]
                deduped.append(SessionSearchResult(
                    session_id=row["id"],
                    summary=row["summary"],
                    snippet=row["snippet"],
                    score=row["score"],
                    created_at=row["created_at"],
                    topics=self._parse_json_list(row["topics"])
                ))
                if len(deduped) >= limit:
                    break

            return deduped

    def delete_session(self, session_id: int) -> bool:
        """Delete a session. FTS is updated automatically via trigger."""
        with self._conn() as conn:
            cursor = conn.execute("DELETE FROM sessions WHERE id = ?", (session_id,))
            return cursor.rowcount > 0

    # =========================================================================
    # Claude Source Methods
    # =========================================================================

    def add_claude_source(self, path: str, name: Optional[str] = None) -> int:
        """Add a Claude conversation source directory.

        Args:
            path: Path to Claude projects directory (e.g., ~/.claude/projects)
            name: Optional friendly name

        Returns:
            Source ID
        """
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO claude_sources (path, name)
                VALUES (?, ?)
                ON CONFLICT(path) DO UPDATE SET
                    name = COALESCE(excluded.name, claude_sources.name),
                    enabled = 1
                RETURNING id
            """, (path, name))
            return cursor.fetchone()[0]

    def remove_claude_source(self, path: str) -> bool:
        """Remove a Claude source by path.

        Returns:
            True if source was found and removed
        """
        with self._conn() as conn:
            cursor = conn.execute(
                "DELETE FROM claude_sources WHERE path = ?",
                (path,)
            )
            return cursor.rowcount > 0

    def list_claude_sources(self) -> list[dict]:
        """List all Claude sources with stats."""
        with self._conn() as conn:
            rows = conn.execute("""
                SELECT cs.id, cs.path, cs.name, cs.enabled, cs.last_sync, cs.created_at,
                       COUNT(cc.id) as conversation_count
                FROM claude_sources cs
                LEFT JOIN claude_conversations cc ON cs.id = cc.source_id
                GROUP BY cs.id
                ORDER BY cs.created_at DESC
            """).fetchall()

            return [dict(row) for row in rows]

    def update_claude_source_sync(self, source_id: int) -> None:
        """Update the last_sync timestamp for a source."""
        with self._conn() as conn:
            conn.execute("""
                UPDATE claude_sources
                SET last_sync = datetime('now')
                WHERE id = ?
            """, (source_id,))

    def get_claude_source_by_id(self, source_id: int) -> Optional[dict]:
        """Get a Claude source by ID."""
        with self._conn() as conn:
            row = conn.execute(
                "SELECT * FROM claude_sources WHERE id = ?",
                (source_id,)
            ).fetchone()
            return dict(row) if row else None

    def get_claude_conversation(
        self,
        source_id: int,
        session_id: str
    ) -> Optional[dict]:
        """Get a tracked Claude conversation."""
        with self._conn() as conn:
            row = conn.execute("""
                SELECT * FROM claude_conversations
                WHERE source_id = ? AND session_id = ?
            """, (source_id, session_id)).fetchone()
            return dict(row) if row else None

    def upsert_claude_conversation(
        self,
        source_id: int,
        session_id: str,
        project: str,
        content_hash: str,
        doc_id: Optional[int] = None
    ) -> int:
        """Insert or update a tracked Claude conversation.

        Returns:
            Conversation tracking ID
        """
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO claude_conversations
                    (source_id, session_id, project, content_hash, doc_id)
                VALUES (?, ?, ?, ?, ?)
                ON CONFLICT(source_id, session_id) DO UPDATE SET
                    content_hash = excluded.content_hash,
                    doc_id = excluded.doc_id,
                    imported_at = datetime('now')
                RETURNING id
            """, (source_id, session_id, project, content_hash, doc_id))
            return cursor.fetchone()[0]

    # -------------------------------------------------------------------------
    # Claude messages methods (message-level tracking)
    # -------------------------------------------------------------------------

    def add_claude_message(
        self,
        claude_conv_id: int,
        uuid: str,
        session_id: str,
        message_index: int,
        role: str,
        content: str,
        created_at: str,
        parent_uuid: Optional[str] = None,
        content_blocks: Optional[str] = None,
        model: Optional[str] = None,
        metadata: Optional[str] = None,
        content_hash: Optional[str] = None
    ) -> int:
        """Add a Claude message to conversation.

        Returns:
            Message ID
        """
        with self._conn() as conn:
            cursor = conn.execute("""
                INSERT INTO claude_messages
                    (claude_conv_id, uuid, parent_uuid, session_id, message_index,
                     role, content, content_blocks, created_at, model, metadata, content_hash)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(uuid) DO UPDATE SET
                    content = excluded.content,
                    content_blocks = excluded.content_blocks,
                    created_at = excluded.created_at,
                    model = excluded.model,
                    metadata = excluded.metadata,
                    content_hash = excluded.content_hash
                RETURNING id
            """, (
                claude_conv_id, uuid, parent_uuid, session_id, message_index,
                role, content, content_blocks, created_at, model, metadata, content_hash
            ))
            return cursor.fetchone()[0]

    def get_messages_since(
        self,
        days: int,
        session_id: Optional[str] = None,
        role: Optional[str] = None,
        limit: Optional[int] = None
    ) -> list[dict]:
        """Get Claude messages from past N days using MESSAGE timestamps.

        Args:
            days: Number of days to look back
            session_id: Optional filter to specific session
            role: Optional filter by role ('user', 'assistant')
            limit: Optional result limit

        Returns:
            List of messages ordered by created_at DESC
        """
        with self._conn() as conn:
            query = """
                SELECT id, claude_conv_id, uuid, session_id, role, content,
                       created_at, model, message_index
                FROM claude_messages
                WHERE datetime(created_at) >= datetime('now', ? || ' days')
            """
            params = [f'-{days}']

            if session_id:
                query += " AND session_id = ?"
                params.append(session_id)
            if role:
                query += " AND role = ?"
                params.append(role)

            query += " ORDER BY created_at DESC"
            if limit:
                query += " LIMIT ?"
                params.append(limit)

            rows = conn.execute(query, params).fetchall()
            return [dict(row) for row in rows]

    def get_sessions_with_recent_activity(
        self,
        days: int
    ) -> list[dict]:
        """Get sessions that have messages in the past N days.

        Returns sessions with recent activity, regardless of when session was created.
        Each result includes last_message_at and recent_message_count.

        Args:
            days: Number of days to look back

        Returns:
            List of sessions with activity info
        """
        with self._conn() as conn:
            query = """
                SELECT
                    s.id, s.created_at, s.updated_at, s.summary,
                    s.topics, s.key_facts, s.word_count, s.message_count,
                    COUNT(m.id) as recent_message_count,
                    MAX(m.created_at) as last_message_at
                FROM sessions s
                JOIN claude_messages m ON CAST(m.session_id AS TEXT) = CAST(s.id AS TEXT)
                WHERE datetime(m.created_at) >= datetime('now', ? || ' days')
                GROUP BY s.id
                ORDER BY last_message_at DESC
            """
            rows = conn.execute(query, [f'-{days}']).fetchall()
            return [dict(row) for row in rows]

    def delete_messages_for_conversation(self, claude_conv_id: int):
        """Delete all messages for a conversation (for re-import).

        FTS cleanup handled automatically by triggers.
        """
        with self._conn() as conn:
            conn.execute(
                "DELETE FROM claude_messages WHERE claude_conv_id = ?",
                (claude_conv_id,)
            )

    # -------------------------------------------------------------------------
    # Email methods
    # -------------------------------------------------------------------------

    def get_email_by_message_id(self, message_id: str) -> Optional[dict]:
        """Get email by Message-ID header."""
        with self._conn() as conn:
            row = conn.execute(
                "SELECT * FROM emails WHERE message_id = ?",
                (message_id,)
            ).fetchone()
            return dict(row) if row else None

    def upsert_email(
        self,
        message_id: str,
        account: str,
        content_hash: str,
        folder: Optional[str] = None,
        from_addr: Optional[str] = None,
        to_addrs: Optional[list] = None,
        cc_addrs: Optional[list] = None,
        subject: Optional[str] = None,
        body_text: Optional[str] = None,
        date: Optional[str] = None,
        thread_id: Optional[str] = None,
        in_reply_to: Optional[str] = None,
        is_read: bool = True,
        is_flagged: bool = False,
        attachments: Optional[list] = None
    ) -> int:
        """Insert or update an email.

        Returns:
            Email ID
        """
        with self._conn() as conn:
            # First create/update the document entry
            doc_cursor = conn.execute("""
                INSERT INTO documents (source_type, source_path, title, created_at,
                                       updated_at, content_hash, word_count, metadata)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(source_path) DO UPDATE SET
                    title = excluded.title,
                    updated_at = excluded.updated_at,
                    indexed_at = datetime('now'),
                    content_hash = excluded.content_hash,
                    word_count = excluded.word_count,
                    metadata = excluded.metadata
                RETURNING id
            """, (
                'email',
                f"email://{account}/{message_id}",
                subject,
                date,
                date,
                content_hash,
                len(body_text.split()) if body_text else 0,
                json.dumps({"account": account, "folder": folder})
            ))
            doc_id = doc_cursor.fetchone()[0]

            # Now insert/update the email record
            cursor = conn.execute("""
                INSERT INTO emails (
                    doc_id, message_id, account, folder, from_addr, to_addrs,
                    cc_addrs, subject, body_text, date, thread_id, in_reply_to,
                    is_read, is_flagged, attachments, content_hash
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(message_id) DO UPDATE SET
                    folder = excluded.folder,
                    is_read = excluded.is_read,
                    is_flagged = excluded.is_flagged,
                    indexed_at = datetime('now')
                RETURNING id
            """, (
                doc_id, message_id, account, folder, from_addr,
                json.dumps(to_addrs) if to_addrs else None,
                json.dumps(cc_addrs) if cc_addrs else None,
                subject, body_text, date, thread_id, in_reply_to,
                is_read, is_flagged,
                json.dumps(attachments) if attachments else None,
                content_hash
            ))
            return cursor.fetchone()[0]

    def search_emails(
        self,
        query: str,
        account: Optional[str] = None,
        limit: int = 20
    ) -> list[dict]:
        """Search emails using FTS.

        Args:
            query: Search query
            account: Optional account filter
            limit: Max results

        Returns:
            List of matching emails with snippets
        """
        with self._conn() as conn:
            if account:
                rows = conn.execute("""
                    SELECT e.*, snippet(emails_fts, 1, '<b>', '</b>', '...', 32) as snippet
                    FROM emails e
                    JOIN emails_fts ON e.id = emails_fts.rowid
                    WHERE emails_fts MATCH ? AND e.account = ?
                    ORDER BY rank
                    LIMIT ?
                """, (query, account, limit)).fetchall()
            else:
                rows = conn.execute("""
                    SELECT e.*, snippet(emails_fts, 1, '<b>', '</b>', '...', 32) as snippet
                    FROM emails e
                    JOIN emails_fts ON e.id = emails_fts.rowid
                    WHERE emails_fts MATCH ?
                    ORDER BY rank
                    LIMIT ?
                """, (query, limit)).fetchall()
            return [dict(row) for row in rows]

    def get_email_stats(self) -> dict:
        """Get email statistics."""
        with self._conn() as conn:
            total = conn.execute("SELECT COUNT(*) FROM emails").fetchone()[0]
            by_account = conn.execute("""
                SELECT account, COUNT(*) as count
                FROM emails GROUP BY account
            """).fetchall()
            return {
                "total": total,
                "by_account": {row["account"]: row["count"] for row in by_account}
            }

    # --- Embedding cache ---

    def get_cached_embedding(self, content_hash: str, model: str) -> Optional[bytes]:
        """Get cached embedding blob by content hash and model."""
        with self._conn() as conn:
            row = conn.execute(
                "SELECT embedding FROM embedding_cache WHERE content_hash = ? AND model = ?",
                (content_hash, model)
            ).fetchone()
            return row["embedding"] if row else None

    def get_cached_embeddings_batch(self, content_hashes: list[str], model: str) -> dict[str, bytes]:
        """Get multiple cached embeddings. Returns {hash: blob} for found entries."""
        if not content_hashes:
            return {}
        with self._conn() as conn:
            placeholders = ",".join("?" for _ in content_hashes)
            rows = conn.execute(
                f"SELECT content_hash, embedding FROM embedding_cache WHERE model = ? AND content_hash IN ({placeholders})",
                [model] + content_hashes
            ).fetchall()
            return {row["content_hash"]: row["embedding"] for row in rows}

    def save_cached_embedding(self, content_hash: str, model: str, embedding: bytes):
        """Save an embedding to cache."""
        with self._conn() as conn:
            conn.execute(
                "INSERT OR REPLACE INTO embedding_cache (content_hash, model, embedding) VALUES (?, ?, ?)",
                (content_hash, model, embedding)
            )

    def save_cached_embeddings_batch(self, entries: list[tuple[str, str, bytes]]):
        """Save multiple embeddings. entries: [(content_hash, model, embedding_blob), ...]"""
        if not entries:
            return
        with self._conn() as conn:
            conn.executemany(
                "INSERT OR REPLACE INTO embedding_cache (content_hash, model, embedding) VALUES (?, ?, ?)",
                entries
            )
