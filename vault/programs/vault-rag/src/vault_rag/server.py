"""MCP Server for vault knowledge base."""

import os
import json
import hashlib
from pathlib import Path
from typing import Optional, Literal
from datetime import datetime

from mcp.server.fastmcp import FastMCP

# Initialize server
mcp = FastMCP("Vault Knowledge Base")

# Configuration from environment
VAULT_ROOT = Path(os.environ.get("VAULT_ROOT", os.path.expanduser("~/vault")))
DATA_DIR = VAULT_ROOT / "data"
ORG_DIR = VAULT_ROOT / "org"
DB_PATH = DATA_DIR / "vault.db"
CHROMA_PATH = DATA_DIR / "chromadb"
CACHE_PATH = DATA_DIR / "cache"

# Embedding batch size - process chunks in batches to avoid overwhelming vector DB
EMBEDDING_BATCH_SIZE = 500

# Lazy-loaded components
_db = None
_chroma = None
_embedder = None
_emacs_actions = None


def get_db():
    """Get or create database connection."""
    global _db
    if _db is None:
        from vault_rag.db.sqlite import VaultDB
        _db = VaultDB(DB_PATH)
    return _db


def get_chroma():
    """Get or create ChromaDB client."""
    global _chroma
    if _chroma is None:
        from vault_rag.db.chroma import VaultChroma
        _chroma = VaultChroma(CHROMA_PATH)
    return _chroma


def get_embedder():
    """Get or create embedder."""
    global _embedder
    if _embedder is None:
        from vault_rag.indexer.embedder import Embedder
        _embedder = Embedder(db=get_db())
    return _embedder


def get_emacs_actions():
    """Get or create Emacs actions handler."""
    global _emacs_actions
    if _emacs_actions is None:
        from vault_rag.emacs_actions import EmacsActionsDB
        _emacs_actions = EmacsActionsDB(get_db())
    return _emacs_actions


def reciprocal_rank_fusion(
    semantic_results: list[tuple[int, float]],
    fts_results: list[tuple[int, float]],
    k: int = 60
) -> list[tuple[int, float]]:
    """Merge results using Reciprocal Rank Fusion."""
    scores = {}

    # Semantic (lower distance = better)
    for rank, (doc_id, _) in enumerate(sorted(semantic_results, key=lambda x: x[1])):
        scores[doc_id] = scores.get(doc_id, 0) + 1 / (k + rank + 1)

    # FTS (already ranked by score)
    for rank, (doc_id, _) in enumerate(fts_results):
        scores[doc_id] = scores.get(doc_id, 0) + 1 / (k + rank + 1)

    return sorted(scores.items(), key=lambda x: x[1], reverse=True)


# =============================================================================
# TOOLS
# =============================================================================

@mcp.tool()
def search_semantic(
    query: str,
    source_types: Optional[list[str]] = None,
    doc_ids: Optional[list[int]] = None,
    limit: int = 10
) -> str:
    """
    Semantic search using vector embeddings.

    Finds conceptually similar content even without exact keyword matches.
    Best for: "notes about learning strategies", "discussions about architecture"

    Args:
        query: Natural language search query
        source_types: Filter by source - "org", "chatgpt", "onenote" (default: all)
        doc_ids: Filter by specific document IDs (default: all documents)
        limit: Maximum results (default: 10)

    Returns:
        JSON array of results with doc_id, title, snippet, score, source_path

    Example:
        # Search across all documents
        search_semantic("twitch notification stream")

        # Search within specific documents only
        search_semantic("notification jarring", doc_ids=[17664, 17665])
    """
    embedder = get_embedder()
    chroma = get_chroma()
    db = get_db()

    query_embedding = embedder.embed_query(query)
    results = chroma.search(
        query_embedding,
        n_results=limit,
        source_types=source_types,
        doc_ids=doc_ids
    )

    output = []
    for r in results:
        doc = db.get_document_by_path(r.metadata.get("source_path", ""))
        output.append({
            "doc_id": r.doc_id,
            "title": doc.title if doc else "Unknown",
            "snippet": r.content[:200] + "..." if len(r.content) > 200 else r.content,
            "score": 1 - r.distance,  # Convert distance to similarity
            "source_path": r.metadata.get("source_path", ""),
            "source_type": r.metadata.get("source_type", "")
        })

    return json.dumps(output, indent=2)


@mcp.tool()
def search_fulltext(
    query: str,
    source_types: Optional[list[str]] = None,
    doc_ids: Optional[list[int]] = None,
    limit: int = 20
) -> str:
    """
    Full-text search using SQLite FTS5 with BM25 ranking.

    Best for exact phrases, specific terms, or keyword searches.
    Supports: quoted phrases "exact match", prefix* wildcards

    Args:
        query: Search query (FTS5 syntax supported)
        source_types: Filter by source - "org", "chatgpt", "onenote" (default: all)
        doc_ids: Filter by specific document IDs (default: all documents)
        limit: Maximum results (default: 20)

    Returns:
        JSON array of results with doc_id, title, snippet, score, source_path

    Example:
        # Search across all documents
        search_fulltext("notification stream")

        # Search within specific documents only
        search_fulltext("jarring flash", doc_ids=[17664])
    """
    db = get_db()
    results = db.search_fts(query, source_types, doc_ids, limit)

    output = [{
        "doc_id": r.doc_id,
        "title": r.title,
        "snippet": r.snippet,
        "score": r.score,
        "source_path": r.source_path,
        "source_type": r.source_type,
        "created_at": r.created_at
    } for r in results]

    return json.dumps(output, indent=2)


@mcp.tool()
def search_hybrid(
    query: str,
    source_types: Optional[list[str]] = None,
    doc_ids: Optional[list[int]] = None,
    limit: int = 10
) -> str:
    """
    Hybrid search combining semantic + full-text with Reciprocal Rank Fusion.

    Best general-purpose search - use this by default.
    Combines conceptual similarity with keyword matching.

    Args:
        query: Natural language search query
        source_types: Filter by source - "org", "chatgpt", "onenote" (default: all)
        doc_ids: Filter by specific document IDs (default: all documents)
        limit: Maximum results (default: 10)

    Returns:
        JSON array of merged results with doc_id, title, snippet, score

    Example:
        # Search across all documents
        search_hybrid("notification stream")

        # Search within specific documents only
        search_hybrid("jarring flash system", doc_ids=[17664])
    """
    embedder = get_embedder()
    chroma = get_chroma()
    db = get_db()

    # Semantic search
    query_embedding = embedder.embed_query(query)
    semantic_results = chroma.search(
        query_embedding,
        n_results=limit * 2,
        source_types=source_types,
        doc_ids=doc_ids
    )

    # FTS search
    fts_results = db.search_fts(query, source_types, doc_ids, limit * 2)

    # Convert to (doc_id, score) format
    semantic_pairs = [(r.doc_id, r.distance) for r in semantic_results]
    fts_pairs = [(r.doc_id, r.score) for r in fts_results]

    # Merge with RRF
    merged = reciprocal_rank_fusion(semantic_pairs, fts_pairs)

    # Build output
    output = []
    seen_docs = set()

    for doc_id, score in merged[:limit]:
        if doc_id in seen_docs:
            continue
        seen_docs.add(doc_id)

        # Get snippet from FTS if available
        snippet = ""
        for fts_r in fts_results:
            if fts_r.doc_id == doc_id:
                snippet = fts_r.snippet
                break

        if not snippet:
            for sem_r in semantic_results:
                if sem_r.doc_id == doc_id:
                    snippet = sem_r.content[:200] + "..."
                    break

        # Get doc info
        with db._conn() as conn:
            row = conn.execute(
                "SELECT title, source_path, source_type, created_at FROM documents WHERE id = ?",
                (doc_id,)
            ).fetchone()

        if row:
            output.append({
                "doc_id": doc_id,
                "title": row["title"] or "Untitled",
                "snippet": snippet,
                "score": score,
                "source_path": row["source_path"],
                "source_type": row["source_type"],
                "created_at": row["created_at"]
            })

    return json.dumps(output, indent=2)


@mcp.tool()
def list_documents(
    source_type: Optional[str] = None,
    limit: int = 50,
    offset: int = 0
) -> str:
    """
    List documents in the vault with optional filtering.

    Args:
        source_type: Filter by "org", "chatgpt", or "onenote" (default: all)
        limit: Maximum results (default: 50)
        offset: Pagination offset (default: 0)

    Returns:
        JSON array of document summaries
    """
    db = get_db()

    with db._conn() as conn:
        if source_type:
            rows = conn.execute("""
                SELECT id, title, source_path, source_type, created_at, word_count
                FROM documents
                WHERE source_type = ?
                ORDER BY created_at DESC
                LIMIT ? OFFSET ?
            """, (source_type, limit, offset)).fetchall()
        else:
            rows = conn.execute("""
                SELECT id, title, source_path, source_type, created_at, word_count
                FROM documents
                ORDER BY created_at DESC
                LIMIT ? OFFSET ?
            """, (limit, offset)).fetchall()

    return json.dumps([dict(row) for row in rows], indent=2)


@mcp.tool()
def get_document(
    doc_id: Optional[int] = None,
    source_path: Optional[str] = None
) -> str:
    """
    Retrieve full document content.

    Provide either doc_id (from search results) or source_path.

    Args:
        doc_id: Document ID from search results
        source_path: Relative path like "org/journal/2026-01-20.org"

    Returns:
        Document content with metadata
    """
    if not doc_id and not source_path:
        return json.dumps({"error": "Provide either doc_id or source_path"})

    db = get_db()

    if doc_id:
        doc = None
        with db._conn() as conn:
            row = conn.execute(
                "SELECT * FROM documents WHERE id = ?", (doc_id,)
            ).fetchone()
            if row:
                source_path = row["source_path"]
    elif source_path:
        doc = db.get_document_by_path(source_path)
        if not doc:
            return json.dumps({"error": f"Document not found: {source_path}"})

    # Read actual file content
    full_path = VAULT_ROOT / source_path
    if not full_path.exists():
        return json.dumps({"error": f"File not found: {source_path}"})

    content = full_path.read_text(encoding="utf-8")

    return json.dumps({
        "source_path": source_path,
        "content": content
    }, indent=2)


@mcp.tool()
def query_database(
    sql: str,
    params: Optional[list] = None
) -> str:
    """
    Execute read-only SQL query on the vault database.

    Useful for custom queries on metadata. Only SELECT allowed.

    Example queries:
    - "SELECT * FROM conversations ORDER BY created_at DESC LIMIT 10"
    - "SELECT source_type, COUNT(*) FROM documents GROUP BY source_type"
    - "SELECT * FROM messages WHERE role = 'user' AND content LIKE '%python%'"

    Args:
        sql: SQL SELECT query
        params: Query parameters for ? placeholders

    Returns:
        JSON array of result rows
    """
    db = get_db()

    try:
        results = db.execute_query(sql, params)
        return json.dumps(results, indent=2, default=str)
    except ValueError as e:
        return json.dumps({"error": str(e)})
    except Exception as e:
        return json.dumps({"error": f"Query failed: {e}"})


@mcp.tool()
def get_stats() -> str:
    """
    Get vault statistics including document counts, index health, etc.

    Returns:
        JSON with statistics by source type, total chunks, conversations, etc.
    """
    db = get_db()
    chroma = get_chroma()

    db_stats = db.get_stats()
    chroma_stats = chroma.get_stats()

    return json.dumps({
        "database": db_stats,
        "embeddings": chroma_stats,
        "vault_root": str(VAULT_ROOT)
    }, indent=2)


@mcp.tool()
def save_session(
    summary: str,
    topics: Optional[list[str]] = None,
    key_facts: Optional[list[str]] = None,
    word_count: Optional[int] = None,
    message_count: Optional[int] = None
) -> str:
    """
    Save a session summary for cross-session memory.

    Call this at the end of a Claude Code session to preserve context
    for future sessions. The summary should capture what was discussed,
    decisions made, and important context.

    Args:
        summary: AI-generated summary of the conversation (required)
        topics: Key topics discussed (e.g., ["vault-rag", "MCP tools", "embeddings"])
        key_facts: Notable facts/decisions (e.g., ["decided to use SQLite FTS5", "user prefers manual cp for dotfiles"])
        word_count: Approximate word count of the session
        message_count: Number of messages in the session

    Returns:
        JSON with session_id and confirmation
    """
    db = get_db()

    session_id = db.save_session(
        summary=summary,
        topics=topics,
        key_facts=key_facts,
        word_count=word_count,
        message_count=message_count
    )

    # Sessions are searchable via FTS5 (search_sessions, get_recent_sessions).
    # Not embedded into ChromaDB — session summaries have low semantic density
    # and dilute document search quality in hybrid/semantic results.

    return json.dumps({
        "status": "saved",
        "session_id": session_id,
        "summary_preview": summary[:100] + "..." if len(summary) > 100 else summary,
        "chunks_embedded": 0
    }, indent=2)


@mcp.tool()
def get_recent_sessions(limit: int = 5) -> str:
    """
    Retrieve recent session summaries for context.

    Use this at the start of a new session to understand what was
    previously discussed with the user.

    Args:
        limit: Maximum number of sessions to retrieve (default: 5)

    Returns:
        JSON array of recent sessions with summaries, topics, and key facts
    """
    db = get_db()
    sessions = db.get_recent_sessions(limit)

    return json.dumps([
        {
            "session_id": s.id,
            "created_at": s.created_at,
            "summary": s.summary,
            "topics": s.topics,
            "key_facts": s.key_facts,
            "word_count": s.word_count,
            "message_count": s.message_count
        }
        for s in sessions
    ], indent=2)


@mcp.tool()
def search_sessions(
    query: str,
    limit: int = 10,
    recency_bias: float = 0.0
) -> str:
    """
    Search past sessions by content.

    Useful for finding sessions where specific topics were discussed,
    or resuming recent work.

    Args:
        query: Search query (FTS5 syntax supported)
        limit: Maximum results (default: 10)
        recency_bias: Temporal recency weight 0.0-1.0 (default: 0.0)
                     - 0.0: Pure topic search (BM25 only)
                     - 0.7: Resumption queries (favor recent sessions)
                     - 0.3-0.5: Balanced (recent + relevant)

    Returns:
        JSON array of matching sessions with snippets and scores

    Examples:
        search_sessions("where were we", recency_bias=0.7)  # Resumption
        search_sessions("emacs config", recency_bias=0.0)   # Topic search
    """
    db = get_db()
    results = db.search_sessions_fts(query, limit, recency_bias)

    return json.dumps([
        {
            "session_id": r.session_id,
            "summary": r.summary,
            "snippet": r.snippet,
            "score": r.score,
            "created_at": r.created_at,
            "topics": r.topics
        }
        for r in results
    ], indent=2)


@mcp.tool()
def append_ledger_event(
    event_type: str,
    event_id: str,
    description: Optional[str] = None,
    context: Optional[str] = None,
    trigger: Optional[str] = None,
    parent_task: Optional[str] = None,
    resolution: Optional[str] = None,
    question: Optional[str] = None,
    choice: Optional[str] = None,
    rationale: Optional[str] = None,
    output: Optional[str] = None,
    priority: Optional[str] = None,
) -> str:
    """
    Append event to WORK_LEDGER.jsonl for tracking tasks, tangents, loose ends.

    Event types:
    - task_start: New work item (requires description, optional context JSON)
    - task_complete: Work item finished (optional output)
    - tangent_begin: Context shift (requires trigger, optional parent_task)
    - tangent_end: Return to primary task (optional resolution)
    - decision: Choice point (requires question, choice, optional rationale)
    - loose_end: Unresolved thread (requires description, priority)
    - question: Open question (requires question)
    - blocker: Impediment (requires description)

    Args:
        event_type: Event type (see above)
        event_id: Unique ID (T1, TG1, D1, LE1, Q1, B1)
        description: Task/blocker description
        context: Context JSON string (e.g., '{"plan":"file.md","phase":1}')
        trigger: Tangent trigger
        parent_task: Parent task ID
        resolution: Tangent resolution
        question: Question text
        choice: Decision choice
        rationale: Decision rationale
        output: Task output
        priority: Priority (high/medium/low)

    Returns:
        JSON confirmation with event details
    """
    from vault_rag import ledger

    # Parse context JSON if provided
    context_dict = None
    if context:
        try:
            context_dict = json.loads(context)
        except json.JSONDecodeError as e:
            return json.dumps({"error": f"Invalid context JSON: {e}"})

    try:
        event = ledger.append_event(
            event_type=event_type,
            event_id=event_id,
            description=description,
            context=context_dict,
            trigger=trigger,
            parent_task=parent_task,
            resolution=resolution,
            question=question,
            choice=choice,
            rationale=rationale,
            output=output,
            priority=priority,
        )
        return json.dumps({"success": True, "event": event}, indent=2)
    except Exception as e:
        return json.dumps({"error": str(e)}, indent=2)


@mcp.tool()
def reassemble_loose_ends(
    lookback_days: int = 10,
    include_completed: bool = False,
    priority_filter: Optional[str] = None
) -> str:
    """
    Reconstruct work context from recent sessions and structured work items.

    Uses indexed sessions as primary content source, augmented with
    WORK_LEDGER structured annotations (tasks, loose ends, questions, blockers).

    Args:
        lookback_days: Number of days to look back (default: 10)
        include_completed: Include completed tasks (default: False)
        priority_filter: Filter loose ends by priority (high/medium/low)

    Returns:
        JSON with:
        - sessions: Recent session summaries with topics and key facts
        - active_tasks: Tasks from WORK_LEDGER (started but not completed)
        - loose_ends: Unresolved threads from WORK_LEDGER
        - open_questions: Questions from WORK_LEDGER
        - blockers: Impediments from WORK_LEDGER
        - tangent_stack: Recent tangents from WORK_LEDGER
        - key_topics: Aggregated topics across all sessions
        - key_facts: Aggregated facts/decisions across all sessions
    """
    from vault_rag import ledger
    from collections import Counter

    try:
        db = get_db()

        # PRIMARY: Get all indexed sessions from past N days
        recent_sessions = db.get_sessions_since(lookback_days)

        # Aggregate session content
        all_topics = []
        all_facts = []
        session_summaries = []

        for s in recent_sessions:
            session_summaries.append({
                "session_id": s.id,
                "created_at": s.created_at,
                "summary": s.summary if s.summary else "No summary",
                "topics": s.topics if s.topics else [],
                "key_facts": s.key_facts if s.key_facts else [],
                "message_count": s.message_count,
                "word_count": s.word_count
            })

            if s.topics:
                all_topics.extend(s.topics)
            if s.key_facts:
                all_facts.extend(s.key_facts)

        # Count topic frequency
        topic_counts = Counter(all_topics)
        top_topics = [{"topic": t, "count": c} for t, c in topic_counts.most_common(10)]

        # SECONDARY: Read WORK_LEDGER for structured annotations
        # Don't filter by session - get ALL recent events from the ledger
        from datetime import datetime, timedelta
        cutoff = (datetime.now() - timedelta(days=lookback_days)).isoformat()
        all_events = ledger.read_events()  # Get all events

        # Filter events by timestamp
        recent_events = [
            e for e in all_events
            if e.get("timestamp", "") >= cutoff
        ]

        # Build structured items from ledger
        tasks = {}
        tangents = {}
        loose_ends_list = []
        questions_list = []
        blockers_list = []

        for event in recent_events:
            etype = event["type"]
            eid = event["id"]

            if etype == "task_start":
                tasks[eid] = {
                    "id": eid,
                    "description": event.get("description"),
                    "status": "in_progress",
                    "started": event["timestamp"],
                    "session": event["session"],
                    "context": event.get("context", {})
                }
            elif etype == "task_complete":
                if eid in tasks:
                    if not include_completed:
                        del tasks[eid]
                    else:
                        tasks[eid]["status"] = "completed"
                        tasks[eid]["completed"] = event["timestamp"]
                        tasks[eid]["output"] = event.get("output")
            elif etype == "tangent_begin":
                tangents[eid] = {
                    "id": eid,
                    "trigger": event.get("trigger"),
                    "started": event["timestamp"],
                    "parent_task": event.get("parent_task")
                }
            elif etype == "tangent_end":
                if eid in tangents:
                    tangents[eid]["ended"] = event["timestamp"]
                    tangents[eid]["resolution"] = event.get("resolution")
            elif etype == "loose_end":
                loose_end = {
                    "id": eid,
                    "description": event.get("description"),
                    "priority": event.get("priority"),
                    "created": event["timestamp"],
                    "session": event["session"]
                }
                if not priority_filter or loose_end["priority"] == priority_filter:
                    loose_ends_list.append(loose_end)
            elif etype == "question":
                questions_list.append({
                    "id": eid,
                    "question": event.get("question"),
                    "created": event["timestamp"],
                    "session": event["session"]
                })
            elif etype == "blocker":
                blockers_list.append({
                    "id": eid,
                    "description": event.get("description"),
                    "created": event["timestamp"],
                    "session": event["session"]
                })

        # Sort loose ends by priority
        priority_order = {"high": 0, "medium": 1, "low": 2}
        loose_ends_list.sort(
            key=lambda x: (priority_order.get(x["priority"], 3), x["created"]),
            reverse=False
        )

        return json.dumps({
            "sessions": session_summaries,
            "key_topics": top_topics,
            "key_facts": all_facts,
            "active_tasks": list(tasks.values()),
            "loose_ends": loose_ends_list,
            "open_questions": questions_list,
            "blockers": blockers_list,
            "tangent_stack": list(tangents.values()),
            "session_count": len(recent_sessions),
            "ledger_event_count": len(recent_events)
        }, indent=2)

    except Exception as e:
        return json.dumps({"error": str(e)}, indent=2)


@mcp.tool()
def query_e2e_history(
    feature: Optional[str] = None,
    story: Optional[str] = None,
    since: Optional[str] = None,
    limit: int = 20
) -> str:
    """
    Query structured e2e test history from e2e_results table.

    Replaces regex-based parsing with direct SQL queries for better performance
    and reliability. Returns test results, regressions, and flakiness data.

    Args:
        feature: Filter by feature name (e.g., "somatic", "vault-rag")
        story: Filter by story name (e.g., "core-input-capture")
        since: ISO date string to filter from (e.g., "2026-01-28")
        limit: Maximum test runs to include (default: 20)

    Returns:
        JSON with structured e2e results, regressions, and summary stats
    """
    db = get_db()

    # Build query for e2e_results
    query = '''
        SELECT r.id, r.run_id, r.feature, r.story, r.result,
               r.started_at, r.completed_at, r.duration_ms,
               r.error_message, r.expected, r.actual,
               run.total_stories, run.passed, run.failed, run.blocked
        FROM e2e_results r
        LEFT JOIN e2e_test_runs run ON r.run_id = run.run_id
        WHERE 1=1
    '''
    params = []

    if feature:
        query += ' AND r.feature = ?'
        params.append(feature)
    if story:
        query += ' AND r.story = ?'
        params.append(story)
    if since:
        query += ' AND r.started_at >= ?'
        params.append(since)

    query += ' ORDER BY r.started_at DESC LIMIT ?'
    params.append(limit * 10)  # Get more results to group by run

    # Execute query
    with db._conn() as conn:
        results = conn.execute(query, params).fetchall()

    # Group by run_id
    runs_dict = {}
    story_history = {}  # track per-story results for flakiness detection

    for r in results:
        run_id = r['run_id']
        if run_id not in runs_dict:
            runs_dict[run_id] = {
                "run_id": run_id,
                "started_at": r['started_at'],
                "total_stories": r['total_stories'],
                "passed": r['passed'],
                "failed": r['failed'],
                "blocked": r['blocked'],
                "stories": []
            }

        story_entry = {
            "feature": r['feature'],
            "story": r['story'],
            "result": r['result'],
            "duration_ms": r['duration_ms'],
            "error_message": r['error_message'] or "",
            "expected": r['expected'] or "",
            "actual": r['actual'] or ""
        }
        runs_dict[run_id]["stories"].append(story_entry)

        # Track story history for flakiness/regression detection
        key = f"{r['feature']}:{r['story']}"
        if key not in story_history:
            story_history[key] = []
        story_history[key].append({
            "date": r['started_at'],
            "result": r['result']
        })

    runs = list(runs_dict.values())[:limit]  # Limit final runs

    # Detect regressions and flaky tests
    regressions = []
    flaky = []

    for key, history in story_history.items():
        # Sort by date
        history.sort(key=lambda x: x['date'])

        # Check for flakiness (inconsistent results)
        results_set = set(h["result"] for h in history)
        if len(results_set) > 1:
            flaky.append({
                "story": key,
                "results": [h["result"] for h in history],
                "run_count": len(history)
            })

        # Check for regressions/fixes (last 2 runs)
        if len(history) >= 2:
            prev, curr = history[-2]["result"], history[-1]["result"]
            if prev == "PASS" and curr == "FAIL":
                regressions.append({
                    "story": key,
                    "type": "regression",
                    "date": history[-1]["date"]
                })
            elif prev == "FAIL" and curr == "PASS":
                regressions.append({
                    "story": key,
                    "type": "fix",
                    "date": history[-1]["date"]
                })

    return json.dumps({
        "runs": runs,
        "regressions": regressions,
        "flaky_tests": flaky,
        "total_runs": len(runs),
        "unique_stories": len(story_history)
    }, indent=2)


@mcp.tool()
def query_e2e_failures(
    feature: Optional[str] = None,
    story: Optional[str] = None,
    since: Optional[str] = None,
    limit: int = 50
) -> str:
    """
    Query e2e test failures with detailed error information.

    Args:
        feature: Filter by feature name (e.g., "somatic")
        story: Filter by specific story
        since: ISO date or relative time like '-24 hours'
        limit: Maximum failures to return (default: 50)

    Returns:
        JSON array of failures with timestamps, errors, and expected/actual values
    """
    db = get_db()

    query = '''
        SELECT r.feature, r.story, r.started_at, r.duration_ms,
               r.error_message, r.expected, r.actual, r.stack_trace,
               run.run_id, run.git_commit
        FROM e2e_results r
        LEFT JOIN e2e_test_runs run ON r.run_id = run.run_id
        WHERE r.result = 'FAIL'
    '''
    params = []

    if feature:
        query += ' AND r.feature = ?'
        params.append(feature)
    if story:
        query += ' AND r.story = ?'
        params.append(story)
    if since:
        if since.startswith('-'):
            # Relative time not supported in SQLite directly, use simple date comparison
            query += ' AND r.started_at > ?'
            from datetime import datetime, timedelta
            if 'hour' in since:
                hours = int(since.split()[0].replace('-', ''))
                cutoff = (datetime.now() - timedelta(hours=hours)).isoformat()
            elif 'day' in since:
                days = int(since.split()[0].replace('-', ''))
                cutoff = (datetime.now() - timedelta(days=days)).isoformat()
            else:
                cutoff = since
            params.append(cutoff)
        else:
            query += ' AND r.started_at > ?'
            params.append(since)

    query += ' ORDER BY r.started_at DESC LIMIT ?'
    params.append(limit)

    with db._conn() as conn:
        results = conn.execute(query, params).fetchall()

    failures = []
    for r in results:
        failures.append({
            "feature": r['feature'],
            "story": r['story'],
            "started_at": r['started_at'],
            "duration_ms": r['duration_ms'],
            "error_message": r['error_message'] or "",
            "expected": r['expected'] or "",
            "actual": r['actual'] or "",
            "run_id": r['run_id'],
            "git_commit": r['git_commit'] or ""
        })

    return json.dumps({"failures": failures, "total": len(failures)}, indent=2)


@mcp.tool()
def query_e2e_test_runs(
    since: Optional[str] = None,
    limit: int = 20
) -> str:
    """
    Query test run history with aggregated results and pass rates.

    Args:
        since: ISO date or relative time like '-7 days'
        limit: Maximum runs to return (default: 20)

    Returns:
        JSON array of test runs with timestamps, pass rates, and stats
    """
    db = get_db()

    query = '''
        SELECT run_id, started_at, completed_at,
               total_stories, passed, failed, blocked, skipped,
               trigger, git_commit, regressions_detected, fixes_detected
        FROM e2e_test_runs
        WHERE completed_at IS NOT NULL
    '''
    params = []

    if since:
        if since.startswith('-'):
            from datetime import datetime, timedelta
            if 'day' in since:
                days = int(since.split()[0].replace('-', ''))
                cutoff = (datetime.now() - timedelta(days=days)).isoformat()
            else:
                cutoff = since
            params.append(cutoff)
        else:
            params.append(since)
        query += ' AND started_at > ?'

    query += ' ORDER BY started_at DESC LIMIT ?'
    params.append(limit)

    with db._conn() as conn:
        results = conn.execute(query, params).fetchall()

    runs = []
    for r in results:
        total = r['total_stories'] or 0
        passed = r['passed'] or 0
        pass_rate = round(100.0 * passed / total, 2) if total > 0 else 0.0

        runs.append({
            "run_id": r['run_id'],
            "started_at": r['started_at'],
            "completed_at": r['completed_at'],
            "total_stories": total,
            "passed": passed,
            "failed": r['failed'] or 0,
            "blocked": r['blocked'] or 0,
            "skipped": r['skipped'] or 0,
            "pass_rate": pass_rate,
            "trigger": r['trigger'] or "unknown",
            "git_commit": r['git_commit'] or "",
            "regressions": r['regressions_detected'] or 0,
            "fixes": r['fixes_detected'] or 0
        })

    return json.dumps({"runs": runs, "total": len(runs)}, indent=2)


@mcp.tool()
def detect_e2e_regressions(
    feature: str,
    lookback_hours: int = 24
) -> str:
    """
    Detect stories that were PASS but are now FAIL (regressions).

    Args:
        feature: Feature to check (e.g., "somatic")
        lookback_hours: Hours to look back (default: 24)

    Returns:
        JSON array of regressions with expected/actual comparisons
    """
    db = get_db()

    from datetime import datetime, timedelta
    cutoff = (datetime.now() - timedelta(hours=lookback_hours)).isoformat()

    query = '''
    WITH latest_results AS (
        SELECT story, result, started_at, error_message, expected, actual,
               ROW_NUMBER() OVER (PARTITION BY story ORDER BY started_at DESC) as rn
        FROM e2e_results
        WHERE feature = ? AND started_at > ?
    )
    SELECT a.story,
           a.started_at as failed_at,
           b.started_at as last_passed,
           a.error_message,
           a.expected,
           a.actual
    FROM latest_results a
    JOIN latest_results b ON a.story = b.story
    WHERE a.rn = 1 AND a.result = 'FAIL'
      AND b.rn = 2 AND b.result = 'PASS'
    '''

    with db._conn() as conn:
        results = conn.execute(query, [feature, cutoff]).fetchall()

    regressions = []
    for r in results:
        regressions.append({
            "story": r['story'],
            "failed_at": r['failed_at'],
            "last_passed": r['last_passed'],
            "error": r['error_message'] or "",
            "expected": r['expected'] or "",
            "actual": r['actual'] or ""
        })

    return json.dumps({"regressions": regressions, "count": len(regressions)}, indent=2)


@mcp.tool()
def detect_e2e_flakiness(
    feature: str,
    min_runs: int = 5,
    lookback_days: int = 7
) -> str:
    """
    Detect flaky tests (inconsistent results across multiple runs).

    Args:
        feature: Feature to analyze (e.g., "somatic")
        min_runs: Minimum runs to consider (default: 5)
        lookback_days: Days to look back (default: 7)

    Returns:
        JSON array of flaky tests with result sequences
    """
    db = get_db()

    from datetime import datetime, timedelta
    cutoff = (datetime.now() - timedelta(days=lookback_days)).isoformat()

    query = '''
    WITH story_results AS (
        SELECT story, result, started_at,
               ROW_NUMBER() OVER (PARTITION BY story ORDER BY started_at DESC) as rn
        FROM e2e_results
        WHERE feature = ? AND started_at > ?
    ),
    story_stats AS (
        SELECT story,
               COUNT(*) as run_count,
               COUNT(DISTINCT result) as unique_results,
               GROUP_CONCAT(result) as result_sequence
        FROM story_results
        WHERE rn <= ?
        GROUP BY story
    )
    SELECT story, run_count, unique_results, result_sequence
    FROM story_stats
    WHERE unique_results > 1
    ORDER BY unique_results DESC, run_count DESC
    '''

    with db._conn() as conn:
        results = conn.execute(query, [feature, cutoff, min_runs]).fetchall()

    flaky = []
    for r in results:
        flaky.append({
            "story": r['story'],
            "run_count": r['run_count'],
            "unique_results": r['unique_results'],
            "result_sequence": r['result_sequence']
        })

    return json.dumps({"flaky_tests": flaky, "count": len(flaky)}, indent=2)


@mcp.tool()
def get_e2e_quantitative_trends(
    feature: str,
    story: str,
    lookback_days: int = 30
) -> str:
    """
    Get quantitative metrics trends for a specific story (duration, anomalies, events).

    Args:
        feature: Feature name (e.g., "somatic")
        story: Story name (e.g., "core-input-capture")
        lookback_days: Days to analyze (default: 30)

    Returns:
        JSON array of metrics over time
    """
    db = get_db()

    from datetime import datetime, timedelta
    cutoff = (datetime.now() - timedelta(days=lookback_days)).isoformat()

    query = '''
    SELECT started_at, result, duration_ms,
           geometry_anomaly_count, x11_event_count,
           attention_shifts, retry_count
    FROM e2e_results
    WHERE feature = ? AND story = ? AND started_at > ?
    ORDER BY started_at ASC
    '''

    with db._conn() as conn:
        results = conn.execute(query, [feature, story, cutoff]).fetchall()

    trends = []
    for r in results:
        trends.append({
            "timestamp": r['started_at'],
            "result": r['result'],
            "duration_ms": r['duration_ms'] or 0,
            "geometry_anomalies": r['geometry_anomaly_count'] or 0,
            "x11_events": r['x11_event_count'] or 0,
            "attention_shifts": r['attention_shifts'] or 0,
            "retries": r['retry_count'] or 0
        })

    return json.dumps({"story": f"{feature}:{story}", "trends": trends, "data_points": len(trends)}, indent=2)


@mcp.tool()
def index_file(
    source_path: str,
    force: bool = False
) -> str:
    """
    Index or re-index a specific file.

    Args:
        source_path: Relative path from vault root (e.g., "org/journal/2026-01-20.org")
        force: Re-index even if content unchanged (default: False)

    Returns:
        Indexing result summary
    """
    from vault_rag.indexer.chunker import chunk_org_document, chunk_text, strip_org_metadata

    db = get_db()
    chroma = get_chroma()
    embedder = get_embedder()

    full_path = VAULT_ROOT / source_path
    if not full_path.exists():
        return json.dumps({"error": f"File not found: {source_path}"})

    content = full_path.read_text(encoding="utf-8")
    content_hash = hashlib.sha256(content.encode()).hexdigest()

    # Check if needs re-indexing
    existing = db.get_document_by_path(source_path)
    if existing and existing.content_hash == content_hash and not force:
        return json.dumps({"status": "unchanged", "source_path": source_path})

    # Determine source type and title
    if source_path.startswith("org/conversations/chatgpt"):
        source_type = "chatgpt"
    elif source_path.startswith("org/conversations/onenote"):
        source_type = "onenote"
    else:
        source_type = "org"

    # Extract title from org file
    title = None
    for line in content.split("\n")[:10]:
        if line.startswith("#+TITLE:"):
            title = line[8:].strip()
            break

    # Insert/update document
    doc_id = db.upsert_document(
        source_type=source_type,
        source_path=source_path,
        content_hash=content_hash,
        title=title,
        word_count=len(content.split()),
        full_text=content
    )

    # Delete old chunks
    db.delete_chunks_for_doc(doc_id)
    chroma.delete_by_doc_id(doc_id)

    # Chunk document
    if source_path.endswith(".org"):
        chunks = list(chunk_org_document(content))
    else:
        chunks = list(chunk_text(content))

    if not chunks:
        return json.dumps({
            "status": "indexed",
            "source_path": source_path,
            "chunks": 0
        })

    # Populate session_id in chunk metadata if this is a conversation file
    if source_path.startswith("org/conversations/"):
        # Extract session date from path: org/conversations/claude/_home_bw/2026-01/session-name.org
        # or: org/conversations/chatgpt/2025-12/session-name.org
        # Sessions are indexed by date, so we can find the session by looking for sessions
        # created on the date in the path
        import re as regex_module
        date_match = regex_module.search(r'/(\d{4})-(\d{2})/', source_path)
        if date_match:
            year, month = date_match.groups()
            # Get sessions from that month
            with db._conn() as conn:
                sessions_in_month = conn.execute("""
                    SELECT id, created_at, summary
                    FROM sessions
                    WHERE strftime('%Y-%m', created_at) = ?
                    ORDER BY created_at DESC
                """, (f"{year}-{month}",)).fetchall()

            # Try to match session by finding the most recently created session
            # that could correspond to this file (within a day of file creation)
            # This is heuristic-based since we don't have a direct session_id in the filename
            if sessions_in_month:
                # Use the most recent session from that month as a best guess
                # TODO: Improve session matching logic with:
                # - File creation timestamp comparison
                # - Session summary/topic matching with file title
                # - Better session ID extraction from source files
                session_id = sessions_in_month[0]["id"]

                # Add session_id to all chunk metadata
                for chunk in chunks:
                    if chunk.metadata is None:
                        chunk.metadata = {}
                    chunk.metadata["session_id"] = session_id

    # Generate embeddings and store in batches
    from vault_rag.indexer.embedder import Embedder as _Emb

    # Process chunks in batches to avoid overwhelming vector DB
    for batch_start in range(0, len(chunks), EMBEDDING_BATCH_SIZE):
        batch_end = min(batch_start + EMBEDDING_BATCH_SIZE, len(chunks))
        batch_chunks = chunks[batch_start:batch_end]

        # Prepare batch for embedding (strip org metadata for cleaner vectors)
        if source_path.endswith(".org"):
            embed_texts = [strip_org_metadata(c.content) for c in batch_chunks]
        else:
            embed_texts = [c.content for c in batch_chunks]
        embeddings = embedder.embed_batch_with_cache(embed_texts)

        # Prepare batch metadata
        chroma_ids = []
        chroma_docs = []
        chroma_metas = []

        for batch_idx, (chunk, embedding) in enumerate(zip(batch_chunks, embeddings)):
            chunk_index = batch_start + batch_idx
            chroma_id = f"doc{doc_id}_chunk{chunk_index}"
            chroma_ids.append(chroma_id)
            chroma_docs.append(chunk.content)
            chroma_metas.append({
                "doc_id": doc_id,
                "source_path": source_path,
                "source_type": source_type,
                "chunk_index": chunk_index,
                "heading_context": chunk.heading_context
            })

            # Add chunk to SQLite
            db.add_chunk(
                doc_id=doc_id,
                chunk_index=chunk_index,
                content=chunk.content,
                content_hash=_Emb.content_hash(chunk.content),
                chroma_id=chroma_id,
                heading_context=chunk.heading_context,
                start_offset=chunk.start_offset,
                end_offset=chunk.end_offset,
                metadata=chunk.metadata
            )

        # Upsert batch to ChromaDB
        chroma.upsert_chunks(
            ids=chroma_ids,
            embeddings=embeddings,
            documents=chroma_docs,
            metadatas=chroma_metas
        )

    return json.dumps({
        "status": "indexed",
        "source_path": source_path,
        "doc_id": doc_id,
        "chunks": len(chunks)
    })


# =============================================================================
# CLAUDE SOURCE TOOLS
# =============================================================================

@mcp.tool()
def add_claude_source(
    path: str,
    name: Optional[str] = None
) -> str:
    """
    Add a Claude Code conversation source directory to watch.

    This registers a path (typically ~/.claude/projects) for importing
    Claude Code conversations into the vault for search and retrieval.

    Args:
        path: Path to Claude projects directory (e.g., "~/.claude/projects")
        name: Optional friendly name (e.g., "Claude Code")

    Returns:
        JSON with source_id and confirmation
    """
    db = get_db()

    # Expand path
    expanded_path = str(Path(path).expanduser().resolve())

    # Verify path exists
    if not Path(expanded_path).exists():
        return json.dumps({
            "error": f"Path does not exist: {expanded_path}"
        })

    source_id = db.add_claude_source(expanded_path, name)

    return json.dumps({
        "status": "added",
        "source_id": source_id,
        "path": expanded_path,
        "name": name
    }, indent=2)


@mcp.tool()
def remove_claude_source(path: str) -> str:
    """
    Remove a Claude Code conversation source.

    Stops watching the path for new conversations.
    Does not delete already-imported conversations from the vault.

    Args:
        path: Path to remove (e.g., "~/.claude/projects")

    Returns:
        JSON with confirmation
    """
    db = get_db()

    # Expand path
    expanded_path = str(Path(path).expanduser().resolve())

    removed = db.remove_claude_source(expanded_path)

    if removed:
        return json.dumps({
            "status": "removed",
            "path": expanded_path
        }, indent=2)
    else:
        return json.dumps({
            "error": f"Source not found: {expanded_path}"
        })


@mcp.tool()
def list_claude_sources() -> str:
    """
    List all registered Claude Code conversation sources.

    Shows all watched paths with their sync status and conversation counts.

    Returns:
        JSON array of sources with path, name, enabled, last_sync, conversation_count
    """
    db = get_db()
    sources = db.list_claude_sources()

    return json.dumps(sources, indent=2)


@mcp.tool()
def sync_claude(source_id: Optional[int] = None) -> str:
    """
    Sync Claude Code conversations from registered sources.

    Scans watched paths for new or changed .jsonl conversation files,
    parses them, converts to org format, and indexes for search.

    Args:
        source_id: Optional specific source to sync (default: all enabled sources)

    Returns:
        JSON with sync results (new, updated, unchanged counts)
    """
    from vault_rag.importers.claude import ClaudeImporter
    from vault_rag.indexer.chunker import chunk_org_document, strip_org_metadata
    from vault_rag.indexer.embedder import Embedder as _Emb

    db = get_db()
    chroma = get_chroma()
    embedder = get_embedder()

    # Get sources to sync
    if source_id:
        source = db.get_claude_source_by_id(source_id)
        if not source:
            return json.dumps({"error": f"Source not found: {source_id}"})
        sources = [source]
    else:
        sources = db.list_claude_sources()
        sources = [s for s in sources if s.get("enabled")]

    if not sources:
        return json.dumps({"error": "No enabled sources found. Add one with add_claude_source."})

    results = {
        "sources_synced": 0,
        "new": 0,
        "updated": 0,
        "unchanged": 0,
        "errors": []
    }

    # Output directory for converted org files
    claude_org_dir = ORG_DIR / "conversations" / "claude"
    claude_org_dir.mkdir(parents=True, exist_ok=True)

    for source in sources:
        source_path = Path(source["path"])
        if not source_path.exists():
            results["errors"].append(f"Source path not found: {source['path']}")
            continue

        importer = ClaudeImporter(source_path)
        results["sources_synced"] += 1

        for jsonl_path in importer.find_conversations():
            try:
                # Compute content hash
                content = jsonl_path.read_text(encoding="utf-8")
                content_hash = hashlib.sha256(content.encode()).hexdigest()

                session_id = jsonl_path.stem
                project = jsonl_path.parent.name

                # Check if already imported with same hash
                existing = db.get_claude_conversation(source["id"], session_id)
                if existing and existing["content_hash"] == content_hash:
                    results["unchanged"] += 1
                    continue

                # Parse conversation
                conv = importer.parse_conversation(jsonl_path)
                if not conv or not conv.messages:
                    continue

                # Convert to org
                org_content = importer.to_org(conv)

                # Determine output path
                output_path = importer.get_output_path(conv, claude_org_dir)
                output_path.parent.mkdir(parents=True, exist_ok=True)
                output_path.write_text(org_content, encoding="utf-8")

                # Get relative path for vault storage
                source_path_rel = str(output_path.relative_to(VAULT_ROOT))

                # Index the document
                org_hash = hashlib.sha256(org_content.encode()).hexdigest()

                doc_id = db.upsert_document(
                    source_type="claude",
                    source_path=source_path_rel,
                    content_hash=org_hash,
                    title=conv.title,
                    created_at=conv.created_at.isoformat() if conv.created_at else None,
                    updated_at=conv.updated_at.isoformat() if conv.updated_at else None,
                    word_count=len(org_content.split()),
                    metadata={
                        "session_id": conv.id,
                        "project": conv.project,
                        "model": conv.model,
                        "message_count": len(conv.messages)
                    },
                    full_text=org_content
                )

                # Track the conversation first (before chunking/embedding)
                conv_id = db.upsert_claude_conversation(
                    source_id=source["id"],
                    session_id=session_id,
                    project=project,
                    content_hash=content_hash,
                    doc_id=doc_id
                )

                # Insert messages (delete old messages if re-importing)
                # Do this BEFORE chunking/embedding so messages are preserved even if embedding fails
                if existing:
                    db.delete_messages_for_conversation(conv_id)

                MESSAGE_BATCH_SIZE = 1000
                for batch_start in range(0, len(conv.messages), MESSAGE_BATCH_SIZE):
                    batch_end = min(batch_start + MESSAGE_BATCH_SIZE, len(conv.messages))
                    batch_messages = conv.messages[batch_start:batch_end]

                    for msg_idx, msg in enumerate(batch_messages):
                        db.add_claude_message(
                            claude_conv_id=conv_id,
                            uuid=msg.id,
                            session_id=conv.id,
                            message_index=batch_start + msg_idx,
                            role=msg.role,
                            content=msg.content,
                            content_blocks=json.dumps(msg.raw_content) if msg.raw_content else None,
                            created_at=msg.created_at.isoformat() if msg.created_at else None,
                            model=msg.model,
                            parent_uuid=msg.parent_uuid
                        )

                # Chunk and embed (wrapped in try/except to not block message insertion)
                try:
                    # Delete old chunks if updating
                    if existing:
                        db.delete_chunks_for_doc(doc_id)
                        chroma.delete_by_doc_id(doc_id)

                    # Chunk and embed (strip org metadata for cleaner vectors)
                    chunks = list(chunk_org_document(org_content))
                    if chunks:
                        # Process chunks in batches to avoid overwhelming vector DB
                        for batch_start in range(0, len(chunks), EMBEDDING_BATCH_SIZE):
                            batch_end = min(batch_start + EMBEDDING_BATCH_SIZE, len(chunks))
                            batch_chunks = chunks[batch_start:batch_end]

                            # Prepare batch for embedding
                            embed_texts = [strip_org_metadata(c.content) for c in batch_chunks]
                            embeddings = embedder.embed_batch_with_cache(embed_texts)

                            # Prepare batch metadata
                            chroma_ids = []
                            chroma_docs = []
                            chroma_metas = []

                            for batch_idx, (chunk, embedding) in enumerate(zip(batch_chunks, embeddings)):
                                chunk_index = batch_start + batch_idx
                                chroma_id = f"doc{doc_id}_chunk{chunk_index}"
                                chroma_ids.append(chroma_id)
                                chroma_docs.append(chunk.content)
                                chroma_metas.append({
                                    "doc_id": doc_id,
                                    "source_path": source_path_rel,
                                    "source_type": "claude",
                                    "chunk_index": chunk_index,
                                    "heading_context": chunk.heading_context
                                })

                                # Add chunk to SQLite
                                db.add_chunk(
                                    doc_id=doc_id,
                                    chunk_index=chunk_index,
                                    content=chunk.content,
                                    content_hash=_Emb.content_hash(chunk.content),
                                    chroma_id=chroma_id,
                                    heading_context=chunk.heading_context,
                                    start_offset=chunk.start_offset,
                                    end_offset=chunk.end_offset,
                                    metadata=chunk.metadata
                                )

                            # Upsert batch to ChromaDB
                            chroma.upsert_chunks(
                                ids=chroma_ids,
                                embeddings=embeddings,
                                documents=chroma_docs,
                                metadatas=chroma_metas
                            )
                except Exception as embed_error:
                    results["errors"].append(f"Embedding error for {jsonl_path.name}: {str(embed_error)}")

                if existing:
                    results["updated"] += 1
                else:
                    results["new"] += 1

            except Exception as e:
                results["errors"].append(f"Error processing {jsonl_path.name}: {str(e)}")

        # Update sync timestamp
        db.update_claude_source_sync(source["id"])

    return json.dumps(results, indent=2)


# =============================================================================
# CLUSTER TOOLS
# =============================================================================

@mcp.tool()
def list_clusters(
    min_size: int = 10,
    include_uncategorized: bool = False
) -> str:
    """
    List all topic clusters with labels and document counts.

    Shows clusters generated by UMAP + HDBSCAN clustering of document embeddings.
    Each cluster represents a topic theme in your knowledge base.

    Args:
        min_size: Minimum chunk count to include (default: 10)
        include_uncategorized: Include the "Uncategorized" cluster (default: False)

    Returns:
        JSON array of clusters with id, label, description, keywords, chunk_count
    """
    db = get_db()

    with db._conn() as conn:
        # Check if clusters table exists
        table_check = conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='clusters'"
        ).fetchone()

        if not table_check:
            return json.dumps({
                "error": "Clusters not generated yet. Run scripts/cluster.py first."
            })

        query = """
            SELECT id, label, description, keywords, chunk_count, created_at
            FROM clusters
            WHERE chunk_count >= ?
        """
        params = [min_size]

        if not include_uncategorized:
            query += " AND id >= 0"

        query += " ORDER BY chunk_count DESC"

        rows = conn.execute(query, params).fetchall()

    clusters = []
    for row in rows:
        clusters.append({
            "cluster_id": row["id"],
            "label": row["label"],
            "description": row["description"],
            "keywords": json.loads(row["keywords"]) if row["keywords"] else [],
            "chunk_count": row["chunk_count"],
            "created_at": row["created_at"]
        })

    return json.dumps(clusters, indent=2)


@mcp.tool()
def get_cluster(
    cluster_id: int,
    limit: int = 20
) -> str:
    """
    Get documents in a specific cluster.

    Args:
        cluster_id: The cluster ID (from list_clusters)
        limit: Maximum documents to return (default: 20)

    Returns:
        JSON with cluster info and list of documents with snippets
    """
    db = get_db()

    with db._conn() as conn:
        # Get cluster info
        cluster_row = conn.execute(
            "SELECT * FROM clusters WHERE id = ?", (cluster_id,)
        ).fetchone()

        if not cluster_row:
            return json.dumps({"error": f"Cluster {cluster_id} not found"})

        cluster_info = {
            "cluster_id": cluster_row["id"],
            "label": cluster_row["label"],
            "description": cluster_row["description"],
            "keywords": json.loads(cluster_row["keywords"]) if cluster_row["keywords"] else [],
            "chunk_count": cluster_row["chunk_count"]
        }

        # Get documents in this cluster
        # Join through chunk_clusters -> chunks -> documents using chroma_id
        rows = conn.execute("""
            SELECT DISTINCT d.id as doc_id, d.title, d.source_path, d.source_type,
                   d.created_at, c.content as sample_content, c.heading_context
            FROM chunk_clusters cc
            JOIN chunks c ON cc.chroma_id = c.chroma_id
            JOIN documents d ON c.doc_id = d.id
            WHERE cc.cluster_id = ?
            ORDER BY d.created_at DESC
            LIMIT ?
        """, (cluster_id, limit)).fetchall()

        documents = []
        seen_docs = set()
        for row in rows:
            if row["doc_id"] in seen_docs:
                continue
            seen_docs.add(row["doc_id"])

            content = row["sample_content"] or ""
            snippet = content[:200] + "..." if len(content) > 200 else content

            documents.append({
                "doc_id": row["doc_id"],
                "title": row["title"] or "Untitled",
                "source_path": row["source_path"],
                "source_type": row["source_type"],
                "created_at": row["created_at"],
                "heading_context": row["heading_context"],
                "snippet": snippet
            })

    return json.dumps({
        "cluster": cluster_info,
        "documents": documents
    }, indent=2)


@mcp.tool()
def search_clusters(
    query: str,
    limit: int = 5
) -> str:
    """
    Find clusters by searching labels and keywords.

    Args:
        query: Search term to match against cluster labels and keywords
        limit: Maximum clusters to return (default: 5)

    Returns:
        JSON array of matching clusters
    """
    db = get_db()
    query_lower = query.lower()

    with db._conn() as conn:
        # Check if clusters table exists
        table_check = conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='clusters'"
        ).fetchone()

        if not table_check:
            return json.dumps({
                "error": "Clusters not generated yet. Run scripts/cluster.py first."
            })

        # Search in label and keywords
        rows = conn.execute("""
            SELECT id, label, description, keywords, chunk_count
            FROM clusters
            WHERE id >= 0
            ORDER BY chunk_count DESC
        """).fetchall()

    matches = []
    for row in rows:
        label = row["label"].lower()
        keywords = json.loads(row["keywords"]) if row["keywords"] else []
        keywords_str = " ".join(keywords).lower()

        # Score based on match quality
        score = 0
        if query_lower in label:
            score += 10
        if query_lower in keywords_str:
            score += 5
        for kw in keywords:
            if query_lower in kw.lower():
                score += 2

        if score > 0:
            matches.append({
                "cluster_id": row["id"],
                "label": row["label"],
                "description": row["description"],
                "keywords": keywords,
                "chunk_count": row["chunk_count"],
                "match_score": score
            })

    # Sort by score then by chunk count
    matches.sort(key=lambda x: (-x["match_score"], -x["chunk_count"]))

    return json.dumps(matches[:limit], indent=2)


@mcp.tool()
def get_document_cluster(
    doc_id: Optional[int] = None,
    source_path: Optional[str] = None
) -> str:
    """
    Get the cluster(s) a document belongs to.

    Args:
        doc_id: Document ID
        source_path: Or provide the source path instead

    Returns:
        JSON with document info and its cluster assignments
    """
    if not doc_id and not source_path:
        return json.dumps({"error": "Provide either doc_id or source_path"})

    db = get_db()

    with db._conn() as conn:
        # Get document
        if source_path:
            doc_row = conn.execute(
                "SELECT id, title, source_path FROM documents WHERE source_path = ?",
                (source_path,)
            ).fetchone()
            if doc_row:
                doc_id = doc_row["id"]
        else:
            doc_row = conn.execute(
                "SELECT id, title, source_path FROM documents WHERE id = ?",
                (doc_id,)
            ).fetchone()

        if not doc_row:
            return json.dumps({"error": "Document not found"})

        # Check if clusters exist
        table_check = conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='chunk_clusters'"
        ).fetchone()

        if not table_check:
            return json.dumps({
                "error": "Clusters not generated yet. Run scripts/cluster.py first."
            })

        # Get cluster assignments for this document's chunks
        rows = conn.execute("""
            SELECT cl.id, cl.label, cl.keywords, COUNT(*) as chunk_count
            FROM chunk_clusters cc
            JOIN chunks c ON cc.chroma_id = c.chroma_id
            JOIN clusters cl ON cc.cluster_id = cl.id
            WHERE c.doc_id = ?
            GROUP BY cl.id
            ORDER BY chunk_count DESC
        """, (doc_id,)).fetchall()

        clusters = []
        for row in rows:
            clusters.append({
                "cluster_id": row["id"],
                "label": row["label"],
                "keywords": json.loads(row["keywords"]) if row["keywords"] else [],
                "chunks_in_cluster": row["chunk_count"]
            })

    return json.dumps({
        "doc_id": doc_id,
        "title": doc_row["title"],
        "source_path": doc_row["source_path"],
        "clusters": clusters
    }, indent=2)


# =============================================================================
# EMACS AGENT ACTION TOOLS
# =============================================================================

@mcp.tool()
def log_emacs_action(
    tool_name: str,
    parameters: dict,
    result: dict,
    session_id: Optional[str] = None,
    screenshot_path: Optional[str] = None,
    timestamp: Optional[str] = None
) -> str:
    """
    Log an action from the Emacs MCP server.

    Use this to record actions taken by Emacs-based agents for analysis,
    debugging, and replay capabilities.

    Args:
        tool_name: Name of the MCP tool that was called (e.g., "eval_elisp", "buffer_read")
        parameters: Parameters passed to the tool (JSON object)
        result: Result from the tool (JSON object)
        session_id: Optional session identifier for grouping related actions
        screenshot_path: Optional path to a screenshot taken with this action
        timestamp: Optional ISO timestamp (defaults to current time)

    Returns:
        JSON with action_id and confirmation
    """
    emacs_actions = get_emacs_actions()

    action_id = emacs_actions.log_action(
        tool_name=tool_name,
        parameters=parameters,
        result=result,
        session_id=session_id,
        screenshot_path=screenshot_path,
        timestamp=timestamp
    )

    return json.dumps({
        "status": "logged",
        "action_id": action_id,
        "tool_name": tool_name,
        "session_id": session_id
    }, indent=2)


@mcp.tool()
def get_recent_emacs_actions(
    limit: int = 20,
    session_id: Optional[str] = None
) -> str:
    """
    Get recent Emacs agent action history.

    Use this to review what actions have been taken recently, useful for
    debugging or understanding agent behavior.

    Args:
        limit: Maximum number of actions to return (default: 20)
        session_id: Optional session ID to filter by specific session

    Returns:
        JSON array of recent actions with tool_name, parameters, result, timestamp
    """
    emacs_actions = get_emacs_actions()

    if session_id:
        actions = emacs_actions.get_actions_by_session(session_id, limit)
    else:
        actions = emacs_actions.get_recent_actions(limit)

    return json.dumps([
        {
            "action_id": a.id,
            "session_id": a.session_id,
            "timestamp": a.timestamp,
            "tool_name": a.tool_name,
            "parameters": a.parameters,
            "result": a.result,
            "screenshot_path": a.screenshot_path
        }
        for a in actions
    ], indent=2)


@mcp.tool()
def search_emacs_actions(
    query: str,
    limit: int = 10
) -> str:
    """
    Search past Emacs agent actions by tool name or parameters.

    Use this to find specific actions, like all uses of a particular tool
    or actions involving certain files or buffers.

    Args:
        query: Search string to match against tool names or parameters
        limit: Maximum number of results (default: 10)

    Returns:
        JSON array of matching actions with tool_name, parameters, result preview
    """
    emacs_actions = get_emacs_actions()
    results = emacs_actions.search_actions(query, limit)

    return json.dumps([
        {
            "action_id": r.id,
            "session_id": r.session_id,
            "timestamp": r.timestamp,
            "tool_name": r.tool_name,
            "parameters": r.parameters,
            "result_preview": r.result_preview,
            "screenshot_path": r.screenshot_path
        }
        for r in results
    ], indent=2)


@mcp.tool()
def get_emacs_action_stats() -> str:
    """
    Get statistics about logged Emacs agent actions.

    Returns:
        JSON with total actions, actions by tool, session count, etc.
    """
    emacs_actions = get_emacs_actions()
    stats = emacs_actions.get_stats()

    return json.dumps(stats, indent=2)


# =============================================================================
# HIERARCHICAL RETRIEVAL - Phase 1: Chunk Operations
# =============================================================================

@mcp.tool()
def get_chunks(
    chunk_ids: list[int],
    include_metadata: bool = True
) -> str:
    """
    Retrieve specific chunks by their IDs.

    Use this when you have chunk IDs from search results and need the full content
    or metadata (document info, headings, position).

    Args:
        chunk_ids: List of chunk IDs to retrieve
        include_metadata: Include doc_id, heading_context, offsets, metadata JSON

    Returns:
        JSON array of chunks with content and optional metadata

    Example:
        get_chunks([101, 102, 103], include_metadata=True)
    """
    db = get_db()

    if not chunk_ids:
        return json.dumps({"error": "chunk_ids cannot be empty"})

    with db._conn() as conn:
        placeholders = ','.join('?' * len(chunk_ids))
        if include_metadata:
            query = f"""
                SELECT c.id, c.doc_id, c.chunk_index, c.content, c.heading_context,
                       c.start_offset, c.end_offset, c.metadata,
                       d.title as doc_title, d.source_path, d.source_type
                FROM chunks c
                JOIN documents d ON c.doc_id = d.id
                WHERE c.id IN ({placeholders})
                ORDER BY c.doc_id, c.chunk_index
            """
        else:
            query = f"""
                SELECT c.id, c.content
                FROM chunks c
                WHERE c.id IN ({placeholders})
                ORDER BY c.id
            """

        rows = conn.execute(query, chunk_ids).fetchall()

        chunks = []
        for row in rows:
            chunk = dict(row)
            if include_metadata and chunk.get("metadata"):
                try:
                    chunk["metadata"] = json.loads(chunk["metadata"])
                except:
                    pass
            chunks.append(chunk)

        return json.dumps({"result": chunks}, indent=2)


@mcp.tool()
def get_surrounding_context(
    chunk_id: int,
    before: int = 2,
    after: int = 2,
    include_doc_metadata: bool = True
) -> str:
    """
    Get chunks surrounding a specific chunk for context expansion.

    **THIS IS THE KEY TOOL FOR LARGE DOCUMENT NAVIGATION**
    When search returns a relevant chunk, use this to see the surrounding content.

    Args:
        chunk_id: The center chunk ID
        before: Number of chunks before (default: 2)
        after: Number of chunks after (default: 2)
        include_doc_metadata: Include document title and path

    Returns:
        JSON with center chunk, preceding chunks, following chunks, and document info

    Example:
        # Found chunk 12345 in search, need context
        get_surrounding_context(12345, before=3, after=3)
    """
    db = get_db()

    with db._conn() as conn:
        # Get center chunk info
        center = conn.execute("""
            SELECT c.*, d.title as doc_title, d.source_path, d.source_type
            FROM chunks c
            JOIN documents d ON c.doc_id = d.id
            WHERE c.id = ?
        """, (chunk_id,)).fetchone()

        if not center:
            return json.dumps({"error": f"Chunk {chunk_id} not found"})

        center = dict(center)
        doc_id = center["doc_id"]
        chunk_index = center["chunk_index"]

        # Get preceding chunks
        preceding = []
        if before > 0:
            rows = conn.execute("""
                SELECT id, chunk_index, content, heading_context, metadata
                FROM chunks
                WHERE doc_id = ? AND chunk_index < ?
                ORDER BY chunk_index DESC
                LIMIT ?
            """, (doc_id, chunk_index, before)).fetchall()
            preceding = [dict(row) for row in reversed(rows)]

        # Get following chunks
        following = []
        if after > 0:
            rows = conn.execute("""
                SELECT id, chunk_index, content, heading_context, metadata
                FROM chunks
                WHERE doc_id = ? AND chunk_index > ?
                ORDER BY chunk_index ASC
                LIMIT ?
            """, (doc_id, chunk_index, after)).fetchall()
            following = [dict(row) for row in rows]

        # Parse metadata if present
        for chunk in [center] + preceding + following:
            if chunk.get("metadata"):
                try:
                    chunk["metadata"] = json.loads(chunk["metadata"])
                except:
                    pass

        result = {
            "center_chunk": center,
            "preceding_chunks": preceding,
            "following_chunks": following,
            "total_chunks": 1 + len(preceding) + len(following)
        }

        if include_doc_metadata:
            result["document"] = {
                "doc_id": center["doc_id"],
                "title": center["doc_title"],
                "source_path": center["source_path"],
                "source_type": center["source_type"]
            }

        return json.dumps({"result": result}, indent=2)


@mcp.tool()
def get_chunk_metadata(
    chunk_id: int,
    include: Optional[list[str]] = None
) -> str:
    """
    Get detailed metadata for a specific chunk.

    Args:
        chunk_id: Chunk ID to query
        include: Optional list of metadata fields to include:
                 - "document_title"
                 - "heading_path" (from metadata JSON)
                 - "session_id" (from metadata JSON)
                 - "position_in_doc"
                 - "surrounding_headings"
                 All fields included if not specified

    Returns:
        JSON with chunk metadata including document context and position

    Example:
        get_chunk_metadata(12345, include=["document_title", "heading_path", "position_in_doc"])
    """
    db = get_db()

    with db._conn() as conn:
        row = conn.execute("""
            SELECT c.*, d.title as doc_title, d.source_path, d.source_type,
                   (SELECT COUNT(*) FROM chunks WHERE doc_id = c.doc_id) as total_chunks_in_doc
            FROM chunks c
            JOIN documents d ON c.doc_id = d.id
            WHERE c.id = ?
        """, (chunk_id,)).fetchone()

        if not row:
            return json.dumps({"error": f"Chunk {chunk_id} not found"})

        chunk = dict(row)

        # Parse metadata JSON
        metadata_json = {}
        if chunk.get("metadata"):
            try:
                metadata_json = json.loads(chunk["metadata"])
            except:
                pass

        # Build result
        result = {
            "chunk_id": chunk["id"],
            "doc_id": chunk["doc_id"],
            "chunk_index": chunk["chunk_index"],
            "document_title": chunk["doc_title"],
            "source_path": chunk["source_path"],
            "source_type": chunk["source_type"],
            "heading_context": chunk["heading_context"],
            "position_in_doc": {
                "chunk_index": chunk["chunk_index"],
                "total_chunks": chunk["total_chunks_in_doc"],
                "percentage": round((chunk["chunk_index"] / chunk["total_chunks_in_doc"]) * 100, 1) if chunk["total_chunks_in_doc"] > 0 else 0
            },
            "offsets": {
                "start": chunk["start_offset"],
                "end": chunk["end_offset"]
            }
        }

        # Add metadata JSON fields
        if metadata_json:
            if "heading_path" in metadata_json:
                result["heading_path"] = metadata_json["heading_path"]
            if "session_id" in metadata_json:
                result["session_id"] = metadata_json["session_id"]
            if "message_index" in metadata_json:
                result["message_index"] = metadata_json["message_index"]

        # Filter by include list if provided
        if include:
            filtered = {"chunk_id": result["chunk_id"]}
            for field in include:
                if field in result:
                    filtered[field] = result[field]
            result = filtered

        return json.dumps({"result": result}, indent=2)


# =============================================================================
# PHASE 2: DOCUMENT SCOPING
# =============================================================================


@mcp.tool()
def get_document_outline(
    doc_id: int
) -> str:
    """
    Get the hierarchical outline/structure of a document.

    Returns the document's heading hierarchy extracted from chunk metadata,
    showing how the document is organized into sections and subsections.

    Args:
        doc_id: Document ID to get outline for

    Returns:
        JSON with document metadata and hierarchical outline of headings

    Example:
        get_document_outline(17664)
        # Returns: {"title": "...", "outline": [{"heading": "...", "chunk_count": N}, ...]}
    """
    db = get_db()

    # Get document info
    doc = db.get_document(doc_id)
    if not doc:
        return json.dumps({"error": f"Document {doc_id} not found"})

    # Get all chunks with heading information
    with db._conn() as conn:
        rows = conn.execute("""
            SELECT heading_context, metadata, COUNT(*) as chunk_count
            FROM chunks
            WHERE doc_id = ?
            GROUP BY heading_context, metadata
            ORDER BY MIN(chunk_index)
        """, (doc_id,)).fetchall()

    # Build outline from headings
    outline = []
    for row in rows:
        heading_context = row["heading_context"] or ""
        metadata_json_str = row["metadata"]
        chunk_count = row["chunk_count"]

        # Try to extract heading from metadata JSON first
        heading_path = None
        if metadata_json_str:
            try:
                import json as json_lib
                metadata_json = json_lib.loads(metadata_json_str)
                heading_path = metadata_json.get("heading_path", [])
            except:
                pass

        # Use heading_path if available, otherwise heading_context
        heading_display = " > ".join(heading_path) if heading_path else heading_context

        if heading_display:
            outline.append({
                "heading": heading_display,
                "heading_path": heading_path if heading_path else heading_context.split(" > "),
                "chunk_count": chunk_count
            })

    return json.dumps({
        "doc_id": doc.id,
        "title": doc.title,
        "source_path": doc.source_path,
        "total_chunks": sum(item["chunk_count"] for item in outline),
        "outline": outline
    }, indent=2)


@mcp.tool()
def get_document_sections(
    doc_id: int,
    include_chunk_ids: bool = False
) -> str:
    """
    Get all sections/headings in a document with their chunks.

    Returns a flat list of all sections in the document, useful for
    navigating to specific parts of the document.

    Args:
        doc_id: Document ID to get sections for
        include_chunk_ids: Include list of chunk IDs in each section (default: False)

    Returns:
        JSON with document sections, each with heading and optionally chunk IDs

    Example:
        get_document_sections(17664, include_chunk_ids=True)
        # Returns: {"sections": [{"heading": "...", "chunk_ids": [1,2,3]}, ...]}
    """
    db = get_db()

    # Get document info
    doc = db.get_document(doc_id)
    if not doc:
        return json.dumps({"error": f"Document {doc_id} not found"})

    # Get all chunks grouped by heading
    with db._conn() as conn:
        rows = conn.execute("""
            SELECT id, chunk_index, heading_context, metadata
            FROM chunks
            WHERE doc_id = ?
            ORDER BY chunk_index
        """, (doc_id,)).fetchall()

    # Group chunks by heading
    from collections import defaultdict
    import json as json_lib

    sections = defaultdict(list)
    heading_order = []

    for row in rows:
        chunk_id = row["id"]
        heading_context = row["heading_context"] or "No heading"
        metadata_json_str = row["metadata"]

        # Try to get heading from metadata JSON
        heading_display = heading_context
        if metadata_json_str:
            try:
                metadata_json = json_lib.loads(metadata_json_str)
                heading_path = metadata_json.get("heading_path", [])
                if heading_path:
                    heading_display = " > ".join(heading_path)
            except:
                pass

        if heading_display not in sections:
            heading_order.append(heading_display)

        sections[heading_display].append(chunk_id)

    # Build output
    sections_list = []
    for heading in heading_order:
        section = {
            "heading": heading,
            "chunk_count": len(sections[heading])
        }
        if include_chunk_ids:
            section["chunk_ids"] = sections[heading]
        sections_list.append(section)

    return json.dumps({
        "doc_id": doc.id,
        "title": doc.title,
        "source_path": doc.source_path,
        "total_sections": len(sections_list),
        "total_chunks": sum(s["chunk_count"] for s in sections_list),
        "sections": sections_list
    }, indent=2)


# =============================================================================
# PHASE 3: SESSION NAVIGATION
# =============================================================================


@mcp.tool()
def search_session(
    session_id: int,
    query: str,
    search_type: str = "hybrid",
    limit: int = 20
) -> str:
    """
    Search within a specific conversation session.

    Scopes search to chunks that belong to a particular Claude/ChatGPT session,
    enabling focused exploration of a single conversation.

    Args:
        session_id: Session ID to search within
        query: Search query
        search_type: "semantic", "fulltext", or "hybrid" (default: hybrid)
        limit: Maximum results (default: 20)

    Returns:
        JSON array of search results scoped to the session

    Example:
        search_session(session_id=15, query="CDC implementation", search_type="fulltext")

    Note:
        Requires chunks to have session_id in their metadata. Sessions without
        chunk metadata will not return results. Use get_stats() to check
        session indexing status.
    """
    db = get_db()

    # First, get doc_ids that belong to this session
    # For now, we'll use source_path matching since session_id isn't populated yet
    # TODO: Once session_id is in chunk metadata, filter by that instead

    session = db.get_session(session_id)
    if not session:
        return json.dumps({"error": f"Session {session_id} not found"})

    # Get documents from this session based on session creation date
    # This is a temporary approach until session_id is in chunk metadata
    with db._conn() as conn:
        session_docs = conn.execute("""
            SELECT id FROM documents
            WHERE created_at >= ?
            AND created_at <= datetime(?, '+1 day')
        """, (session.created_at, session.created_at)).fetchall()

    doc_ids = [row["id"] for row in session_docs]

    if not doc_ids:
        return json.dumps({
            "session_id": session_id,
            "results": [],
            "note": "No documents found for this session. Session-chunk linking not yet implemented."
        })

    # Use existing search functions with doc_ids filter
    if search_type == "semantic":
        return search_semantic(query, doc_ids=doc_ids, limit=limit)
    elif search_type == "fulltext":
        return search_fulltext(query, doc_ids=doc_ids, limit=limit)
    else:  # hybrid
        return search_hybrid(query, doc_ids=doc_ids, limit=limit)


@mcp.tool()
def get_session_messages(
    session_id: int,
    limit: int = 100,
    include_chunks: bool = False
) -> str:
    """
    Get messages/chunks from a conversation session.

    Retrieves the chronological flow of a conversation, optionally including
    the actual chunk content.

    Args:
        session_id: Session ID to retrieve messages from
        limit: Maximum messages to return (default: 100)
        include_chunks: Include full chunk content (default: False, only metadata)

    Returns:
        JSON with session metadata and list of messages/chunks

    Example:
        get_session_messages(session_id=15, limit=50, include_chunks=True)
    """
    db = get_db()

    session = db.get_session(session_id)
    if not session:
        return json.dumps({"error": f"Session {session_id} not found"})

    # Get chunks that belong to this session
    # TODO: Filter by session_id in metadata once populated
    # For now, get chunks from documents created during the session

    with db._conn() as conn:
        # Get documents from session timeframe
        session_docs = conn.execute("""
            SELECT id FROM documents
            WHERE created_at >= ?
            AND created_at <= datetime(?, '+1 day')
        """, (session.created_at, session.created_at)).fetchall()

        doc_ids = [row["id"] for row in session_docs]

        if not doc_ids:
            return json.dumps({
                "session_id": session_id,
                "session_summary": session.summary,
                "created_at": session.created_at,
                "messages": [],
                "note": "No messages found. Session-chunk linking not yet implemented."
            })

        # Get chunks from these documents
        placeholders = ",".join("?" * len(doc_ids))
        query = f"""
            SELECT c.id, c.chunk_index, c.content, c.metadata, d.title, d.source_path
            FROM chunks c
            JOIN documents d ON c.doc_id = d.id
            WHERE c.doc_id IN ({placeholders})
            ORDER BY c.chunk_index
            LIMIT ?
        """

        chunks = conn.execute(query, doc_ids + [limit]).fetchall()

    # Build message list
    messages = []
    for chunk in chunks:
        message = {
            "chunk_id": chunk["id"],
            "chunk_index": chunk["chunk_index"],
            "document_title": chunk["title"],
            "source_path": chunk["source_path"]
        }

        if include_chunks:
            message["content"] = chunk["content"][:500] + "..." if len(chunk["content"]) > 500 else chunk["content"]

        # Extract metadata if available
        if chunk["metadata"]:
            try:
                import json as json_lib
                metadata = json_lib.loads(chunk["metadata"])
                if "heading_path" in metadata:
                    message["heading"] = " > ".join(metadata["heading_path"])
                if "message_index" in metadata:
                    message["message_index"] = metadata["message_index"]
            except:
                pass

        messages.append(message)

    return json.dumps({
        "session_id": session_id,
        "session_summary": session.summary,
        "created_at": session.created_at,
        "message_count": len(messages),
        "messages": messages
    }, indent=2)


@mcp.tool()
def get_session_timeline(
    session_id: int
) -> str:
    """
    Get a timeline view of a conversation session.

    Returns key events and turning points in the conversation, useful for
    understanding the flow and progression of topics.

    Args:
        session_id: Session ID to get timeline for

    Returns:
        JSON with session timeline including topics, key facts, and message flow

    Example:
        get_session_timeline(session_id=15)
    """
    db = get_db()

    session = db.get_session(session_id)
    if not session:
        return json.dumps({"error": f"Session {session_id} not found"})

    # Get session metadata
    import json as json_lib

    topics = []
    if session.topics:
        try:
            topics = json_lib.loads(session.topics)
        except:
            topics = session.topics_text.split(",") if session.topics_text else []

    key_facts = []
    if session.key_facts:
        try:
            key_facts = json_lib.loads(session.key_facts)
        except:
            key_facts = session.key_facts_text.split(",") if session.key_facts_text else []

    # Get message count from chunks
    with db._conn() as conn:
        session_docs = conn.execute("""
            SELECT id FROM documents
            WHERE created_at >= ?
            AND created_at <= datetime(?, '+1 day')
        """, (session.created_at, session.created_at)).fetchall()

        doc_ids = [row["id"] for row in session_docs]

        chunk_count = 0
        if doc_ids:
            placeholders = ",".join("?" * len(doc_ids))
            chunk_count = conn.execute(f"""
                SELECT COUNT(*) as count FROM chunks WHERE doc_id IN ({placeholders})
            """, doc_ids).fetchone()["count"]

    return json.dumps({
        "session_id": session_id,
        "created_at": session.created_at,
        "updated_at": session.updated_at,
        "summary": session.summary,
        "topics": topics,
        "key_facts": key_facts,
        "message_count": session.message_count or chunk_count,
        "word_count": session.word_count,
        "timeline_note": "Detailed message timeline requires session_id in chunk metadata (not yet implemented)"
    }, indent=2)


@mcp.tool()
def get_conversation_thread(
    chunk_id: int,
    direction: str = "both",
    limit: int = 50
) -> str:
    """
    Get the conversation thread around a specific chunk/message.

    Retrieves surrounding messages in a conversation, useful for understanding
    the context and flow before and after a particular point.

    Args:
        chunk_id: Starting chunk ID (the "anchor" message)
        direction: "before", "after", or "both" (default: both)
        limit: Maximum chunks to return in each direction (default: 50)

    Returns:
        JSON with conversation thread including before/after messages

    Example:
        # Get 20 messages before and after chunk 12345
        get_conversation_thread(chunk_id=12345, direction="both", limit=20)

        # Get only messages that came before
        get_conversation_thread(chunk_id=12345, direction="before", limit=30)
    """
    db = get_db()

    # Get the anchor chunk
    with db._conn() as conn:
        anchor = conn.execute("""
            SELECT id, doc_id, chunk_index, content, metadata
            FROM chunks
            WHERE id = ?
        """, (chunk_id,)).fetchone()

        if not anchor:
            return json.dumps({"error": f"Chunk {chunk_id} not found"})

        doc_id = anchor["doc_id"]
        anchor_index = anchor["chunk_index"]

        # Get surrounding chunks from same document
        chunks_before = []
        chunks_after = []

        if direction in ["before", "both"]:
            chunks_before = conn.execute("""
                SELECT id, chunk_index, content, metadata, heading_context
                FROM chunks
                WHERE doc_id = ? AND chunk_index < ?
                ORDER BY chunk_index DESC
                LIMIT ?
            """, (doc_id, anchor_index, limit)).fetchall()
            chunks_before.reverse()  # Put in chronological order

        if direction in ["after", "both"]:
            chunks_after = conn.execute("""
                SELECT id, chunk_index, content, metadata, heading_context
                FROM chunks
                WHERE doc_id = ? AND chunk_index > ?
                ORDER BY chunk_index ASC
                LIMIT ?
            """, (doc_id, anchor_index, limit)).fetchall()

    # Build thread
    import json as json_lib

    def format_chunk(chunk_row):
        formatted = {
            "chunk_id": chunk_row["id"],
            "chunk_index": chunk_row["chunk_index"],
            "content_preview": chunk_row["content"][:200] + "..." if len(chunk_row["content"]) > 200 else chunk_row["content"],
            "heading": chunk_row["heading_context"] or ""
        }

        # Extract metadata if available
        if chunk_row["metadata"]:
            try:
                metadata = json_lib.loads(chunk_row["metadata"])
                if "heading_path" in metadata:
                    formatted["heading"] = " > ".join(metadata["heading_path"])
                if "message_index" in metadata:
                    formatted["message_index"] = metadata["message_index"]
            except:
                pass

        return formatted

    thread = {
        "anchor_chunk_id": chunk_id,
        "anchor_chunk_index": anchor_index,
        "doc_id": doc_id,
        "before": [format_chunk(c) for c in chunks_before] if direction in ["before", "both"] else [],
        "anchor": {
            "chunk_id": anchor["id"],
            "chunk_index": anchor["chunk_index"],
            "content_preview": anchor["content"][:200] + "..." if len(anchor["content"]) > 200 else anchor["content"]
        },
        "after": [format_chunk(c) for c in chunks_after] if direction in ["after", "both"] else [],
        "total_before": len(chunks_before),
        "total_after": len(chunks_after)
    }

    return json.dumps(thread, indent=2)


# =============================================================================
# PHASE 4: HEADING-LEVEL SCOPING
# =============================================================================


@mcp.tool()
def search_within_heading(
    doc_id: int,
    heading_path: list[str],
    query: str,
    search_type: str = "hybrid",
    limit: int = 20
) -> str:
    """
    Search within a specific heading/section of a document.

    Enables focused search within a particular section, useful when you know
    which part of a large document is relevant.

    Args:
        doc_id: Document ID to search in
        heading_path: Hierarchical heading path as list (e.g., ["Phase 2", "Implementation"])
        query: Search query
        search_type: "semantic", "fulltext", or "hybrid" (default: hybrid)
        limit: Maximum results (default: 20)

    Returns:
        JSON array of search results scoped to the heading/section

    Example:
        search_within_heading(
            doc_id=17664,
            heading_path=["User [2026-01-27 Tue 12:52]"],
            query="notification stream",
            search_type="fulltext"
        )
    """
    db = get_db()

    # Get document
    doc = db.get_document(doc_id)
    if not doc:
        return json.dumps({"error": f"Document {doc_id} not found"})

    # Find chunks that match the heading path
    heading_str = " > ".join(heading_path)

    with db._conn() as conn:
        # Find chunks with matching heading_context or metadata.heading_path
        chunk_ids = []

        # Try heading_context first (legacy)
        rows = conn.execute("""
            SELECT id FROM chunks
            WHERE doc_id = ? AND heading_context = ?
        """, (doc_id, heading_str)).fetchall()

        chunk_ids.extend([row["id"] for row in rows])

        # Also try metadata.heading_path (Phase 1+)
        all_chunks = conn.execute("""
            SELECT id, metadata FROM chunks WHERE doc_id = ?
        """, (doc_id,)).fetchall()

        import json as json_lib
        for row in all_chunks:
            if row["metadata"]:
                try:
                    metadata = json_lib.loads(row["metadata"])
                    if metadata.get("heading_path") == heading_path:
                        if row["id"] not in chunk_ids:
                            chunk_ids.append(row["id"])
                except:
                    pass

    if not chunk_ids:
        return json.dumps({
            "doc_id": doc_id,
            "heading_path": heading_path,
            "results": [],
            "note": "No chunks found with this heading path"
        })

    # Now search within these chunks
    # For simplicity, get the chunks and do client-side filtering
    # A more efficient approach would be to add chunk_ids to the search query
    results = []

    with db._conn() as conn:
        placeholders = ",".join("?" * len(chunk_ids))
        chunks = conn.execute(f"""
            SELECT id, content, chunk_index
            FROM chunks
            WHERE id IN ({placeholders})
            ORDER BY chunk_index
        """, chunk_ids).fetchall()

    # Simple text matching (could be enhanced with FTS or embeddings)
    for chunk in chunks:
        if query.lower() in chunk["content"].lower():
            results.append({
                "chunk_id": chunk["id"],
                "chunk_index": chunk["chunk_index"],
                "snippet": chunk["content"][:200] + "..." if len(chunk["content"]) > 200 else chunk["content"]
            })

            if len(results) >= limit:
                break

    return json.dumps({
        "doc_id": doc_id,
        "doc_title": doc.title,
        "heading_path": heading_path,
        "results": results,
        "total_chunks_in_heading": len(chunk_ids)
    }, indent=2)


@mcp.tool()
def get_chunks_in_heading(
    doc_id: int,
    heading_path: list[str],
    include_content: bool = False
) -> str:
    """
    Get all chunks within a specific heading/section.

    Retrieves all content under a particular heading, useful for reading
    an entire section of a document.

    Args:
        doc_id: Document ID
        heading_path: Hierarchical heading path as list
        include_content: Include full chunk content (default: False, only metadata)

    Returns:
        JSON with chunks under the specified heading

    Example:
        get_chunks_in_heading(
            doc_id=17664,
            heading_path=["Thread", "User [2026-01-27 Tue 12:52]"],
            include_content=True
        )
    """
    db = get_db()

    doc = db.get_document(doc_id)
    if not doc:
        return json.dumps({"error": f"Document {doc_id} not found"})

    heading_str = " > ".join(heading_path)

    with db._conn() as conn:
        # Find matching chunks
        chunks = []

        # Try heading_context
        rows = conn.execute("""
            SELECT id, chunk_index, content, metadata
            FROM chunks
            WHERE doc_id = ? AND heading_context = ?
            ORDER BY chunk_index
        """, (doc_id, heading_str)).fetchall()

        for row in rows:
            chunk = {
                "chunk_id": row["id"],
                "chunk_index": row["chunk_index"]
            }
            if include_content:
                chunk["content"] = row["content"]
            chunks.append(chunk)

        # Also check metadata.heading_path
        all_chunks = conn.execute("""
            SELECT id, chunk_index, content, metadata
            FROM chunks
            WHERE doc_id = ?
            ORDER BY chunk_index
        """, (doc_id,)).fetchall()

        import json as json_lib
        seen_ids = {c["chunk_id"] for c in chunks}

        for row in all_chunks:
            if row["id"] in seen_ids:
                continue

            if row["metadata"]:
                try:
                    metadata = json_lib.loads(row["metadata"])
                    if metadata.get("heading_path") == heading_path:
                        chunk = {
                            "chunk_id": row["id"],
                            "chunk_index": row["chunk_index"]
                        }
                        if include_content:
                            chunk["content"] = row["content"]
                        chunks.append(chunk)
                except:
                    pass

    return json.dumps({
        "doc_id": doc_id,
        "doc_title": doc.title,
        "heading_path": heading_path,
        "chunk_count": len(chunks),
        "chunks": chunks
    }, indent=2)


@mcp.tool()
def navigate_document_hierarchy(
    doc_id: int,
    path: Optional[list[str]] = None
) -> str:
    """
    Navigate the heading hierarchy of a document.

    Returns the heading structure, allowing you to explore the document's
    organization and drill down into specific sections.

    Args:
        doc_id: Document ID to navigate
        path: Optional heading path to start from (default: root, shows all top-level headings)

    Returns:
        JSON with heading hierarchy and navigation options

    Example:
        # Get top-level headings
        navigate_document_hierarchy(doc_id=17664)

        # Get subheadings under a specific path
        navigate_document_hierarchy(doc_id=17664, path=["Thread"])
    """
    db = get_db()

    doc = db.get_document(doc_id)
    if not doc:
        return json.dumps({"error": f"Document {doc_id} not found"})

    with db._conn() as conn:
        # Get all unique headings with their chunk counts
        all_chunks = conn.execute("""
            SELECT heading_context, metadata, COUNT(*) as chunk_count
            FROM chunks
            WHERE doc_id = ?
            GROUP BY heading_context, metadata
        """, (doc_id,)).fetchall()

    # Build heading hierarchy
    import json as json_lib
    from collections import defaultdict

    heading_tree = defaultdict(int)

    for row in all_chunks:
        heading_context = row["heading_context"] or ""
        metadata_str = row["metadata"]
        chunk_count = row["chunk_count"]

        # Get heading_path from metadata or heading_context
        heading_path_list = []
        if metadata_str:
            try:
                metadata = json_lib.loads(metadata_str)
                heading_path_list = metadata.get("heading_path", [])
            except:
                pass

        if not heading_path_list and heading_context:
            heading_path_list = [h.strip() for h in heading_context.split(" > ")]

        # Add to tree
        if heading_path_list:
            heading_key = tuple(heading_path_list)
            heading_tree[heading_key] += chunk_count

    # Filter by path if provided
    if path:
        path_tuple = tuple(path)
        filtered = {
            k: v for k, v in heading_tree.items()
            if len(k) > len(path_tuple) and k[:len(path_tuple)] == path_tuple
        }
    else:
        filtered = heading_tree

    # Build navigation structure
    navigation = []
    for heading_tuple, chunk_count in sorted(filtered.items()):
        navigation.append({
            "heading_path": list(heading_tuple),
            "heading_display": " > ".join(heading_tuple),
            "depth": len(heading_tuple),
            "chunk_count": chunk_count,
            "has_children": any(k[:len(heading_tuple)] == heading_tuple and len(k) > len(heading_tuple) for k in heading_tree.keys())
        })

    return json.dumps({
        "doc_id": doc_id,
        "doc_title": doc.title,
        "current_path": path or [],
        "headings": navigation[:50],  # Limit to 50 for readability
        "total_headings": len(navigation)
    }, indent=2)


# =============================================================================
# RESOURCES
# =============================================================================

@mcp.resource("vault://stats")
def resource_stats() -> str:
    """Get vault statistics."""
    return get_stats()


@mcp.resource("vault://schema")
def resource_schema() -> str:
    """Get database schema for query reference."""
    from vault_rag.db.sqlite import SCHEMA
    return SCHEMA


@mcp.resource("vault://recent-sessions")
def resource_recent_sessions() -> str:
    """
    Get recent session summaries for context injection.

    This resource provides formatted recent sessions suitable for
    injecting into a new Claude Code session's context.
    """
    db = get_db()
    sessions = db.get_recent_sessions(5)

    if not sessions:
        return "No previous sessions recorded."

    lines = ["# Recent Sessions\n"]
    for s in sessions:
        lines.append(f"## Session from {s.created_at}")
        lines.append(f"**Topics:** {', '.join(s.topics) if s.topics else 'None recorded'}")
        lines.append(f"\n{s.summary}\n")
        if s.key_facts:
            lines.append("**Key facts:**")
            for fact in s.key_facts:
                lines.append(f"- {fact}")
        lines.append("")

    return "\n".join(lines)


# =============================================================================
# MAIN
# =============================================================================

def main():
    """Run the MCP server."""
    mcp.run()


if __name__ == "__main__":
    main()
