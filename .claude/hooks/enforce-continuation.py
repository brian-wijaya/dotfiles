#!/usr/bin/env python3
"""
Deterministic Exit Enforcement Hook
Validates Claude responses and enforces dual-condition exit gate.

CRITICAL: This hook runs after EVERY Claude response and blocks premature stopping.

Exit requires BOTH conditions:
1. completion_indicators >= 2 (accumulated when EXIT_SIGNAL=true)
2. EXIT_SIGNAL: true in current response

Exceeds Ralph's validation with:
- Native Python JSON parsing (faster, more reliable than jq)
- Multi-format EXIT_SIGNAL extraction (5 tiers vs Ralph's 3)
- Enhanced pattern matching with regex + semantic scoring
- Comprehensive state tracking with persistence
- Integration with existing checkpoint system
- Anomaly detection for stuck loops
"""

import json
import os
import re
import sys
import time
from dataclasses import dataclass, asdict
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, List, Dict, Any

# Configuration
ENFORCE_DIR = Path.home() / ".claude/enforcement"
ENFORCE_DIR.mkdir(parents=True, exist_ok=True)

STATE_FILE = ENFORCE_DIR / "exit_state.json"
ANALYSIS_FILE = ENFORCE_DIR / "response_analysis.json"
HISTORY_FILE = ENFORCE_DIR / "response_history.jsonl"
SESSION_FILE = ENFORCE_DIR / "session.json"

# Exit gate thresholds
COMPLETION_THRESHOLD = 2  # Minimum completion indicators needed
SAFETY_CIRCUIT_THRESHOLD = 5  # Force exit after this many explicit signals
MAX_STUCK_LOOPS = 3  # Exit if stuck on same error for this many loops
MAX_TEST_ONLY_LOOPS = 3  # Exit if only running tests for this many loops

# Completion keywords (expanded from Ralph)
COMPLETION_PATTERNS = [
    r'\b(done|complete|completed|finished|ready)\b',
    r'\ball tasks (complete|done|finished)\b',
    r'\bproject (complete|ready|done)\b',
    r'\bnothing (left|more|else) to do\b',
    r'\bfully implemented\b',
    r'\bsuccessfully (deployed|completed|finished)\b',
    r'\bno (more|further|additional) work\b',
]

# Test-only patterns
TEST_PATTERNS = [
    r'\bnpm test\b',
    r'\bpytest\b',
    r'\bcarpo test\b',
    r'\bgo test\b',
    r'\bjest\b',
    r'\brun(ning)? tests\b',
    r'\btest suite\b',
]

# Implementation patterns (anti-test-only)
IMPLEMENTATION_PATTERNS = [
    r'\bimplement(ing|ed)?\b',
    r'\bcreate(d|ing)?\b',
    r'\bwrit(e|ing|ten)\b',
    r'\badd(ed|ing)?\b',
    r'\b(class|function|method|def)\b',
    r'\brefactor(ed|ing)?\b',
]


@dataclass
class ResponseAnalysis:
    """Analysis result for a single Claude response."""
    loop_number: int
    timestamp: str
    output_format: str  # "json" or "text"
    exit_signal: bool
    explicit_exit_signal: bool  # True if EXIT_SIGNAL found (not inferred)
    has_completion_language: bool
    is_test_only: bool
    is_stuck: bool
    has_progress: bool
    files_modified: int
    error_count: int
    confidence_score: int
    work_summary: str
    session_id: Optional[str] = None
    phase_name: Optional[str] = None
    stage_name: Optional[str] = None


@dataclass
class ExitState:
    """Persistent state for exit gate logic."""
    completion_indicators: List[int]  # Loop numbers where EXIT_SIGNAL=true
    test_only_loops: List[int]
    done_signals: List[int]
    last_error: Optional[str] = None
    error_loop_count: int = 0
    session_id: Optional[str] = None
    session_started: Optional[str] = None


class ResponseValidator:
    """Validates Claude responses and enforces continuation."""

    def __init__(self):
        self.state = self._load_state()
        self.loop_number = self._get_loop_number()

    def _load_state(self) -> ExitState:
        """Load persistent exit state."""
        if not STATE_FILE.exists():
            return ExitState(
                completion_indicators=[],
                test_only_loops=[],
                done_signals=[]
            )

        try:
            data = json.loads(STATE_FILE.read_text())
            return ExitState(**data)
        except (json.JSONDecodeError, TypeError):
            # Corrupted state - reinitialize
            return ExitState(
                completion_indicators=[],
                test_only_loops=[],
                done_signals=[]
            )

    def _save_state(self):
        """Persist exit state."""
        # Keep only last 10 entries in rolling windows
        self.state.completion_indicators = self.state.completion_indicators[-10:]
        self.state.test_only_loops = self.state.test_only_loops[-10:]
        self.state.done_signals = self.state.done_signals[-10:]

        STATE_FILE.write_text(json.dumps(asdict(self.state), indent=2))

    def _get_loop_number(self) -> int:
        """Get current loop number from checkpoint system."""
        checkpoint_db = Path.home() / ".claude/context-checkpoints.db"
        if not checkpoint_db.exists():
            return 1

        try:
            import sqlite3
            conn = sqlite3.connect(str(checkpoint_db))
            session_id = os.environ.get('CLAUDE_SESSION_ID', 'unknown')
            result = conn.execute(
                "SELECT COUNT(*) FROM checkpoints WHERE session_id = ?",
                (session_id,)
            ).fetchone()
            conn.close()
            return result[0] if result else 1
        except Exception:
            return 1

    def detect_output_format(self, output: str) -> str:
        """Detect if output is JSON or text."""
        output = output.strip()
        if not output:
            return "text"

        # Check first non-whitespace character
        if output[0] not in ('{', '['):
            return "text"

        # Validate JSON
        try:
            json.loads(output)
            return "json"
        except json.JSONDecodeError:
            return "text"

    def extract_exit_signal(self, output: str, output_format: str) -> tuple[bool, bool]:
        """
        Extract EXIT_SIGNAL from Claude's response.

        Returns: (exit_signal, explicit_found)

        5-tier extraction (exceeds Ralph's 3-tier):
        1. Direct JSON field: exit_signal
        2. Claude CLI result field: .result.exit_signal
        3. RALPH_STATUS block in .result text
        4. CONTINUATION_STATUS block (new format)
        5. STATUS field fallback (COMPLETE inference)
        """
        explicit_found = False
        exit_signal = False

        if output_format == "json":
            try:
                data = json.loads(output)

                # Tier 1: Direct exit_signal field
                if "exit_signal" in data:
                    explicit_found = True
                    exit_signal = bool(data["exit_signal"])
                    return exit_signal, explicit_found

                # Handle array format (Claude CLI)
                if isinstance(data, list):
                    # Find result type message
                    result_msgs = [msg for msg in data if msg.get("type") == "result"]
                    if result_msgs:
                        data = result_msgs[-1]  # Use last result

                # Tier 2: Nested in result
                if "result" in data:
                    result_obj = data["result"]
                    if isinstance(result_obj, dict) and "exit_signal" in result_obj:
                        explicit_found = True
                        exit_signal = bool(result_obj["exit_signal"])
                        return exit_signal, explicit_found

                    # Tier 3: RALPH_STATUS block in .result text
                    if isinstance(result_obj, str) and "---RALPH_STATUS---" in result_obj:
                        exit_signal, explicit_found = self._parse_status_block(
                            result_obj, "RALPH_STATUS"
                        )
                        if explicit_found:
                            return exit_signal, explicit_found

                    # Tier 4: CONTINUATION_STATUS block (our enhanced format)
                    if isinstance(result_obj, str) and "---CONTINUATION_STATUS---" in result_obj:
                        exit_signal, explicit_found = self._parse_status_block(
                            result_obj, "CONTINUATION_STATUS"
                        )
                        if explicit_found:
                            return exit_signal, explicit_found

                # Tier 5: Infer from completion_status metadata
                metadata = data.get("metadata", {})
                if metadata.get("completion_status") in ("complete", "COMPLETE"):
                    exit_signal = True
                    # This is inferred, not explicit
                    return exit_signal, explicit_found

            except json.JSONDecodeError:
                pass

        # Text parsing for non-JSON output
        # Look for RALPH_STATUS or CONTINUATION_STATUS blocks
        for block_type in ["CONTINUATION_STATUS", "RALPH_STATUS"]:
            if f"---{block_type}---" in output:
                exit_signal, explicit_found = self._parse_status_block(output, block_type)
                if explicit_found:
                    return exit_signal, explicit_found

        return exit_signal, explicit_found

    def _parse_status_block(self, text: str, block_type: str) -> tuple[bool, bool]:
        """Parse status block from text."""
        pattern = f"---{block_type}---(.*?)(?:---|$)"
        match = re.search(pattern, text, re.DOTALL)

        if not match:
            return False, False

        block_content = match.group(1)

        # Look for EXIT_SIGNAL or CONTINUE fields
        for line in block_content.split("\n"):
            line = line.strip()

            # EXIT_SIGNAL: true/false
            if line.startswith("EXIT_SIGNAL:"):
                value = line.split(":", 1)[1].strip().lower()
                return value == "true", True

            # CONTINUE: true/false (inverse of exit)
            if line.startswith("CONTINUE:"):
                value = line.split(":", 1)[1].strip().lower()
                return value == "false", True

            # STATUS: COMPLETE
            if line.startswith("STATUS:"):
                value = line.split(":", 1)[1].strip().upper()
                if value == "COMPLETE":
                    return True, True

        return False, False

    def detect_completion_language(self, output: str) -> tuple[bool, int]:
        """
        Detect completion language in output.
        Returns: (has_completion_language, confidence_boost)
        """
        confidence = 0
        has_completion = False

        for pattern in COMPLETION_PATTERNS:
            if re.search(pattern, output, re.IGNORECASE):
                has_completion = True
                confidence += 10
                break

        return has_completion, confidence

    def detect_test_only(self, output: str) -> bool:
        """Detect if response is test-only (no implementation)."""
        test_count = sum(1 for p in TEST_PATTERNS if re.search(p, output, re.IGNORECASE))
        impl_count = sum(1 for p in IMPLEMENTATION_PATTERNS if re.search(p, output, re.IGNORECASE))

        return test_count > 0 and impl_count == 0

    def detect_errors(self, output: str) -> int:
        """
        Count error indicators in output.
        Uses two-stage filtering to avoid JSON field false positives.
        """
        # Stage 1: Remove JSON field patterns
        lines = output.split("\n")
        filtered_lines = [
            line for line in lines
            if not re.search(r'"[^"]*error[^"]*"\s*:', line)
        ]
        filtered_text = "\n".join(filtered_lines)

        # Stage 2: Count actual errors
        error_patterns = [
            r'^Error:',
            r'^ERROR:',
            r'^\s*error:',
            r'\]: error',
            r'Link: error',
            r'Error occurred',
            r'failed with error',
            r'[Ee]xception',
            r'Fatal',
            r'FATAL',
            r'Traceback \(most recent call last\)',
        ]

        error_count = 0
        for pattern in error_patterns:
            error_count += len(re.findall(pattern, filtered_text, re.MULTILINE))

        return error_count

    def detect_file_changes(self) -> int:
        """Detect file changes via git."""
        try:
            import subprocess
            result = subprocess.run(
                ['git', 'diff', '--name-only'],
                capture_output=True,
                text=True,
                timeout=2
            )
            if result.returncode == 0:
                return len([line for line in result.stdout.split('\n') if line.strip()])
        except Exception:
            pass
        return 0

    def extract_phase_info(self, output: str) -> tuple[Optional[str], Optional[str]]:
        """Extract current Phase and Stage from output."""
        # Look for Phase/Stage markers
        phase_match = re.search(r'Phase\s+(\d+)', output, re.IGNORECASE)
        stage_match = re.search(r'Stage\s+(\d+)', output, re.IGNORECASE)

        phase = f"Phase {phase_match.group(1)}" if phase_match else None
        stage = f"Stage {stage_match.group(1)}" if stage_match else None

        return phase, stage

    def analyze_response(self, output: str) -> ResponseAnalysis:
        """Comprehensive response analysis exceeding Ralph's capabilities."""
        output_format = self.detect_output_format(output)

        # Extract EXIT_SIGNAL (5-tier extraction)
        exit_signal, explicit_exit_signal = self.extract_exit_signal(output, output_format)

        # Pattern detection
        has_completion_language, comp_confidence = self.detect_completion_language(output)
        is_test_only = self.detect_test_only(output)
        error_count = self.detect_errors(output)
        files_modified = self.detect_file_changes()

        # Phase/Stage tracking
        phase_name, stage_name = self.extract_phase_info(output)

        # Progress detection
        has_progress = files_modified > 0 or not is_test_only

        # Stuck detection (same error recurring)
        is_stuck = False
        if error_count > 5:
            # Extract error signature
            error_sig = self._extract_error_signature(output)
            if error_sig:
                if self.state.last_error == error_sig:
                    self.state.error_loop_count += 1
                    is_stuck = self.state.error_loop_count >= MAX_STUCK_LOOPS
                else:
                    self.state.last_error = error_sig
                    self.state.error_loop_count = 1

        # Confidence scoring (enhanced from Ralph)
        confidence = 0

        if output_format == "json":
            confidence += 50  # Structured output

        if explicit_exit_signal:
            confidence = 100  # Explicit signal = max confidence
        elif has_completion_language:
            confidence += comp_confidence

        if files_modified > 0:
            confidence += min(files_modified * 5, 30)  # Cap at +30

        if not has_progress:
            confidence += 20  # No progress = possible completion

        # Extract work summary
        work_summary = self._extract_summary(output, output_format)

        # Extract session ID
        session_id = self._extract_session_id(output, output_format)

        return ResponseAnalysis(
            loop_number=self.loop_number,
            timestamp=datetime.now().isoformat(),
            output_format=output_format,
            exit_signal=exit_signal,
            explicit_exit_signal=explicit_exit_signal,
            has_completion_language=has_completion_language,
            is_test_only=is_test_only,
            is_stuck=is_stuck,
            has_progress=has_progress,
            files_modified=files_modified,
            error_count=error_count,
            confidence_score=confidence,
            work_summary=work_summary,
            session_id=session_id,
            phase_name=phase_name,
            stage_name=stage_name
        )

    def _extract_error_signature(self, output: str) -> Optional[str]:
        """Extract normalized error signature for stuck detection."""
        # Find error messages
        errors = []
        for line in output.split("\n"):
            if re.search(r'(Error:|ERROR:|Exception|Traceback)', line):
                # Normalize: remove paths, line numbers, timestamps
                normalized = re.sub(r'/[^\s]+', '<path>', line)
                normalized = re.sub(r':\d+', ':<line>', normalized)
                normalized = re.sub(r'\d{4}-\d{2}-\d{2}', '<date>', normalized)
                errors.append(normalized)

        if not errors:
            return None

        # Return first 3 errors as signature
        return "\n".join(errors[:3])

    def _extract_summary(self, output: str, format: str) -> str:
        """Extract work summary from output."""
        if format == "json":
            try:
                data = json.loads(output)
                if isinstance(data, list):
                    data = next((m for m in data if m.get("type") == "result"), {})

                return (
                    data.get("summary", "") or
                    data.get("work_summary", "") or
                    data.get("result", "")[:200]
                )
            except Exception:
                pass

        # Text extraction: find summary-like content
        for line in output.split("\n"):
            if re.search(r'(summary|completed|implemented):', line, re.IGNORECASE):
                return line.strip()[:200]

        return "No explicit summary"

    def _extract_session_id(self, output: str, format: str) -> Optional[str]:
        """Extract session ID from output."""
        if format == "json":
            try:
                data = json.loads(output)
                if isinstance(data, list):
                    # Check init message first
                    for msg in data:
                        if msg.get("type") == "system" and msg.get("subtype") == "init":
                            if "session_id" in msg:
                                return msg["session_id"]
                    # Then check result
                    result = next((m for m in data if m.get("type") == "result"), {})
                    return result.get("sessionId") or result.get("session_id")

                return data.get("sessionId") or data.get("session_id")
            except Exception:
                pass

        return None

    def update_state(self, analysis: ResponseAnalysis):
        """Update exit state based on analysis."""
        # Update test_only_loops
        if analysis.is_test_only:
            self.state.test_only_loops.append(analysis.loop_number)
        elif analysis.has_progress:
            # Clear test loops if we made progress
            self.state.test_only_loops = []

        # Update done_signals (completion language detected)
        if analysis.has_completion_language:
            self.state.done_signals.append(analysis.loop_number)

        # Update completion_indicators (ONLY when EXIT_SIGNAL=true)
        # This is the critical fix from Ralph v0.11.1
        if analysis.exit_signal and analysis.explicit_exit_signal:
            self.state.completion_indicators.append(analysis.loop_number)

        # Update session tracking
        if analysis.session_id:
            if self.state.session_id != analysis.session_id:
                # New session started
                self.state.session_id = analysis.session_id
                self.state.session_started = analysis.timestamp
                # Reset exit signals for new session
                self.state.completion_indicators = []
                self.state.test_only_loops = []
                self.state.done_signals = []

        self._save_state()

    def check_exit_conditions(self, analysis: ResponseAnalysis) -> tuple[bool, str]:
        """
        Check if exit should be allowed.
        Returns: (should_exit, reason)
        """
        # Priority 1: Stuck on errors
        if analysis.is_stuck:
            return True, "stuck_on_errors"

        # Priority 2: Too many test-only loops
        recent_test_loops = [l for l in self.state.test_only_loops
                            if l >= analysis.loop_number - 5]
        if len(recent_test_loops) >= MAX_TEST_ONLY_LOOPS:
            return True, "test_saturation"

        # Priority 3: Safety circuit breaker (too many explicit exit signals)
        if len(self.state.completion_indicators) >= SAFETY_CIRCUIT_THRESHOLD:
            return True, "safety_circuit_breaker"

        # Priority 4: DUAL-CONDITION GATE (normal completion)
        recent_indicators = [i for i in self.state.completion_indicators
                            if i >= analysis.loop_number - 10]

        if len(recent_indicators) >= COMPLETION_THRESHOLD and analysis.exit_signal:
            return True, "project_complete"

        # Default: continue
        return False, "continuation_required"

    def enforce(self, output: str) -> dict:
        """
        Main enforcement logic.
        Returns decision with metadata.
        """
        # Analyze response
        analysis = self.analyze_response(output)

        # Update state
        self.update_state(analysis)

        # Check exit conditions
        should_exit, reason = self.check_exit_conditions(analysis)

        # Save analysis for inspection
        ANALYSIS_FILE.write_text(json.dumps(asdict(analysis), indent=2))

        # Append to history
        with HISTORY_FILE.open('a') as f:
            f.write(json.dumps(asdict(analysis)) + "\n")

        # Build decision
        decision = {
            "should_exit": should_exit,
            "reason": reason,
            "loop_number": analysis.loop_number,
            "exit_signal": analysis.exit_signal,
            "explicit": analysis.explicit_exit_signal,
            "completion_indicators": len(self.state.completion_indicators),
            "threshold_met": len(self.state.completion_indicators) >= COMPLETION_THRESHOLD,
            "gate_satisfied": len(self.state.completion_indicators) >= COMPLETION_THRESHOLD and analysis.exit_signal,
            "phase": analysis.phase_name,
            "stage": analysis.stage_name,
            "confidence": analysis.confidence_score
        }

        return decision


def main():
    """Hook entry point."""
    # Read output from environment or stdin
    output = ""

    # Check for tool output file from environment
    output_file = os.environ.get('CLAUDE_TOOL_OUTPUT')
    if output_file and Path(output_file).exists():
        try:
            output = Path(output_file).read_text()
        except Exception:
            pass

    # Fall back to stdin if available
    if not output and not sys.stdin.isatty():
        output = sys.stdin.read()

    if not output:
        # No output to validate - allow continuation
        sys.exit(0)

    # Enforce continuation
    validator = ResponseValidator()
    decision = validator.enforce(output)

    # Log decision
    print(f"\n[ENFORCE] Loop {decision['loop_number']}: {decision['reason']}", file=sys.stderr)
    print(f"[ENFORCE] EXIT_SIGNAL={decision['exit_signal']} (explicit={decision['explicit']})", file=sys.stderr)
    print(f"[ENFORCE] Indicators: {decision['completion_indicators']}/{COMPLETION_THRESHOLD}, Gate: {decision['gate_satisfied']}", file=sys.stderr)

    if decision['phase']:
        print(f"[ENFORCE] Current: {decision['stage'] or 'Stage ?'}, {decision['phase']}", file=sys.stderr)

    if not decision['should_exit']:
        print(f"[ENFORCE] ⚠️  CONTINUATION ENFORCED - Exit blocked (reason: {decision['reason']})", file=sys.stderr)
        # Create marker for wrapper script
        (ENFORCE_DIR / "CONTINUATION_REQUIRED").touch()

    # Always exit 0 from hook (don't interrupt tool execution)
    sys.exit(0)


if __name__ == "__main__":
    main()
