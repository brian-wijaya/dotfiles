#!/usr/bin/env python3
"""
Autonomous Work Skill
Execute tasks from a plan file autonomously and report results.
"""

import subprocess
import sys
from pathlib import Path

def main():
    # Get plan file path from args or use most recent
    if len(sys.argv) > 1:
        plan_file = Path(sys.argv[1]).expanduser()
    else:
        # Find most recent plan file
        plans_dir = Path.home() / ".claude/plans"
        if not plans_dir.exists():
            print("❌ No plans directory found at ~/.claude/plans")
            return 1

        plan_files = sorted(plans_dir.glob("*.md"), key=lambda p: p.stat().st_mtime, reverse=True)
        if not plan_files:
            print("❌ No plan files found in ~/.claude/plans")
            return 1

        plan_file = plan_files[0]

    if not plan_file.exists():
        print(f"❌ Plan file not found: {plan_file}")
        return 1

    print(f"🚀 Executing autonomous work from: {plan_file.name}\n")

    # Run autonomous worker with quiet mode
    worker_path = Path.home() / "vault/programs/claude-coordinator/autonomous-worker.py"
    venv_python = Path.home() / "vault/programs/claude-coordinator/.venv/bin/python"

    if not worker_path.exists():
        print(f"❌ Autonomous worker not found at: {worker_path}")
        return 1

    try:
        result = subprocess.run(
            [
                str(venv_python),
                "-u",  # Unbuffered output
                str(worker_path),
                "--plan", str(plan_file),
                "--quiet",
                "--heartbeat", "5",
            ],
            timeout=600,  # 10 minute timeout
        )

        return result.returncode

    except subprocess.TimeoutExpired:
        print("\n⚠️  Timeout: Work is still in progress. Check coordinator status.")
        return 1
    except KeyboardInterrupt:
        print("\n⚠️  Interrupted by user")
        return 1
    except Exception as e:
        print(f"\n❌ Error: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
