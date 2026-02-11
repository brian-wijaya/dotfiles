#!/usr/bin/env python3
"""
E2E Test Debug Loop skill wrapper.

Thin wrapper that invokes the e2e-test program from vault/programs/.
"""

import sys
import subprocess
from pathlib import Path

# E2E test program location
E2E_TEST_PROGRAM = Path.home() / "vault" / "programs" / "e2e-test" / "scripts" / "run_tests.py"

def main():
    """Forward all arguments to the e2e-test program."""
    if not E2E_TEST_PROGRAM.exists():
        print(f"ERROR: E2E test program not found at {E2E_TEST_PROGRAM}", file=sys.stderr)
        print("Run: pip install -e ~/vault/programs/e2e-test/", file=sys.stderr)
        sys.exit(1)

    # Pass through all command-line arguments
    cmd = [sys.executable, str(E2E_TEST_PROGRAM)] + sys.argv[1:]

    try:
        result = subprocess.run(cmd, check=False)
        sys.exit(result.returncode)
    except KeyboardInterrupt:
        print("\nTest execution interrupted", file=sys.stderr)
        sys.exit(130)
    except Exception as e:
        print(f"ERROR: Failed to run e2e-test: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
