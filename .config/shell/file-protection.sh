#!/bin/bash
# File Protection Layer - Safe wrappers for destructive operations
# Source this in ~/.bashrc or ~/.zshrc

# Use trash instead of rm for safety
alias rm='trash-put'
alias rm-real='/usr/bin/rm'  # Escape hatch for when you really need it

# Safe copy - backup before overwriting
safe_cp() {
    local force=false
    local interactive=false
    local args=()

    # Parse flags
    while [[ $# -gt 0 ]]; do
        case $1 in
            -f|--force) force=true; shift ;;
            -i|--interactive) interactive=true; shift ;;
            *) args+=("$1"); shift ;;
        esac
    done

    # Check if destination exists and will be overwritten
    if [[ ${#args[@]} -ge 2 ]]; then
        local dest="${args[-1]}"
        if [[ -f "$dest" && "$force" != "true" ]]; then
            # Save version before overwrite
            if command -v file-version-daemon >/dev/null 2>&1; then
                python3 -c "
from pathlib import Path
import sys
sys.path.insert(0, str(Path.home() / 'bin'))
from file_version_daemon import VersionStore
store = VersionStore()
store.save_version(Path('$dest'), 'overwrite')
" 2>/dev/null
            fi
        fi
    fi

    # Execute actual cp
    /usr/bin/cp "${args[@]}"
}

# Safe move - backup source before moving
safe_mv() {
    local args=("$@")

    # Save version of source file before move
    if [[ ${#args[@]} -ge 1 && -f "${args[0]}" ]]; then
        local source="${args[0]}"
        if command -v file-version-daemon >/dev/null 2>&1; then
            python3 -c "
from pathlib import Path
import sys
sys.path.insert(0, str(Path.home() / 'bin'))
from file_version_daemon import VersionStore
store = VersionStore()
store.save_version(Path('$source'), 'move')
" 2>/dev/null
        fi
    fi

    # Execute actual mv
    /usr/bin/mv "$@"
}

# Optionally alias cp and mv (commented out by default - opt-in)
# alias cp='safe_cp'
# alias mv='safe_mv'

# Recovery aliases
alias unrm='trash-restore'  # Restore from trash
alias file-history='file-versions list'
alias file-restore='file-versions restore'

# Show trash size
trash-size() {
    du -sh ~/.local/share/Trash 2>/dev/null || echo "Trash is empty"
}

# Empty trash older than N days
trash-empty() {
    local days=${1:-30}
    trash-empty --dry-run "$days"
    read -p "Empty trash items older than $days days? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        trash-empty "$days"
        echo "✓ Trash emptied"
    fi
}
