#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# --- IMMEDIATELY prevent tmux in Cursor IDE (run FIRST, before anything else) ---
# Cursor terminals should not use tmux (they have their own terminal management)
if [ -n "$CURSOR_AGENT" ] || [ -n "$VSCODE_CWD" ] || [ -n "$VSCODE_INJECTION" ] || [ -n "$CURSOR_NO_TMUX" ]; then
    export CURSOR_NO_TMUX=1
    # Aggressively kill any tmux attach processes immediately
    pkill -9 -f "tmux attach.*main" 2>/dev/null || true
    # If we're in tmux, detach immediately
    if [ -n "$TMUX" ]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE TMUX_PANE_WIDTH TMUX_PANE_HEIGHT
        exec bash
    fi
fi

# --- PATH ---
# Put ~/.local/bin before /usr/bin so our tmux wrapper takes precedence
export PATH="$HOME/.local/bin:/usr/bin:$PATH"

# --- Go (for Mason's gopls installation) ---
# Mason uses 'go install' for gopls, so Go must be in PATH
if [ -d "/c/Program Files/Go/bin" ]; then
  export PATH="/c/Program Files/Go/bin:$PATH"
fi

# --- tmuxifier (tmux session templates) ---
# Install with: git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
if [ -d "$HOME/.tmuxifier" ] && [ -f "$HOME/.tmuxifier/bin/tmuxifier" ]; then
  export PATH="$HOME/.tmuxifier/bin:$PATH"
  # Initialize tmuxifier (suppress errors if it fails)
  eval "$($HOME/.tmuxifier/bin/tmuxifier init -)" 2>/dev/null || true
fi

# --- Aliases ---
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -la'
alias v='nvim'
alias cld='claude --dangerously-skip-permissions'

# --- Prompt ---
PS1='[\u@\h \W]\$ '

# --- Default working directory ---
cd ~/signal-assembly-platform 2>/dev/null

# --- zoxide (smart cd) ---
eval "$(zoxide init bash)"

# --- fzf (fuzzy finder) ---
[ -f /usr/share/fzf/key-bindings.bash ] && source /usr/share/fzf/key-bindings.bash
[ -f /usr/share/fzf/completion.bash ] && source /usr/share/fzf/completion.bash

# --- Auto-start tmux if not already inside tmux ---
# Skip if:
#   - WEZTERM_NOTMUX is set (for independent WezTerm tabs)
#   - CURSOR_AGENT is set (running in Cursor IDE - don't share tmux session with WezTerm)
#   - VSCODE_INJECTION is set (running in VSCode/Cursor integrated terminal)
#   - VSCODE_CWD is set (VSCode/Cursor working directory indicator)
#   - CURSOR_NO_TMUX is set (explicit flag to prevent tmux)
# Note: We check these BEFORE tmux starts, since TERM_PROGRAM becomes "tmux" after attaching

# In Cursor - prevent tmux entirely and kill any attach processes
if [ -n "$CURSOR_AGENT" ] || [ -n "$VSCODE_CWD" ] || [ -n "$VSCODE_INJECTION" ] || [ -n "$CURSOR_NO_TMUX" ]; then
    export CURSOR_NO_TMUX=1
    # Aggressively kill any tmux attach processes (including ones started after this script)
    pkill -9 -f "tmux attach.*main" 2>/dev/null || true
    # If we're somehow in tmux, detach immediately
    if [ -n "$TMUX" ]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE
        exec bash
    fi
    # Don't start tmux - exit early
    return 0 2>/dev/null || true
fi

# Only start tmux if we're NOT in Cursor and NOT already in tmux
if [ -z "$TMUX" ] && [ -z "$WEZTERM_NOTMUX" ] && [ -z "$CURSOR_AGENT" ] && [ -z "$VSCODE_INJECTION" ] && [ -z "$VSCODE_CWD" ] && [ -z "$CURSOR_NO_TMUX" ]; then
    tmux attach -t main 2>/dev/null || tmux new -s main -c ~/signal-assembly-platform
fi
