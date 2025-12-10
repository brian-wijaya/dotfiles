#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# --- PATH ---
export PATH="/usr/bin:$HOME/.local/bin:$PATH"

# --- Aliases ---
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -la'
alias v='nvim'

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
# Skip if WEZTERM_NOTMUX is set (for independent tabs)
if [ -z "$TMUX" ] && [ -z "$WEZTERM_NOTMUX" ]; then
    tmux attach -t main 2>/dev/null || tmux new -s main -c ~/signal-assembly-platform
fi
