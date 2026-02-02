# ~/.bashrc - matching fish feature parity (X11 native)

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# --- IDE terminal detection (prevent tmux) ---
# Covers: Cursor, VSCode, IntelliJ, PyCharm, Junie, etc.
_in_ide=0
if [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]]; then
    _in_ide=1
elif [[ "$TERM_PROGRAM" == "vscode" ]]; then
    _in_ide=1
elif [[ "$PATH" == *"cursor/resources/app/bin"* ]] || [[ "$PATH" == *"Code/bin"* ]]; then
    _in_ide=1
elif [[ "$TERMINAL_EMULATOR" == *"JetBrains"* ]]; then
    _in_ide=1
elif [[ -n "$JETBRAINS_INTELLIJ_ZSH_DIR" ]] || [[ -n "$INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED" ]]; then
    _in_ide=1
fi

if [[ $_in_ide -eq 1 ]]; then
    export CURSOR_NO_TMUX=1
    pkill -9 -f "tmux attach.*main" 2>/dev/null || true
    if [[ -n "$TMUX" ]]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE
        exec bash
    fi
fi

# --- PATH ---
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.config/emacs/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# --- Environment ---
export DOOMDIR="$HOME/.config/doom"
export NLTK_DATA="$HOME/.cache/nltk_data"

# --- Bash completion ---
[[ -r /usr/share/bash-completion/bash_completion ]] && source /usr/share/bash-completion/bash_completion

# --- Starship prompt ---
eval "$(starship init bash)"

# --- tmuxifier (tmux session templates) ---
if [[ $_in_ide -eq 0 ]] && [[ -d "$HOME/.tmuxifier" ]] && [[ -f "$HOME/.tmuxifier/bin/tmuxifier" ]]; then
    export PATH="$HOME/.tmuxifier/bin:$PATH"
    eval "$($HOME/.tmuxifier/bin/tmuxifier init -)" 2>/dev/null || true
fi

# --- Aliases ---
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -la'
alias v='nvim'
alias cld='claude --dangerously-skip-permissions'

# eza aliases (if available)
if command -v eza >/dev/null 2>&1; then
    alias ls='eza -lh --group-directories-first --icons=auto'
    alias lsa='eza -lha --group-directories-first --icons=auto'
    alias lt='eza --tree --level=2 --long --icons --git'
    alias lta='eza --tree --level=2 --long --icons --git -a'
fi

# --- Clipboard (X11) ---
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# --- Navigation ---
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# --- Tools ---
alias c='claude'
alias d='docker'
alias g='git'
alias lg='lazygit'
alias ld='lazydocker'

# --- Git aliases ---
alias gcm='git commit -m'
alias gcam='git commit -a -m'
alias gcad='git commit -a --amend'
alias gst='git status'
alias gd='git diff'
alias gp='git push'
alias gl='git pull'

# --- Fuzzy finder ---
alias ff='fzf --preview "bat --style=numbers --color=always {}"'

# --- Functions ---
open() {
    xdg-open "$@" &>/dev/null &
}

n() {
    if [[ $# -eq 0 ]]; then
        nvim .
    else
        nvim "$@"
    fi
}

# --- History ---
HISTFILE="$HOME/.bash_history"
HISTSIZE=10000
HISTFILESIZE=10000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend

# --- zoxide (smart cd) ---
eval "$(zoxide init bash)"

# --- atuin (shell history) ---
if command -v atuin >/dev/null 2>&1; then
    eval "$(atuin init bash)"
fi

# --- mise (polyglot version manager) ---
if [[ -f "$HOME/.local/bin/mise" ]]; then
    eval "$($HOME/.local/bin/mise activate bash)"
fi

# --- fzf (fuzzy finder) ---
[[ -f /usr/share/fzf/key-bindings.bash ]] && source /usr/share/fzf/key-bindings.bash
[[ -f /usr/share/fzf/completion.bash ]] && source /usr/share/fzf/completion.bash

# --- tmux: Update pane current path on directory change ---
if [[ -n "$TMUX" ]]; then
    _update_tmux_pwd() {
        printf '\033]7;file://%s%s\033\\' "${HOSTNAME:-localhost}" "$PWD"
    }
    PROMPT_COMMAND="_update_tmux_pwd${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi

# --- Auto-start tmux ---
# Skip if in IDE terminal or already in tmux
if [[ $_in_ide -eq 0 ]] && [[ -z "$TMUX" ]] && [[ -z "$WEZTERM_NOTMUX" ]]; then
    tmux attach -t main 2>/dev/null || tmux new -s main -c ~
fi
