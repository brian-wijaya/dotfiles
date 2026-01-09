# ~/.zshrc - matching fish feature parity (X11 native)

# If not running interactively, don't do anything
[[ -o interactive ]] || return

# --- IDE/embedded terminal detection (prevent tmux) ---
# Covers: Cursor, VSCode, IntelliJ, PyCharm, Emacs vterm, etc.
_in_ide=0
if [[ -n "$INSIDE_EMACS" ]] || [[ -n "$EMACS_VTERM_PATH" ]]; then
    _in_ide=1
elif [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]]; then
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
    if [[ -n "$TMUX" ]]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE
        exec zsh
    fi
fi

# --- PATH ---
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.config/emacs/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# --- Environment ---
export DOOMDIR="$HOME/.config/doom"

# --- Starship prompt ---
eval "$(starship init zsh)"

# --- tmuxifier (tmux session templates) ---
if [[ $_in_ide -eq 0 ]] && [[ -d "$HOME/.tmuxifier" ]] && [[ -f "$HOME/.tmuxifier/bin/tmuxifier" ]]; then
    export PATH="$HOME/.tmuxifier/bin:$PATH"
    eval "$($HOME/.tmuxifier/bin/tmuxifier init -)" >/dev/null 2>&1 || true
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

# --- Disable bell ---
unsetopt BEEP

# --- Key bindings ---
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char
bindkey '^[[3~' delete-char
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line

# --- History ---
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# --- zoxide (smart cd) ---
eval "$(zoxide init zsh)"

# --- atuin (shell history) ---
if command -v atuin >/dev/null 2>&1; then
    eval "$(atuin init zsh)"
fi

# --- fzf (fuzzy finder) ---
[[ -f /usr/share/fzf/key-bindings.zsh ]] && source /usr/share/fzf/key-bindings.zsh
[[ -f /usr/share/fzf/completion.zsh ]] && source /usr/share/fzf/completion.zsh

# --- tmux: Update pane current path on directory change ---
if [[ -n "$TMUX" ]]; then
    _update_tmux_pwd() {
        printf '\033]7;file://%s%s\033\\' "${HOST:-localhost}" "$PWD"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook chpwd _update_tmux_pwd
    add-zsh-hook precmd _update_tmux_pwd
    _update_tmux_pwd
fi

# --- Auto-start tmux ---
# Skip if in IDE terminal or already in tmux
# Each terminal gets its own session (no sharing)
if [[ $_in_ide -eq 0 ]] && [[ -z "$TMUX" ]] && [[ -z "$WEZTERM_NOTMUX" ]]; then
    tmux new -s "term-$$" -c ~
fi
