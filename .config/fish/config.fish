# Fish config - matching zsh feature parity

# If not running interactively, don't do anything
status is-interactive; or return

# --- IDE/embedded terminal detection (prevent tmux) ---
# Covers: Cursor, VSCode, IntelliJ, PyCharm, Emacs vterm, etc.
set -l in_ide 0
if set -q INSIDE_EMACS; or set -q EMACS_VTERM_PATH
    set in_ide 1
else if set -q CURSOR_AGENT; or set -q VSCODE_CWD; or set -q VSCODE_INJECTION; or set -q CURSOR_NO_TMUX
    set in_ide 1
else if test "$TERM_PROGRAM" = "vscode"
    set in_ide 1
else if string match -q "*cursor/resources/app/bin*" "$PATH"
    set in_ide 1
else if string match -q "*Code/bin*" "$PATH"
    set in_ide 1
else if string match -q "*JetBrains*" "$TERMINAL_EMULATOR"
    set in_ide 1
else if set -q JETBRAINS_INTELLIJ_ZSH_DIR; or set -q INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED
    set in_ide 1
end

if test $in_ide -eq 1
    set -gx CURSOR_NO_TMUX 1
    if set -q TMUX
        tmux detach 2>/dev/null
        set -e TMUX TMUX_PANE
        exec fish
    end
end

# --- PATH ---
fish_add_path ~/bin ~/.local/bin ~/.config/emacs/bin $GOPATH/bin

# --- Environment ---
set -gx GOPATH $HOME/go
set -gx DOOMDIR ~/.config/doom
set -gx NLTK_DATA $HOME/.cache/nltk_data

# --- Starship prompt ---
if type -q starship
    starship init fish | source
end

# --- tmuxifier (tmux session templates) ---
if test $in_ide -eq 0; and test -d "$HOME/.tmuxifier"; and test -f "$HOME/.tmuxifier/bin/tmuxifier"
    fish_add_path ~/.tmuxifier/bin
    eval ($HOME/.tmuxifier/bin/tmuxifier init - fish) 2>/dev/null
end

# --- Aliases ---
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -la'
alias v='nvim'
alias cld='claude --dangerously-skip-permissions'

# eza aliases (if available)
if type -q eza
    alias ls='eza -lh --group-directories-first --icons=auto'
    alias lsa='eza -lha --group-directories-first --icons=auto'
    alias lt='eza --tree --level=2 --long --icons --git'
    alias lta='eza --tree --level=2 --long --icons --git -a'
end

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
function open
    xdg-open $argv &>/dev/null &
end

function n
    if test (count $argv) -eq 0
        nvim .
    else
        nvim $argv
    end
end

# --- Disable bell ---
set -g fish_bell_command ''

# --- zoxide (smart cd) ---
if type -q zoxide
    zoxide init fish | source
end

# --- atuin (shell history) ---
if type -q atuin
    atuin init fish | source
end

# --- fzf (fuzzy finder) ---
if test -f /usr/share/fish/vendor_functions.d/fzf_key_bindings.fish
    source /usr/share/fish/vendor_functions.d/fzf_key_bindings.fish
    fzf_key_bindings
end

# --- tmux: Update pane current path on directory change ---
if set -q TMUX
    function __update_tmux_pwd --on-variable PWD
        printf '\033]7;file://%s%s\033\\' (cat /etc/hostname 2>/dev/null; or echo "localhost") "$PWD"
    end
    __update_tmux_pwd
end

# --- Auto-start tmux ---
# Skip if in IDE terminal or already in tmux
# Each terminal gets its own session (no sharing)
if test $in_ide -eq 0; and not set -q TMUX; and not set -q WEZTERM_NOTMUX
    tmux new -s "term-$fish_pid" -c ~
end
