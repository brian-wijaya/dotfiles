# ~/.zshenv - Runs for ALL zsh instances (login, interactive, non-interactive)

# XDG Base Directory Specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# PATH
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"

# Readline config (XDG location)
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

# Editor
export EDITOR="nvim"
export VISUAL="nvim"

# Terminal
export TERM="xterm-256color"
export LANG="en_US.UTF-8"

# Browser (X11)
export BROWSER="xdg-open"

# tmuxifier (tmux session templates)
if [[ -d "$HOME/.tmuxifier" ]] && [[ -f "$HOME/.tmuxifier/bin/tmuxifier" ]]; then
    export PATH="$HOME/.tmuxifier/bin:$PATH"
fi

# GitHub Personal Access Token (for MCP server and other tools)
if [[ -f "$HOME/.config/gh/hosts.yml" ]]; then
    export GITHUB_PERSONAL_ACCESS_TOKEN=$(grep -m1 'oauth_token:' "$HOME/.config/gh/hosts.yml" | awk '{print $2}')
fi
