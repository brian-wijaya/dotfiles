# ~/.zshrc - Runs for interactive shells only

# If not running interactively, don't do anything
[[ -o interactive ]] || return

# --- IMMEDIATELY prevent tmux in Cursor IDE (run FIRST, before anything else) ---
# Cursor terminals should not use tmux (they have their own terminal management)
if [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]]; then
    export CURSOR_NO_TMUX=1
    # Aggressively kill any tmux attach processes immediately
    pkill -9 -f "tmux attach.*main" 2>/dev/null || true
    # If we're in tmux, detach immediately
    if [[ -n "$TMUX" ]]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE TMUX_PANE_WIDTH TMUX_PANE_HEIGHT
        exec zsh
    fi
fi

# --- Starship Prompt ---
eval "$(starship init zsh)"

# --- tmuxifier (tmux session templates) ---
# Install with: git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
if [[ -d "$HOME/.tmuxifier" ]] && [[ -f "$HOME/.tmuxifier/bin/tmuxifier" ]]; then
    # Initialize tmuxifier (suppress errors if it fails)
    eval "$($HOME/.tmuxifier/bin/tmuxifier init -)" 2>/dev/null || true
fi

# --- Aliases ---
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias ll='ls -la'
alias v='nvim'
alias cld='claude --dangerously-skip-permissions'

# --- Clipboard (Windows integration) ---
alias pbcopy='powershell.exe -Command "Set-Clipboard -Value \$input"'
alias pbpaste='powershell.exe -Command "Get-Clipboard"'

# Paste image from Windows clipboard to file, output path for Claude Code
pbimg() {
  local timestamp=$(date +%Y%m%d_%H%M%S)
  local imgdir="/mnt/c/Users/wijay/Dropbox/Downloads"
  local filename="clipboard_${timestamp}.png"
  local wslpath="${imgdir}/${filename}"
  local winpath="C:\\Users\\wijay\\Dropbox\\Downloads\\${filename}"

  # Ensure directory exists
  mkdir -p "$imgdir" 2>/dev/null

  # Save clipboard image using PowerShell
  powershell.exe -Command "
    \$img = Get-Clipboard -Format Image
    if (\$img) {
      \$img.Save('${winpath}')
      Write-Output 'saved'
    } else {
      Write-Output 'no-image'
    }
  " | grep -q 'saved'

  if [[ -f "$wslpath" ]]; then
    echo "$wslpath"
    # Also copy path to clipboard for easy pasting
    echo -n "$wslpath" | powershell.exe -Command "Set-Clipboard -Value \$input"
  else
    echo "No image in clipboard"
    return 1
  fi
}

# --- Disable bell ---
unsetopt BEEP

# --- History ---
HISTFILE="$XDG_STATE_HOME/zsh/history"
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# --- Default working directory ---
cd ~ 2>/dev/null

# --- zoxide (smart cd) ---
eval "$(zoxide init zsh)"

# --- atuin (shell history) ---
eval "$(atuin init zsh)"

# --- fzf (fuzzy finder) ---
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

# --- Auto-start tmux if not already inside tmux ---
# Skip if:
#   - WEZTERM_NOTMUX is set (for independent WezTerm tabs)
#   - CURSOR_AGENT is set (running in Cursor IDE - don't share tmux session with WezTerm)
#   - VSCODE_INJECTION is set (running in VSCode/Cursor integrated terminal)
#   - VSCODE_CWD is set (VSCode/Cursor working directory indicator)
#   - CURSOR_NO_TMUX is set (explicit flag to prevent tmux)
# Note: We check these BEFORE tmux starts, since TERM_PROGRAM becomes "tmux" after attaching

# In Cursor - prevent tmux entirely and kill any attach processes
if [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]]; then
    export CURSOR_NO_TMUX=1
    # Aggressively kill any tmux attach processes (including ones started after this script)
    pkill -9 -f "tmux attach.*main" 2>/dev/null || true
    # If we're somehow in tmux, detach immediately
    if [[ -n "$TMUX" ]]; then
        tmux detach 2>/dev/null
        unset TMUX TMUX_PANE
        exec zsh
    fi
    # Don't start tmux - exit early
    return 0 2>/dev/null || true
fi

# Only start tmux if we're NOT in Cursor and NOT already in tmux
if [[ -z "$TMUX" ]] && [[ -z "$WEZTERM_NOTMUX" ]] && [[ -z "$CURSOR_AGENT" ]] && [[ -z "$VSCODE_INJECTION" ]] && [[ -z "$VSCODE_CWD" ]] && [[ -z "$CURSOR_NO_TMUX" ]]; then
    tmux attach -t main 2>/dev/null || tmux new -s main -c ~
fi
export PATH="$HOME/bin:$PATH"
