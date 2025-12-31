# ~/.zshrc - Runs for interactive shells only

# If not running interactively, don't do anything
[[ -o interactive ]] || return

# --- IMMEDIATELY prevent tmux in IDE terminals (run FIRST, before anything else) ---
# IDE terminals should not use tmux (they have their own terminal management)
# Covers: Cursor, VSCode, IntelliJ, PyCharm, Junie, etc.
if [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]] || [[ "$TERM_PROGRAM" == "vscode" ]] || [[ "$TERMINAL_EMULATOR" == *"JetBrains"* ]] || [[ -n "$JETBRAINS_INTELLIJ_ZSH_DIR" ]] || [[ -n "$INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED" ]]; then
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
# Skip in IDE terminals where tmux is disabled
if [[ -z "$CURSOR_NO_TMUX" ]] && [[ -z "$CURSOR_AGENT" ]] && [[ -z "$VSCODE_CWD" ]] && [[ -z "$VSCODE_INJECTION" ]] && [[ "$TERM_PROGRAM" != "vscode" ]] && [[ "$TERMINAL_EMULATOR" != *"JetBrains"* ]] && [[ -z "$JETBRAINS_INTELLIJ_ZSH_DIR" ]] && [[ -z "$INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED" ]]; then
    if [[ -d "$HOME/.tmuxifier" ]] && [[ -f "$HOME/.tmuxifier/bin/tmuxifier" ]]; then
        # Initialize tmuxifier (suppress all output and errors)
        eval "$($HOME/.tmuxifier/bin/tmuxifier init -)" >/dev/null 2>&1 || true
    fi
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

# --- Fix backspace and delete keys ---
# Ensure backspace works correctly (sends ^? or ^H)
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char
# Ensure delete key works correctly (sends ^[[3~)
bindkey '^[[3~' delete-char
# Fix home/end keys
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line

# --- History ---
HISTFILE="$XDG_STATE_HOME/zsh/history"
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# --- tmux: Update pane current path on directory change ---
# Send OSC 7 escape sequence so tmux can track the working directory
# This is what makes pane_current_path work for split-window -c
if [[ -n "$TMUX" ]]; then
  function _update_tmux_pwd() {
    # OSC 7: file://hostname/path - standard way to notify terminal of directory change
    printf '\033]7;file://%s%s\033\\' "${HOST:-localhost}" "$PWD"
  }
  autoload -Uz add-zsh-hook
  add-zsh-hook chpwd _update_tmux_pwd
  add-zsh-hook precmd _update_tmux_pwd
  _update_tmux_pwd  # Initialize on shell start
fi

# --- Default working directory ---
if [[ -o login ]]; then
    cd ~ 2>/dev/null
fi

# --- zoxide (smart cd) ---
eval "$(zoxide init zsh)"

# --- atuin (shell history) ---
eval "$(atuin init zsh)"

# --- fzf (fuzzy finder) ---
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh

# --- Auto-start tmux if not already inside tmux ---
# Skip if in any IDE terminal (Cursor, VSCode, IntelliJ, PyCharm, Junie, etc.)
# or if WEZTERM_NOTMUX is set (for independent WezTerm tabs)

# In IDE terminals - prevent tmux entirely
if [[ -n "$CURSOR_AGENT" ]] || [[ -n "$VSCODE_CWD" ]] || [[ -n "$VSCODE_INJECTION" ]] || [[ -n "$CURSOR_NO_TMUX" ]] || [[ "$TERM_PROGRAM" == "vscode" ]] || [[ "$TERMINAL_EMULATOR" == *"JetBrains"* ]] || [[ -n "$JETBRAINS_INTELLIJ_ZSH_DIR" ]] || [[ -n "$INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED" ]]; then
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

# Only start tmux if we're NOT in an IDE terminal and NOT already in tmux
if [[ -z "$TMUX" ]] && [[ -z "$WEZTERM_NOTMUX" ]] && [[ -z "$CURSOR_AGENT" ]] && [[ -z "$VSCODE_INJECTION" ]] && [[ -z "$VSCODE_CWD" ]] && [[ -z "$CURSOR_NO_TMUX" ]] && [[ "$TERM_PROGRAM" != "vscode" ]] && [[ -z "$JETBRAINS_INTELLIJ_ZSH_DIR" ]] && [[ -z "$INTELLIJ_TERMINAL_COMMAND_BLOCKS_REWORKED" ]] && [[ "$TERMINAL_EMULATOR" != *"JetBrains"* ]]; then
    tmux attach -t main 2>/dev/null || tmux new -s main -c ~
fi
export PATH="$HOME/bin:$PATH"
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
