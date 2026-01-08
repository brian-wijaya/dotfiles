# Fish config - X11 migration

# Auto-start X on tty1 (disabled - using SDDM)
# if test -z "$DISPLAY"; and test (tty) = "/dev/tty1"
#     exec startx
# end

# X11 clipboard aliases
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

# File system aliases (eza)
alias ls='eza -lh --group-directories-first --icons=auto'
alias lsa='eza -lha --group-directories-first --icons=auto'
alias lt='eza --tree --level=2 --long --icons --git'
alias lta='eza --tree --level=2 --long --icons --git -a'

# Navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Tools
alias c='claude'
alias d='docker'
alias g='git'
alias lg='lazygit'
alias ld='lazydocker'

# Git aliases
alias gcm='git commit -m'
alias gcam='git commit -a -m'
alias gcad='git commit -a --amend'
alias gst='git status'
alias gd='git diff'
alias gp='git push'
alias gl='git pull'

# Fuzzy finder
alias ff='fzf --preview "bat --style=numbers --color=always {}"'

# Open function
function open
    xdg-open $argv &>/dev/null &
end

# Neovim (opens current dir if no args)
function n
    if test (count $argv) -eq 0
        nvim .
    else
        nvim $argv
    end
end

# Initialize starship prompt
if type -q starship
    starship init fish | source
end

# Initialize zoxide (smart cd)
if type -q zoxide
    zoxide init fish | source
end

# Add ~/bin to PATH
fish_add_path ~/bin ~/.local/bin
