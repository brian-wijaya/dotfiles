#
# ~/.bash_profile
#

# Add ~/bin to PATH
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Source bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc

# Start X on login to tty1
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi
. "$HOME/.cargo/env"
