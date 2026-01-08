#!/bin/bash
# Polybar launch script

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Wait for i3 IPC socket to be ready
while ! i3-msg -t get_version >/dev/null 2>&1; do sleep 0.5; done

# Launch Polybar
polybar main 2>&1 | tee -a /tmp/polybar.log & disown

echo "Polybar launched..."
