#!/bin/bash
# Polybar launch script
# Only kills polybar instances on the CURRENT display to avoid cross-display damage.

# Terminate polybar instances on this display only (not other displays like :99)
for pid in $(pgrep -u "$UID" -x polybar); do
    if grep -qz "DISPLAY=${DISPLAY:-:0}" /proc/"$pid"/environ 2>/dev/null; then
        kill "$pid" 2>/dev/null
    fi
done

# Wait until our polybar instances have been shut down
while true; do
    found=0
    for pid in $(pgrep -u "$UID" -x polybar); do
        if grep -qz "DISPLAY=${DISPLAY:-:0}" /proc/"$pid"/environ 2>/dev/null; then
            found=1
            break
        fi
    done
    [ "$found" -eq 0 ] && break
    sleep 1
done

# Wait for i3 IPC socket to be ready
while ! i3-msg -t get_version >/dev/null 2>&1; do sleep 0.5; done

# Launch Polybar
polybar main 2>&1 | tee -a /tmp/polybar.log & disown

echo "Polybar launched..."
