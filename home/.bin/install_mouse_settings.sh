#!/bin/bash

echo Copying the mouse control script to /usr/local/bin/
sudo cp ~/.bin/mouse-ctl /usr/local/bin/
sudo chmod +x /usr/local/bin/mouse-ctl

echo Placing udev rules to /etc/udev/rules.d/10-mouse.rules
sudo bash -c 'cat > /etc/udev/rules.d/10-mouse.rules << __EOF__
SUBSYSTEM=="input", ACTION=="add", ENV{DEVNAME}=="/dev/input/mouse?" RUN+="/usr/local/bin/mouse-ctl"
__EOF__'

echo Reloading udev rules ...
sudo udevadm control --reload-rules
