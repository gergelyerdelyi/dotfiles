#!/bin/bash

# Find an attached AudioEngine D1 USB DAC and perform a device reset on it after
# system resume. This fixes the problem when sound playback is completely garbled
# due to the sample rate getting messed up by the driver after resume, as noted in
# the kernel log:
#
# usb 1-2: current rate 4500480 is different from the runtime rate 44100
#
# Place it to /usr/lib/systemd/system-sleep/reset-audioengine-d1.sh
#

if [ $1 == "post" ]; then
    # Find the AudioEngine D1 by its vendor and product id
    AUDIO_DEVICE=$(lsusb -d 1101:0003 | sed -e 's/:.*$//' -e 's|Bus |/dev/bus/usb/|' -e 's| Device |/|')
    # Perform a device reset
    python -c "import fcntl; f=open(\"${AUDIO_DEVICE}\", 'w'); fcntl.ioctl(f, 21780, 0)"
fi
