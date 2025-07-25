#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

# Pin libinput to version 1.28.0-1.fc42 because 1.28.902-1.fc42 introduced a
# regression in device quirks, which in turn prevents us from disabling
# high-resolution scrolling.
#
# https://gitlab.freedesktop.org/libinput/libinput/-/issues/1160
#

sudo dnf downgrade -y --allowerasing https://kojipkgs.fedoraproject.org/packages/libinput/1.28.0/1.fc42/x86_64/libinput-1.28.0-1.fc42.x86_64.rpm
sudo dnf versionlock add libinput-1.28.0-1.fc42
