#!/usr/bin/env bash

# Pin bluez to version 5.73.
#
# Starting with version 5.74, the newly introduced bt_uhid_replay feature
# attempts to deallocate memory that has already been freed or that was never
# allocated, causing some devices to malfunction when resuming from a suspend
# state.
#
# https://github.com/bluez/bluez/issues/838
#
if bluetoothctl --version | grep -q "5.73"; then
  exit 0
fi

sudo dnf swap -y bluez https://kojipkgs.fedoraproject.org/packages/bluez/5.73/3.fc40/x86_64/bluez-5.73-3.fc40.x86_64.rpm

sudo dnf install -y \
  'dnf-command(versionlock)'

sudo dnf versionlock add bluez-obexd
sudo dnf versionlock add bluez-cups
sudo dnf versionlock add bluez-libs
