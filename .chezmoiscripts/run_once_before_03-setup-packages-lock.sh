#!/usr/bin/env bash

set -x # Print trace of simple commands.

# Pin bluez to 5.73.
#
# Starting with 5.74, newly introduced bt_uhid_replay will attempts to
# deallocate memory that has already been freed once or that was never
# allocated in the first place. This causes some devices to break when
# resuming from suspend state.
#
# https://github.com/bluez/bluez/issues/838
#
if bluetoothctl --version | grep -q "5.73"; then
  exit 0
fi

sudo dnf swap -y \
  bluez https://kojipkgs.fedoraproject.org/packages/bluez/5.73/3.fc40/x86_64/bluez-5.73-3.fc40.x86_64.rpm

sudo dnf install -y \
  'dnf-command(versionlock)'

sudo dnf versionlock add \
  bluez-obexd

sudo dnf versionlock add \
  bluez-cups

sudo dnf versionlock add \
  bluez-libs
