#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

if [ -f ~/.no-reboot ]; then
  exit 0
fi

touch ~/.no-reboot && reboot
