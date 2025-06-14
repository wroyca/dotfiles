#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

if ! command -v b > /dev/null 2>&1; then
 v=$(curl -sSf https://stage.build2.org/0/toolchain.sha256 | sed -n 's,^.*/build2-install-\(.*\)-stage\.sh$,\1,p')
  curl --connect-timeout 10 --retry 5 --retry-all-errors -O \
    "https://stage.build2.org/0/$v/build2-install-$v-stage.sh"
  sh build2-install-$v-stage.sh --local --yes /usr
fi

sudo rm -rf build2-* || true
