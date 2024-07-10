#!/usr/bin/env bash

set -x # Print trace of simple commands.
set +e # Ignore errors, if any.

if command -v zed > /dev/null 2>&1; then
  exit 0
fi

owd="$(pwd)"
ret ()
{
  cd "$owd"
}

trap ret EXIT

mkdir -p /tmp/zed && cd $_

sudo dnf install -y \
  'pkgconfig(xkbcommon-x11)'

cargo install --git \
  https://github.com/zed-industries/zed.git zed
