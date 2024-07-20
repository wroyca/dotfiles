#!/usr/bin/env bash

set -x # Print trace of simple commands.
set +e # Ignore pipeline that returns a non-zero status.

if command -v neovide > /dev/null 2>&1; then
  exit 0
fi

owd="$(pwd)"
ret ()
{
  cd "$owd"
}

trap ret EXIT

mkdir -p /tmp/neovide && cd $_

cargo install --git \
  https://github.com/neovide/neovide.git
