#!/usr/bin/env bash

set -x # Print trace of simple commands.
set +e # Ignore pipeline that returns a non-zero status.

if ! command -v b > /dev/null 2>&1; then
  exit 0
fi

if command -v cli > /dev/null 2>&1; then
  exit 0
fi

owd="$(pwd)"
ret ()
{
  cd "$owd" || exit
}

trap ret EXIT

cd /tmp || exit

# Create the build configuration
bpkg create -d cli-gcc cc   \
  config.cxx=g++            \
  config.cc.coptions=-O3    \
  config.bin.rpath=/usr/lib \
  config.install.root=/usr  \
  config.install.sudo=sudo

cd cli-gcc || return
bpkg build \
  cli@https://git.codesynthesis.com/cli/cli.git#master

bpkg test cli
bpkg install cli
