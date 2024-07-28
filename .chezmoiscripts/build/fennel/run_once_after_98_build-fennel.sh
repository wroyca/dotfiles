#!/usr/bin/env bash

set -x # Print trace of simple commands.
set +e # Ignore pipeline that returns a non-zero status.

if command -v fennel > /dev/null 2>&1; then
  exit 0
fi

owd="$(pwd)"
ret ()
{
  cd "$owd"
}

trap ret EXIT

mkdir -p /tmp/fennel && cd $_

git init .
git remote add --no-fetch origin https://git.sr.ht/~technomancy/fennel --no-tags
git config remote.origin.fetch "+refs/heads/main:refs/remotes/origin/main"
git fetch -v --depth=1
git pull origin main

sudo make install PREFIX=/usr

