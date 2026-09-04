#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

if ! command -v git-credential-manager > /dev/null 2>&1; then
  curl --fail --location \
    --connect-timeout 10 \
    --retry 5 \
    --retry-all-errors \
    --output install-from-source.sh \
    "https://raw.githubusercontent.com/git-ecosystem/git-credential-manager/main/build/install-from-source.sh"

  sh install-from-source.sh -y --install-prefix=/usr
fi

sudo rm -rf dotnet-install.sh      || true
sudo rm -rf install-from-source.sh || true
sudo rm -rf git-credential-manager || true
