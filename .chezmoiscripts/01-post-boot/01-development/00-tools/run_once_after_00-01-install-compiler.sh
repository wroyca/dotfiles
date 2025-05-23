#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

sudo dnf install -y g++
sudo dnf install -y gcc-plugin-devel
sudo dnf install -y clang
sudo dnf install -y clang-tools-extra
sudo dnf install -y libcxx
sudo dnf install -y libcxx-devel
