#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

sudo dnf swap -y ptyxis kitty

systemctl --user enable kitty.service
systemctl --user start  kitty.service
