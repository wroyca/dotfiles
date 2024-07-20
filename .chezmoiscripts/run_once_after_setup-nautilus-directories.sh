#!/usr/bin/env bash

set -x # Print trace of simple commands.
set -e # Exit immediately if a pipeline returns a non-zero status.

cd ~/

[[ ! -d Projects ]] && mkdir Projects

gio set Projects metadata::custom-icon \
  file:///home/wroy/.local/share/icons/morewaita/places/scalable/folder-projects.svg
