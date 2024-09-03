#!/usr/bin/env bash

cd ~/

[[ ! -d Projects ]] && mkdir Projects

gio set Projects metadata::custom-icon \
  file:///home/wroy/.local/share/icons/morewaita/places/scalable/folder-projects.svg
