#!/usr/bin/env bash

cd ~/ || exit

[[ ! -d Projects ]] && mkdir Projects

# https://gitlab.gnome.org/GNOME/nautilus/-/issues/1481
#
gio set Projects metadata::custom-icon \
  file:///home/wroy/.local/share/icons/morewaita/places/scalable/folder-projects.svg
