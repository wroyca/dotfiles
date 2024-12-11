#!/usr/bin/env bash

cd ~/ || exit

[[ ! -d Projects ]] && mkdir Projects
[[ ! -d Games ]]    && mkdir Games

# https://gitlab.gnome.org/GNOME/nautilus/-/issues/1481
#
gio set Projects metadata::custom-icon file:///home/wroy/.local/share/icons/morewaita/scalable/places/folder-projects.svg
gio set Games    metadata::custom-icon file:///home/wroy/.local/share/icons/morewaita/scalable/places/folder-games.svg
