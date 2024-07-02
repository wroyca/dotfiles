#!/usr/bin/env bash

set -x # Print trace of simple commands.

sudo dnf remove -y \
  gnome-terminal

sudo dnf install -y \
  kitty

sudo dnf install -y \
  fish

sudo dnf install -y \
  scrcpy

sudo dnf install -y \
  wine

sudo dnf install -y \
  winetricks

sudo dnf install -y \
  adw-gtk3-theme

sudo dnf install -y \
  firefoxpwa
