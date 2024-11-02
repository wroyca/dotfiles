#!/usr/bin/env bash

sudo dnf upgrade -y
sudo dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm
sudo dnf install -y https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-"$(rpm -E %fedora)".noarch.rpm
sudo dnf install -y akmod-nvidia
sudo dnf install -y xorg-x11-drv-nvidia-cuda
sudo dnf install -y intel-media-driver
sudo dnf install -y libva-nvidia-driver

# https://github.com/rpm-software-management/dnf5/issues/138
#
sudo dnf group install -y \
  multimedia

# Fedora 41 uses fractional scaling by default, but we prefer to maintain
# integer scaling. Undo this change by resetting the experimental features in
# Mutter.
#
gsettings set org.gnome.mutter experimental-features "[]"
