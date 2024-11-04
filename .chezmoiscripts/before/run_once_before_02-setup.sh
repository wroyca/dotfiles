#!/usr/bin/env bash

sudo dnf upgrade -y
sudo dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm
sudo dnf install -y https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-"$(rpm -E %fedora)".noarch.rpm
sudo dnf install -y akmod-nvidia
sudo dnf install -y xorg-x11-drv-nvidia-cuda

# https://docs.fedoraproject.org/en-US/quick-docs/installing-plugins-for-playing-movies-and-music/
#
sudo dnf group install -y multimedia
sudo dnf install -y intel-media-driver
sudo dnf swap -y ffmpeg-free ffmpeg --allowerasing

# Fedora 41 uses fractional scaling by default, but we prefer to maintain
# integer scaling. Undo this change by resetting the experimental features in
# Mutter.
#
gsettings set org.gnome.mutter experimental-features "[]"
