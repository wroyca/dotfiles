#! /usr/bin/env bash

sudo dnf upgrade -y \
  --refresh

sudo dnf install -y \
  https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

sudo dnf install -y \
  https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

sudo dnf install -y \
  akmod-nvidia

sudo dnf install -y \
  xorg-x11-drv-nvidia-cuda

sudo dnf install -y \
  gstreamer1-plugins-{bad-\*,good-\*,base} 

sudo dnf install -y \
  gstreamer1-plugin-openh264 

sudo dnf install -y \
  gstreamer1-libav --exclude=gstreamer1-plugins-bad-free-devel

sudo dnf install -y \
  lame\* --exclude=lame-devel

sudo dnf group upgrade -y \
  Multimedia --with-optional --allowerasing

