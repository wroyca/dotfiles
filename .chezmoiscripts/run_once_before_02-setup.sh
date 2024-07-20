#!/usr/bin/env bash

set -x # Print trace of simple commands.

sudo dnf upgrade -y \
  --refresh

sudo dnf install -y \
  https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm

sudo dnf install -y \
  https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-"$(rpm -E %fedora)".noarch.rpm

sudo dnf install -y \
  akmod-nvidia

sudo dnf install -y \
  xorg-x11-drv-nvidia-cuda

sudo dnf install -y \
  intel-media-driver

sudo dnf install -y \
  libva-nvidia-driver

sudo dnf group install -y \
  multimedia
