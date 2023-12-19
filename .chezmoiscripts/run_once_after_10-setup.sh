#! /usr/bin/env bash

# There have been issues lately with Fedora "default" mirror, so let's adjust
# dnf's minimum rate appropriately.
#
if ! grep -q "fastestmirror=1" /etc/dnf/dnf.conf; then
  echo "fastestmirror=1" |\
    sudo tee -a /etc/dnf/dnf.conf > /dev/null

  echo "minrate=30k" |\
    sudo tee -a /etc/dnf/dnf.conf > /dev/null

  echo "max_parallel_downloads=10" |\
    sudo tee -a /etc/dnf/dnf.conf > /dev/null
fi

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
