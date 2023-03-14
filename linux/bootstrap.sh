#!/bin/bash

# Upgrade the system to the latest release of Fedora
#
# The dnf upgrade command updates all packages on the system to the latest
# version available in the repositories.
#
sudo dnf upgrade -y

# Install RPM Fusion repositories
#
# These community-maintained repositories offer a range of packages that are not available
# in the official Fedora repositories.
#
# The $(rpm -E %fedora) variable expands to the current Fedora release number,
# which is included in the URL of the repository to ensure that the correct repository
# is installed.
#
sudo dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
sudo dnf install -y https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm

# Install NVIDIA graphics drivers
#
# The akmod-nvidia package includes pre-built kernel modules for the NVIDIA
# graphics drivers that are automatically built when a new kernel is installed.
#
# The xorg-x11-drv-nvidia-cuda package provides the NVIDIA X.Org X11 video
# driver with CUDA support.
#
sudo dnf install -y akmod-nvidia
sudo dnf install -y xorg-x11-drv-nvidia-cuda

# Install GStreamer plugins for audio and video support
#
# GStreamer is a pipeline-based multimedia framework that links together a wide
# variety of media processing systems to complete complex workflows
#
# The curly brace expansion syntax allows us to specify multiple package names
# with a common pattern, using the * character to match any characters.
# The --exclude flag specifies a package that should be excluded from the
# installation.
#
sudo dnf install -y gstreamer1-plugins-{bad-\*,good-\*,base} --exclude=gstreamer1-plugins-bad-free-devel
sudo dnf install -y gstreamer1-plugin-openh264
sudo dnf install -y gstreamer1-libav
sudo dnf install -y lame\* --exclude=lame-devel
sudo dnf group upgrade -y --with-optional Multimedia
