#!/bin/bash

echo "This script is tailored to my specific needs and preferences, which may not be suitable for your use case."
read -p "Do you want to proceed? (y/n) " choice
case "$choice" in
  y|Y ) echo "Installing applications...";;
  n|N ) echo "Aborting installation..."; exit;;
    * ) echo "Invalid choice. Aborting installation..."; exit;;
esac

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

# Fedora offers a collection of groups that include almost all of the necessary
# packages for GNOME software development.
#
# The development-libs group includes libraries necessary for software
# development.
#
# The development-tools group includes the GNU Compiler
# Collection (GCC) and other tools necessary for software development.
#
# The gnome-software-development group includes the GTK+ and GNOME developer
# tools.
#
# In addition to these groups, the libadwaita-devel, gtksourceview5-devel,
# g++, clang, and clang-tools-extra packages are also installed to provide 
# additional development libraries and tools.
#
sudo yum groupinstall development-libs development-tools gnome-software-development -y
sudo dnf install -y libadwaita-devel
sudo dnf install -y gtksourceview5-devel
sudo dnf install -y g++
sudo dnf install -y clang
sudo dnf install -y clang-tools-extra

# Install Git Credential Manager for secure authentication to GitHub
# 
# Git Credential Manager (GCM) is a tool that allows us to securely store our
# credentials and connect to GitHub over HTTPS. With GCM, we don't need to 
# manually create and store a personal access token, as it handles 
# authentication for us, including two-factor authentication (2FA). 
# 
# The dotnet-sdk-6.0 package and the git-credential-manager tool are installed
# to provide support for GCM.
#
sudo dnf --enablerepo=updates-testing install -y dotnet-sdk-6.0-6.0.114-1.fc37.x86_64
dotnet tool install -g git-credential-manager

# Install Flathub, which is the best way to get Flatpak apps
# 
# Flathub aims to be the place to get and distribute apps for Linux. It is powered by
# Flatpak which allows Flathub apps to run on almost any Linux distribution.
# 
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

# Install Extension Manager utility for GNOME Shell
# 
# This script installs Extension Manager, a utility for searching, browsing, and 
# installing extensions for the GNOME Shell desktop environment.
#
flatpak install -y com.mattjakeman.ExtensionManager

# Install applications that I frequently use from Flathub
# 
# This script installs several applications that I frequently use from the Flathub 
# platform. The installed applications include Discord, Slack, Riot, KeePassXC, 
# and Builder.
#
# NOTE: XWayland is broken with Electron. I recommand PWAS instead.
# https://addons.mozilla.org/en-CA/firefox/addon/pwas-for-firefox/
#
# flatpak install -y com.discordapp.Discord
# flatpak install -y com.slack.Slack
# flatpak install -y im.riot.Riot
#
flatpak install -y org.keepassxc.KeePassXC
flatpak install -y org.gnome.Builder

# Let us all bow down and worship this god, who makes coding a pleasure and not a mere slog.
# For Neovim doth rule with an iron fist, and woe betide any who would choose another text editor to persist.
#
sudo dnf install neovim

# Install and use GNU Stow as a symlink farm manager
# 
# GNU Stow is a symlink farm manager that allows us to manage collections of 
# software and/or data packages that are stored in separate directories on the 
# filesystem. This script installs Stow and uses it to create symbolic links to 
# the package directories in the home directory.
# 
sudo dnf install -y stow && stow --target=$HOME */

