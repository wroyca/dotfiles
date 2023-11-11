#! /usr/bin/env bash

flatpak_pkgs=(
  "org.gnome.World.Secrets"
  "org.gnome.World.PikaBackup"
  "com.mattjakeman.ExtensionManager"
)

for pkgs in "${flatpak_pkgs[@]}"; do
  flatpak install -y flathub "$pkgs"
done

sudo dnf install -y \
  adw-gtk3-theme

sudo dnf install -y \
  gnome-tweaks

sudo dnf groupinstall -y \
  development-libs
 
sudo dnf groupinstall -y \
  development-tools

sudo dnf groupinstall -y \
  gnome-software-development

sudo dnf install -y \
  kitty

sudo dnf install -y \
  neovim

sudo dnf install -y \
  make

sudo dnf install -y \
  cmake

sudo dnf install -y \
  gcc

sudo dnf install -y \
  clang

sudo dnf install -y \
  clang-tools-extra

# build2 | C/C++ Build Toolchain
#
if ! command -v b > /dev/null 2>&1; then
  cd ~
  mkdir Projects && cd Projects

  # Parse href to obtain the most recent version.
  #
  v=$(curl -s  'https://stage.build2.org/0/' | \
      grep -oP '(?<=href=")0\.\K\d+\.\d+'    | \
      sort -nr                               | \
      head -1)
  
  # Download and install build2 toolchain with debug symbols.
  #
  curl -O \
    "https://stage.build2.org/0/0.$v-a.0/build2-install-0.$v-a.0-stage.sh"
  sh build2-install-0.$v-a.0-stage.sh --local --debug
fi

# Download and install rust toolchain.
# 
if ! command -v cargo > /dev/null 2>&1; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -- -y
fi
