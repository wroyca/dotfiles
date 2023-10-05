#! /usr/bin/env bash

flatpak_pkgs=(
  "com.slack.Slack"
  "com.discordapp.Discord"
  "org.keepassxc.KeePassXC"
  "com.github.tchx84.Flatseal"
  "com.mattjakeman.ExtensionManager"
)

for pkgs in "${flatpak_pkgs[@]}"; do
  flatpak install -y flathub "$pkgs"
done

sudo dnf groupinstall -y \
  development-libs
 
sudo dnf groupinstall -y \
  development-tools
 
sudo dnf groupinstall -y \
  gnome-software-development

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
  # Interpret versioning in order to obtain the most recent version.
  v=$(curl -s  'https://stage.build2.org/0/' | \
      grep -oP '(?<=href=")0\.\K\d+\.\d+'    | \
      sort -nr                               | \
      head -1)
  
  # Download on local machine since piping doesn't appear to function here.
  # (What might be the reason?)
  curl -O \
    "https://stage.build2.org/0/0.$v-a.0/build2-install-0.$v-a.0-stage.sh"
  
  # Install the toolchain system-wide.
  #
  sh build2-install-0.$v-a.0-stage.sh
  rm --recursive --force build2-*
fi

# cargo | Package manager and crate host for rust.
# 
if ! command -v cargo > /dev/null 2>&1; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi
