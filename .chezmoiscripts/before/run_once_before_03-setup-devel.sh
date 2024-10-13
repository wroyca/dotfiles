#!/usr/bin/env bash

sudo dnf groupinstall -y development-libs
sudo dnf groupinstall -y development-tools
sudo dnf groupinstall -y gnome-software-development

sudo dnf install -y g++
sudo dnf install -y gcc-plugin-devel
sudo dnf install -y clang
sudo dnf install -y clang-tools-extra
sudo dnf install -y libcxx
sudo dnf install -y libcxx-devel
sudo dnf install -y python3-pip
sudo dnf install -y meson
sudo dnf install -y cargo
sudo dnf install -y cmake
sudo dnf install -y npm
sudo npm install -g npm
sudo dnf install -y yarnpkg
sudo dnf install -y go
sudo dnf install -y luarocks
sudo dnf install -y mold
sudo dnf install -y lld
sudo dnf install -y shellcheck
sudo dnf install -y msitools
sudo dnf install -y neovim
sudo dnf install -y git-delta
sudo dnf install -y glibc-devel.i686 # gnu/stubs-32.h
sudo dnf install -y --skip-broken \
  mingw*

pip install pynvim
pip install pygments

if ! command -v b > /dev/null 2>&1; then
 v=$(curl -sSf https://stage.build2.org/0/toolchain.sha256 | sed -n 's,^.*/build2-install-\(.*\)-stage\.sh$,\1,p')
  curl --connect-timeout 10 --retry 5 --retry-all-errors -O \
    "https://stage.build2.org/0/$v/build2-install-$v-stage.sh"
  sh build2-install-$v-stage.sh --local --yes /usr
fi

if ! command -v git-credential-manager > /dev/null 2>&1; then
  curl --connect-timeout 10 --retry 5 --retry-all-errors -O \
    "https://raw.githubusercontent.com/git-ecosystem/git-credential-manager/main/src/linux/Packaging.Linux/install-from-source.sh"
  sh install-from-source.sh -y --install-prefix=/usr && git-credential-manager configure
fi

sudo rm -rf build2-*               || true
sudo rm -rf dotnet-install.sh      || true
sudo rm -rf install-from-source.sh || true
sudo rm -rf git-credential-manager || true
