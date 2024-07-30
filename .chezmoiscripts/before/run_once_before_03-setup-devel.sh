#!/usr/bin/env bash

set -x # Print trace of simple commands.

sudo dnf groupinstall -y \
  development-libs

sudo dnf groupinstall -y \
  development-tools

sudo dnf groupinstall -y \
  gnome-software-development

sudo dnf install -y \
  g++

sudo dnf install -y \
  gcc-plugin-devel

sudo dnf install -y \
  clang

sudo dnf install -y \
  clang-tools-extra

sudo dnf install -y \
  python3-pip

pip install \
  git+https://github.com/wroyca/compiledb@support-cl-clang-cl # https://github.com/build2/build2/issues/96

sudo dnf install -y \
  meson

sudo dnf install -y \
  cargo

sudo dnf install -y \
  cmake

sudo dnf install -y \
  npm

sudo npm install \
  -g npm

sudo dnf install -y \
  yarnpkg

sudo dnf install -y \
  go

sudo dnf install -y \
  luarocks

sudo dnf install -y \
  mold

sudo dnf install -y \
  shellcheck

# https://github.com/JohnnyMorganz/StyLua/pull/839
cargo install --git \
  https://github.com/alerque/StyLua --branch space-in-function-defs

sudo dnf install -y \
  neovim

sudo dnf install -y \
  ruby-devel

sudo gem update \
  --system

gem install \
  neovim

pip install \
  pynvim

sudo npm install -g \
  neovim

sudo dnf install -y \
  perl-App-cpanminus

cpanm --sudo \
  Neovim::Ext

if ! command -v b > /dev/null 2>&1; then
  v=$(curl -sSf https://stage.build2.org/0/toolchain.sha256 | sed -n 's,^.*/build2-install-\(.*\)-stage\.sh$,\1,p')
  curl --retry 5 --retry-all-errors -O \
    "https://stage.build2.org/0/$v/build2-install-$v-stage.sh"
  sh build2-install-$v-stage.sh --local --yes /usr
fi

if ! command -v git-credential-manager > /dev/null 2>&1; then
  curl --retry 5 --retry-all-errors -O \
    "https://raw.githubusercontent.com/git-ecosystem/git-credential-manager/main/src/linux/Packaging.Linux/install-from-source.sh"
  sh install-from-source.sh -y --install-prefix=/usr && git-credential-manager configure
fi

# Clean up
#
sudo rm -rf build2-*               || true
sudo rm -rf dotnet-install.sh      || true
sudo rm -rf install-from-source.sh || true
sudo rm -rf git-credential-manager || true
