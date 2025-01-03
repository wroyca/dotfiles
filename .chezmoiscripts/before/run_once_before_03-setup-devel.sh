#!/usr/bin/env bash

sudo dnf install -y g++
sudo dnf install -y gcc-plugin-devel
sudo dnf install -y clang
sudo dnf install -y clang-tools-extra
sudo dnf install -y libcxx
sudo dnf install -y libcxx-devel
sudo dnf install -y mingw32-gcc-c++
sudo dnf install -y mingw64-gcc-c++

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
