#!/usr/bin/env bash

set -x # Print trace of simple commands.
set +e # Ignore pipeline that returns a non-zero status.

if command -v emacs > /dev/null 2>&1; then
  exit 0
fi

owd="$(pwd)"
ret ()
{
  cd "$owd"
}

trap ret EXIT

mkdir -p /tmp/emacs && cd $_

# https://dnf-plugins-core.readthedocs.io/en/latest/builddep.html
#
sudo dnf builddep -y \
  emacs

git init .
git remote add --no-fetch origin https://github.com/emacs-mirror/emacs.git --no-tags
git config remote.origin.fetch "+refs/heads/master:refs/remotes/origin/master"
git fetch -v --depth=1
git pull origin master

CC='CFLAGS=-DMAIL_USE_LOCKF -O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Werror=format-security -Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1  -m64  -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer LDFLAGS=-Wl,-z,relro gcc'
PKG_CONFIG_PATH=:/usr/lib64/pkgconfig:/usr/share/pkgconfig

make distclean
make uninstall

./autogen.sh
./configure                           \
  --prefix=$HOME/.local               \
  --without-pop                       \
  --with-mailutils                    \
  --with-pgtk                         \
  --without-x                         \
  --with-xwidgets                     \
  --with-cairo                        \
  --without-compress-install          \
  --with-native-compilation           \
  --with-json                         \
  --with-dbus                         \
  --with-gif                          \
  --with-jpeg                         \
  --with-png                          \
  --with-rsvg                         \
  --with-tiff                         \
  --with-webp                         \
  --with-gpm                          \
  --with-modules                      \
  --with-harfbuzz                     \
  build_alias=x86_64-redhat-linux-gnu \
  host_alias=x86_64-redhat-linux-gnu

make -j$((`nproc`+2))
make install

git clone --depth 1 https://github.com/doomemacs/doomemacs \
  ~/.config/emacs && ~/.config/emacs/bin/doom install
