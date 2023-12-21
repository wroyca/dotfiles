#! /usr/bin/env bash

# Permanently enables bluetooth Fast Connectable setting for adapters that
# support it, along with ReconnectAttempts and ReconnectIntervals.
#
# See also:
# https://wiki.archlinux.org/title/bluetooth
#
sudo sed -i \
  's/#FastConnectable = false/FastConnectable = true/' /etc/bluetooth/main.conf

sudo sed -i \
  's/#ReconnectAttempts/ReconnectAttempts/' /etc/bluetooth/main.conf

sudo sed -i \
  's/#ReconnectIntervals/ReconnectIntervals/' /etc/bluetooth/main.conf

# XWayland manifests incorrect frame rendering with NVIDIA hardware. For
# example, in GNOME and Plasma, this results in motion duplication and/or
# high-frequency stuttering. Meanwhile, in wlroots and Sway, it causes
# partially rendered frames with transparent segments. For now, a suggested
# workaround involves disabling glamor support.
#
# See also:
# https://gitlab.freedesktop.org/xorg/xserver/-/issues/1317
#
if ! grep -q "XWAYLAND_NO_GLAMOR=1" /etc/environment; then
  echo "XWAYLAND_NO_GLAMOR=1" | sudo tee -a /etc/environment > /dev/null
fi

# Force Qt to request wayland plugin by default.
#
if ! grep -q "QT_QPA_PLATFORM=wayland" /etc/environment; then
  echo "QT_QPA_PLATFORM=wayland" | sudo tee -a /etc/environment > /dev/null
fi
