#! /usr/bin/env bash

# Fractional scaling in GNOME is considered an experimental feature – one which
# is now available by default on Fedora 40, but it's not "quite"
# ready so disable it for now.
#
# See also:
# https://pagure.io/fedora-workstation/issue/357
#
gsettings set org.gnome.mutter experimental-features "[]"
