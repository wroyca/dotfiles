#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

sudo dnf install -y wine winetricks

# Wine DPI settings for display scaling.
#
# To configure Wine for integer display scaling (e.g., 100%, 200%, etc.), the
# LogPixels registry value under HKEY_CURRENT_USER\Control Panel\Desktop must be
# set to an appropriate DPI value. Windows uses 96 DPI as the baseline for 100%
# scaling. Therefore:
#
# 100% scaling → LogPixels = 96 (0x60)
# 200% scaling → LogPixels = 192 (0xC0)
# 300% scaling → LogPixels = 288 (0x120)
#
wine reg add "HKEY_CURRENT_USER\Control Panel\Desktop" /v LogPixels /t REG_DWORD /d 0xC0 /f
