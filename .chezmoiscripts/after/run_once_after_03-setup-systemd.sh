#!/usr/bin/env bash

set +e # Ignore pipeline that returns a non-zero status.

systemctl --user enable kitty.service
systemctl --user start  kitty.service
