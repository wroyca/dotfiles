#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

# As of Fedora 42, fractional scaling is enabled by default. Disabling it
# explicitly is necessary to restore the optimal performance path.
#
# That is, GNOME on Wayland supports only discrete integer scaling factors
# (e.g., 1×, 2×). Applications render their surfaces at the selected scale
# factor, and the Wayland compositor presents the output directly without
# interpolation. This approach is computationally efficient and preserves pixel
# integrity.
#
# When GNOME's experimental fractional scaling feature is enabled, this behavior
# changes significantly. Regardless of the configured scaling factor (e.g.,
# 125%, 150%, 175%), GNOME internally instructs all clients to render at 2×
# scale. The compositor then performs a post-process downscaling operation to
# approximate the requested fractional scale.
#
# The problem is that this additional compositing path remains active even when
# the effective scale factor is an integer (e.g., 100% or 200%). In such cases,
# enabling fractional scaling imposes unnecessary non-trivial overhead.
#
gsettings set org.gnome.mutter experimental-features "[]"
