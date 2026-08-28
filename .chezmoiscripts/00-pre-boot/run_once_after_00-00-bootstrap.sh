#!/usr/bin/env bash

set -x # Print commands and their arguments as they are executed.
set -e # Exit immediately if a command exits with a non-zero status.

marker="${HOME}/.no-reboot"
retry_interval_seconds="${REBOOT_RETRY_INTERVAL_SECONDS:-30}"

if [ -f "${marker}" ]; then
  exit 0
fi

while true; do
  if output="$(systemctl reboot --check-inhibitors=yes --no-ask-password --no-wall 2>&1)"; then
    touch "${marker}"
    exit 0
  fi

  printf '%s\n' "${output}" >&2

  case "${output}" in
    *inhibit* | *Inhibit* | *inhibitor* | *Inhibitor*)
      sleep "${retry_interval_seconds}"
      ;;
    *)
      exit 1
      ;;
  esac
done
