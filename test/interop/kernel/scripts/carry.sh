#!/bin/bash
# Prints the lines of the provenance OLD that name the columns another host
# recorded, which a regeneration on this host keeps: HOST's own lines, those
# matching KEYS, and, when OLD was written there, its revision, date and
# inputs as HOST.revision, HOST.recorded and HOST.input.
#   carry.sh OLD HOST HOST_UNAME KEYS
set -euo pipefail
old=$1 host=$2 uname=$3 keys=$4
[ -f "$old" ] || exit 0
if grep -Eq "^host: ($uname)" "$old"; then
  sed -n -e "s/^revision: /$host.revision: /p" -e "s/^recorded: /$host.recorded: /p" \
    -e "s/^input: /$host.input: /p" "$old"
else
  grep -E "^$host\.(revision|recorded|input): " "$old" || true
fi
grep -E "^($keys)" "$old" | grep -Ev "^$host\.(revision|recorded|input): " || true
