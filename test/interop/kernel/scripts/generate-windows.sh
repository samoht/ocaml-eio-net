#!/bin/bash
# Records the kernel's windows column through eio_windows, on Windows with
# Git Bash, as the kernel-windows workflow does on GitHub's windows-latest.
# The columns macOS and Linux recorded, generate.sh, are kept from the
# traces before. probe-windows.sh prints where it ran.
#   generate-windows.sh OUT_DIR RECORD_EXE
set -euo pipefail
case "$(uname -s)" in
MINGW* | MSYS* | CYGWIN*) ;;
*)
  echo "generate-windows.sh records Windows: run it on Windows" >&2
  exit 1
  ;;
esac
out="$(cd "$1" && pwd)"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
transports="tcp4 tcp6 unix-stream udp4 udp6 unix-dgram"
traces="$transports scenarios"

# Unix socket paths must stay short, and the recorder is a Windows program:
# it takes Windows paths.
work="$(mktemp -d)"
trap 'rm -rf "$work" || true' EXIT
mkdir "$work/sock"
w() { cygpath -w "$1"; }

# As on macOS, each operation that can send a TCP segment waits for the
# peer's answer before the next. eio_windows never completes a refused TCP
# connect, so a TCP operation has 5 s before it answers "timeout". The
# runner has other interfaces and its ephemeral range cannot be narrowed
# below 255 ports: the scenarios off loopback or out of ports are not
# recorded.
for t in $traces; do
  case $t in
  tcp4 | tcp6) settle=0.02 timeout=5 ;;
  *) settle=0 timeout=10 ;;
  esac
  start=$SECONDS
  KERNEL_SETTLE=$settle KERNEL_TIMEOUT=$timeout KERNEL_DIR="$(w "$work/sock")" \
    "$record" "$t" >"$work/$t.windows"
  echo "$t: $((SECONDS - start)) s" >&2
done

for t in $traces; do
  "$record" merge "$t" "$(w "$src/test/interop/kernel/traces/$t.csv")" \
    "windows eio_windows=$(w "$work/$t.windows")" >"$out/$t.csv"
done
