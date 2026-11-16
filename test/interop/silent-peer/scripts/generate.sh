#!/bin/bash
# Times TCP connections over loopback whose peer drops every segment, until a
# byte sent goes unacknowledged long enough to fail them: macOS natively, so
# this runs on a Mac, and Linux in Docker, in the image the kernel traces
# pin, with its default settings. Writes OUT_DIR/silent.csv, one row per
# connection.
#   generate.sh OUT_DIR MEASURE_EXE
set -euo pipefail
[ "$(uname -s)" = Darwin ] || {
  echo "generate.sh measures macOS natively: run it on macOS" >&2
  exit 1
}
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$1" && pwd)"
measure="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
pin="$(dirname "$0")/../../kernel/scripts/generate.sh"
image="$(sed -n 's/^image="\(.*\)"$/\1/p' "$pin")"
[ -n "$image" ] || { echo "generate.sh: no image in $pin" >&2; exit 1; }
runs=5

work="$(mktemp -d /tmp/silent-peer.XXXXXX)"
trap 'rm -rf "$work"' EXIT

"$measure" $runs | sed 's/^/macos,/' >"$work/macos"

docker run --rm -v "$src:/src:ro" -v "$work:/out" "$image" bash -euc "
    mkdir ~/src && tar -C /src --exclude=_build --exclude=.git -cf - . | tar -C ~/src -xf -
    cd ~/src
    opam update -y >/dev/null
    opam install -y . --deps-only >/dev/null
    opam exec -- dune build ./test/interop/silent-peer/scripts/measure.exe
    cp ./_build/default/test/interop/silent-peer/scripts/measure.exe /out/measure.linux"

# Root, for the CAP_NET_RAW that a source route needs; Docker grants it.
docker run --rm --network none --user root -v "$work:/out" "$image" bash -euc "
    /out/measure.linux $runs | sed 's/^/linux,/' >/out/linux"

{
  echo "kernel,case,seconds,first,read,write,read"
  cat "$work/linux" "$work/macos"
} >"$out/silent.csv"
