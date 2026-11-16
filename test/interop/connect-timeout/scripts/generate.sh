#!/bin/bash
# Times TCP connects whose SYNs are all dropped, as eio_posix, eio_linux and a
# plain Unix.connect see them: macOS natively, so this runs on a Mac, and
# Linux in Docker, in the image the kernel traces pin, with its default
# settings. Writes OUT_DIR/connect.csv, one row per connect.
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
runs=10

work="$(mktemp -d /tmp/connect-timeout.XXXXXX)"
trap 'rm -rf "$work"' EXIT

# macOS: lo0 holds 127.0.0.1 only, so a SYN to 127.0.0.2 loops back to no
# address and is dropped.
"$measure" unix $runs 127.0.0.2:9 | sed 's/^/macos,unix,/' >"$work/macos.unix" &
EIO_BACKEND=posix "$measure" eio $runs 127.0.0.2:9 |
  sed 's/^/macos,eio_posix,/' >"$work/macos.eio_posix" &
wait

docker run --rm -v "$src:/src:ro" -v "$work:/out" "$image" bash -euc "
    mkdir ~/src && tar -C /src --exclude=_build --exclude=.git -cf - . | tar -C ~/src -xf -
    cd ~/src
    opam update -y >/dev/null
    opam install -y . --deps-only >/dev/null
    opam install -y eio_main >/dev/null
    opam exec -- dune build ./test/interop/connect-timeout/scripts/measure.exe
    cp ./_build/default/test/interop/connect-timeout/scripts/measure.exe /out/measure.linux"

# Linux: a SYN to a listener whose accept queue is full is dropped. Docker's
# default seccomp profile refuses io_uring, which eio_linux needs.
docker run --rm --network none --security-opt seccomp=unconfined \
  -v "$work:/out" "$image" bash -euc "
    /out/measure.linux unix $runs full | sed 's/^/linux,unix,/' >/out/linux.unix &
    for b in posix linux; do
      EIO_BACKEND=\$b /out/measure.linux eio $runs full |
        sed \"s/^/linux,eio_\$b,/\" >/out/linux.eio_\$b &
    done
    wait"

cat "$work"/linux.unix "$work"/linux.eio_posix "$work"/linux.eio_linux \
  "$work"/macos.unix "$work"/macos.eio_posix >"$work/rows"
if grep -v ',ETIMEDOUT$' "$work/rows" >&2; then
  echo "generate.sh: a connect did not time out" >&2
  exit 1
fi
{
  echo "kernel,client,seconds,outcome"
  cat "$work/rows"
} >"$out/connect.csv"
