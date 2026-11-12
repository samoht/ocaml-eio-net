#!/bin/bash
# Records the kernel's traces through eio_posix: macOS natively, so this runs
# on a Mac, and Linux in Docker, in the image pinned below.
#   generate.sh OUT_DIR RECORD_EXE
set -euo pipefail
[ "$(uname -s)" = Darwin ] || {
  echo "generate.sh records macOS natively: run it on macOS" >&2
  exit 1
}
command -v docker >/dev/null || { echo "docker not on PATH" >&2; exit 1; }
out="$(cd "$1" && pwd)"
record="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"
src="${DUNE_SOURCEROOT:?run through dune: REGEN=1 dune build @regen-traces}"
image="ocaml/opam@sha256:d161f099e5d053fe198accc09fda214c6275289570847fd906f9c65f862bf460"
transports="tcp4 tcp6 unix-stream udp4 udp6 unix-dgram"
traces="$transports scenarios"

# Unix socket paths must stay short.
work="$(mktemp -d /tmp/kernel-traces.XXXXXX)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/sock"

for t in $traces; do
  KERNEL_DIR="$work/sock" "$record" "$t" >"$work/$t.macos"
done

docker run --rm -v "$src:/src:ro" -v "$work:/out" "$image" bash -euc "
    mkdir ~/src && tar -C /src --exclude=_build --exclude=.git -cf - . | tar -C ~/src -xf -
    cd ~/src
    opam update -y >/dev/null
    opam install -y . --deps-only >/dev/null
    opam install -y eio_posix >/dev/null
    opam exec -- dune build ./test/interop/kernel/scripts/record.exe
    cp ./_build/default/test/interop/kernel/scripts/record.exe /out/record.linux"

docker run --rm \
  --sysctl net.ipv6.conf.all.disable_ipv6=0 \
  --sysctl net.ipv6.conf.lo.disable_ipv6=0 \
  -v "$work:/out" "$image" bash -euc "
    mkdir -p /tmp/sock
    for t in $traces; do
      KERNEL_DIR=/tmp/sock /out/record.linux \$t >/out/\$t.linux
    done"

# The scenarios off the loopback interface, with no network, so that the
# host has loopback only and nothing leaves it.
docker run --rm --network none \
  --sysctl net.ipv6.conf.all.disable_ipv6=0 \
  --sysctl net.ipv6.conf.lo.disable_ipv6=0 \
  -v "$work:/out" "$image" bash -euc "
    mkdir -p /tmp/sock
    KERNEL_LOOPBACK_ONLY=1 KERNEL_DIR=/tmp/sock /out/record.linux scenarios >/out/lo.linux"
# The scenarios that run out of ephemeral ports, with a range of 20 ports,
# so that they fill it in a few connects and leave the host's alone.
docker run --rm --network none \
  --sysctl net.ipv6.conf.all.disable_ipv6=0 \
  --sysctl net.ipv6.conf.lo.disable_ipv6=0 \
  --sysctl net.ipv4.ip_local_port_range="40000 40019" \
  -v "$work:/out" "$image" bash -euc "
    mkdir -p /tmp/sock
    KERNEL_NARROW_PORTS=1 KERNEL_DIR=/tmp/sock /out/record.linux scenarios >/out/ports.linux"
"$record" overlay "$work/scenarios.linux" "$work/lo.linux" >"$work/both.linux"
"$record" overlay "$work/both.linux" "$work/ports.linux" >"$work/scenarios.linux"

for t in $traces; do
  "$record" merge "$work/$t.linux" "$work/$t.macos" >"$out/$t.csv"
done
