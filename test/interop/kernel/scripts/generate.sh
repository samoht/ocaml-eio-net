#!/bin/bash
# Records the kernel's linux and macos columns through eio_posix: macOS
# natively, so this runs on a Mac, and Linux in Docker, in the image pinned
# below. The column Windows recorded, generate-windows.sh, is kept from the
# traces before.
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
# The image's own opam-repository predates Eio 1.7.
repo="https://github.com/ocaml/opam-repository/archive/1c93acbc4176695001cc3356717dcfe90f89ce44.tar.gz"
transports="tcp4 tcp6 unix-stream udp4 udp6 unix-dgram"
traces="$transports scenarios"

# Unix socket paths must stay short.
work="$(mktemp -d /tmp/kernel-traces.XXXXXX)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/sock"

# macOS's loopback delivers a TCP segment asynchronously: each operation
# that can send one waits for the peer's answer, a reset, before the next.
# A loaded Mac takes more than 2 s for the longer datagram scenarios.
for t in $traces; do
  case $t in tcp4 | tcp6) settle=0.02 ;; *) settle=0 ;; esac
  KERNEL_SETTLE=$settle KERNEL_TIMEOUT=10 KERNEL_DIR="$work/sock" \
    "$record" "$t" >"$work/$t.macos"
done

docker run --rm -v "$src:/src:ro" -v "$work:/out" "$image" bash -euc "
    mkdir ~/src && tar -C /src --exclude=_build --exclude=.git -cf - . | tar -C ~/src -xf -
    cd ~/src
    mkdir ~/repo && curl -fsSL $repo | tar -C ~/repo --strip-components 1 -xzf -
    opam repo set-url default ~/repo >/dev/null
    opam install -y eio.1.7 eio_posix.1.7 hermetic cstruct fmt mtime ipaddr >/dev/null
    opam exec -- dune build ./test/interop/kernel/scripts/record.exe
    cp ./_build/default/test/interop/kernel/scripts/record.exe /out/record.linux"

# A getnameinfo that asks Docker's name server takes more than 2 s.
docker run --rm \
  --sysctl net.ipv6.conf.all.disable_ipv6=0 \
  --sysctl net.ipv6.conf.lo.disable_ipv6=0 \
  -v "$work:/out" "$image" bash -euc "
    mkdir -p /tmp/sock
    for t in $traces; do
      KERNEL_TIMEOUT=10 KERNEL_DIR=/tmp/sock /out/record.linux \$t >/out/\$t.linux
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
  "$record" merge "$t" "$src/test/interop/kernel/traces/$t.csv" \
    linux="$work/$t.linux" macos="$work/$t.macos" >"$out/$t.csv"
done
