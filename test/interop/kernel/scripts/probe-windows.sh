#!/bin/bash
# Prints, as "key: value" lines, where generate-windows.sh records: the
# eio_windows of the switch, the Windows it runs on and the TCP settings the
# traces depend on, "absent" for one at its default; and, kept from the
# provenance before, where the macOS and Linux columns were recorded.
set -euo pipefail
dir="$(dirname "$0")"
switch="${OPAM_SWITCH_PREFIX:?run in an opam switch}"
switch="$(cygpath -u "$switch")"
pkg=("$switch"/.opam-switch/packages/eio_windows.*)
[ -d "${pkg[0]}" ] || { echo "eio_windows is not installed in $switch" >&2; exit 1; }
echo "eio_windows: ${pkg[0]##*/eio_windows.}"

ps() { MSYS_NO_PATHCONV=1 powershell.exe -NoProfile -NonInteractive -Command "$1" | tr -d '\r'; }
os() { ps "(Get-CimInstance Win32_OperatingSystem).$1"; }
echo "windows: $(os Caption) $(os Version)"
echo "windows.image: ${ImageOS:-unknown} ${ImageVersion:-}"
tcpip='HKLM:\SYSTEM\CurrentControlSet\Services\Tcpip\Parameters'
for n in TcpTimedWaitDelay MaxUserPort TcpMaxDataRetransmissions \
  TcpMaxConnectRetransmissions KeepAliveTime KeepAliveInterval; do
  v="$(ps "(Get-ItemProperty -Path $tcpip -ErrorAction SilentlyContinue).$n")"
  echo "windows.tcpip.$n: ${v:-absent}"
done
for p in tcp udp; do
  netsh.exe int ipv4 show dynamicport "$p" | tr -d '\r' |
    awk -v p="$p" -F': *' '/Start Port/ { s = $2 } /Number of Ports/ { n = $2 }
      END { print "windows.dynamicport." p ": " s " " n }'
done
bash "$dir/carry.sh" "${DUNE_SOURCEROOT:?run through dune}/test/interop/kernel/traces/provenance" \
  posix Darwin "macos[.:]|linux[.:]|docker: "
