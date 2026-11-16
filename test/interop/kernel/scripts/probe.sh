#!/bin/bash
# Prints, as "key: value" lines, the two kernels generate.sh records: the Mac
# it runs on and the Linux of Docker's VM, as a container of the image
# generate.sh pins sees it, with the settings the traces depend on. A setting
# the kernel does not have reads "absent", one it refuses to show "unreadable".
set -euo pipefail
dir="$(dirname "$0")"
image="$(sed -n 's/^image="\(.*\)"$/\1/p' "$dir/generate.sh")"
[ -n "$image" ] || { echo "probe.sh: no image in generate.sh" >&2; exit 1; }

echo "macos: $(sw_vers -productVersion) ($(sw_vers -buildVersion))"
for n in kern.ipc.somaxconn net.inet.tcp.keepinit net.inet.tcp.keepidle \
  net.inet.tcp.keepintvl net.inet.tcp.keepcnt net.inet.tcp.msl \
  net.inet.tcp.sendspace net.inet.tcp.recvspace net.inet.udp.recvspace \
  net.inet.udp.maxdgram net.inet.ip.portrange.first net.inet.ip.portrange.last \
  net.local.stream.sendspace net.local.stream.recvspace \
  net.local.dgram.maxdgram net.local.dgram.recvspace; do
  echo "macos.$n: $(sysctl -n "$n" 2>/dev/null || echo unreadable)"
done

echo "docker: $(docker version --format '{{.Server.Version}}')"
echo "linux.image: $image"
docker run --rm --network none "$image" bash -euc '
  echo "linux: $(uname -srvm)"
  for n in net.ipv4.tcp_syn_retries net.ipv4.tcp_syn_linear_timeouts \
    net.ipv4.tcp_synack_retries net.ipv4.tcp_retries2 \
    net.ipv4.tcp_keepalive_time net.ipv4.tcp_keepalive_intvl \
    net.ipv4.tcp_keepalive_probes net.ipv4.tcp_fin_timeout \
    net.ipv4.tcp_rmem net.ipv4.tcp_wmem net.ipv4.ip_local_port_range \
    net.core.somaxconn net.core.rmem_default net.core.wmem_default \
    net.unix.max_dgram_qlen; do
    f="/proc/sys/$(echo "$n" | tr . /)"
    if [ -r "$f" ]; then v="$(tr "\t" " " <"$f")"; else v=absent; fi
    echo "linux.$n: $v"
  done'
