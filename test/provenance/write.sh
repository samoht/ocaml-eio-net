#!/bin/bash
# Runs COMMAND, which records the traces in the current directory, the build
# copy of ORACLE/traces, then writes ./provenance for them:
#   write.sh ORACLE PROBE INPUT... -- TRACE... -- COMMAND...
# ORACLE is the oracle directory from the project root; PROBE, a script that
# prints "key: value" lines naming the oracle and the kernel it ran on, and
# each INPUT, a file that decides the traces, are relative to ORACLE; TRACE
# is a file recorded, provenance itself being skipped. The revision and the
# inputs are read before COMMAND runs: a recording can take an hour, and a
# commit made meanwhile did not record the traces.
set -euo pipefail
oracle=$1 probe=$2
shift 2
inputs=()
while [ "$1" != -- ]; do
  inputs+=("$1")
  shift
done
shift
traces=()
while [ "$1" != -- ]; do
  traces+=("$1")
  shift
done
shift
src="${DUNE_SOURCEROOT:?run through dune}"

md5() {
  if command -v md5sum >/dev/null; then
    md5sum "$1" | cut -d' ' -f1
  else
    md5 -q "$1"
  fi
}

revision="$(git -C "$src" rev-parse HEAD)"
paths=()
for f in "${inputs[@]}"; do paths+=("$oracle/$f"); done
if [ -n "$(git -C "$src" status --porcelain -- "${paths[@]}")" ]; then
  revision="$revision, with uncommitted changes to its inputs"
fi
pins=()
for f in "${inputs[@]}"; do pins+=("input: $f $(md5 "../$f")"); done

"$@"

{
  echo "# Where the traces beside this file come from. write.sh rewrites it on"
  echo "# REGEN=1; test/provenance checks it."
  echo "generator: $oracle/scripts/generate.sh"
  echo "regenerate: REGEN=1 dune build @$oracle/regen-traces"
  echo "revision: $revision"
  echo "recorded: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "uid: $(id -u)"
  echo "host: $(uname -srm)"
  bash "../$probe"
  printf '%s\n' "${pins[@]}"
  for f in "${traces[@]}"; do
    f="${f#./}"
    [ "$f" = provenance ] || echo "trace: traces/$f $(md5 "$f")"
  done
} >provenance
