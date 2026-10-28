#!/bin/bash
set -euo pipefail
oracle_script_dir="$(dirname "$0")"
oracle_python="${PYTHON:-python3.15}"
oracle_venv="${VENV_DIR:-.venv}"
if [ ! -x "$oracle_venv/bin/python" ]; then
  "$oracle_python" -m venv --without-pip "$oracle_venv"
fi
"$oracle_venv/bin/python" "$oracle_script_dir/generate.py" "$1"
