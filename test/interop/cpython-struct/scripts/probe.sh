#!/bin/bash
# Prints the interpreter generate.sh ran, as a "key: value" line.
set -euo pipefail
echo "python: $("${VENV_DIR:-.venv}/bin/python" -VV)"
