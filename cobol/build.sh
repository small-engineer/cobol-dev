#!/bin/bash
set -euo pipefail
shopt -s nullglob

echo "[build] pre-compile modules/*.cob"
for src in modules/*.cob; do
  ocesql "$src" "${src%.cob}.pcob"
done

echo "[build] pre-compile OCESQL runtime sources"
for src in "$OCESQL_SRC"/*.cob; do
  ocesql "$src" "$OCESQL_SRC/$(basename "${src%.cob}").pcob"
done

echo "[build] pre-compile main.cob"
ocesql main.cob main.pcob

echo "[build] link objects"
cobc -x \
     main.pcob \
     modules/*.pcob \
     "$OCESQL_SRC"/*.pcob \
     -locesql -locesqlrt \
     -o main

echo "[build] completed."
