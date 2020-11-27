#!/usr/bin/env sh
set -euo pipefail
cd "$(dirname $0)"
[ -d gen ] || mkdir gen
capnp compile -ohaskell:gen --src-prefix=schema/ schema/*.capnp
