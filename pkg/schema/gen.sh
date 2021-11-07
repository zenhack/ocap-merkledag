#!/usr/bin/env bash

set -euo pipefail

schema='protocol diskstore'

do_gen() {
	for s in $schema; do
		[ -d $s ] || mkdir $s
		cd $s
		capnp compile -ogo  \
			-I $GO_CAPNP_SRC/std \
			--src-prefix=../ \
			../$s.capnp
		cd ..
	done
}

do_clean() {
	for s in $schema; do
		rm -rf $s
	done
}

err_usage() {
	echo "Usage: $0 (gen | clean)" 2>&1
	exit 1
}

cd "$(dirname $0)"

[ "$#" = 1 ] || err_usage

case "$1" in
	gen) do_gen;;
	clean) do_clean;;
	*) err_usage;;
esac
