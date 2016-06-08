#!/bin/sh -eux
cargo build --release --target="$1"
bindir=$(printf "bin-%s-%s" $(uname -sm))
mkdir -p "${bindir}"
cp target/"$1"/release/hab-prompt-utils "${bindir}/"
