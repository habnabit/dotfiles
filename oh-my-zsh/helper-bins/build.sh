#!/bin/sh -eux
cargo build --release
bindir=$(printf "lfs-%s-%s" $(uname -sm))
mkdir -p "${bindir}"
cp target/release/hab-prompt-utils "${bindir}/"
