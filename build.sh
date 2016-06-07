#!/bin/sh -eux
cargo build --release --target="$1"
bin=$(printf "hab-prompt-utils-%s-%s" $(uname -sm))
cp target/"$1"/release/hab-prompt-utils bin/"${bin}"
