#!/bin/sh -eux
bindir=$(printf "bin-%s-%s" $(uname -sm))
prompt_utils="oh-my-zsh/helper-bins/${bindir}"
python lfs-fetch.py .lfsconfig "${prompt_utils}"
"${prompt_utils}" install "$(pwd)/Manifest" "${HOME}"
