#!/bin/sh -eux
bindir=$(printf "bin-%s-%s" $(uname -sm))
prompt_utils_desc="oh-my-zsh/helper-bins/${bindir}/hab-prompt-utils"
if "${prompt_utils_desc}" -V >/dev/null 2>&1; then
    prompt_utils="${prompt_utils_desc}"
else
    prompt_utils_dir="oh-my-zsh/helper-bins/${bindir}-alt"
    mkdir -p "${prompt_utils_dir}"
    prompt_utils="${prompt_utils_dir}/hab-prompt-utils"
    cp "${prompt_utils_desc}" "${prompt_utils}"
    python lfs-fetch.py .lfsconfig "${prompt_utils}"
    chmod +x "${prompt_utils}"
fi
"${prompt_utils}" install ./Manifest "${HOME}"
