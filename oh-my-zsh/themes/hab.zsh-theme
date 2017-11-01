# -*- sh -*-
autoload -U add-zsh-hook
zmodload zsh/datetime

function hab_prompt_run () {
    cd ${1}
    shift
    hab-prompt-utils "$@"
}

function hab_prompt_results () {
    if [[ $2 == 0 ]]; then
	promptinfo=("${(0)3}")
    else
	promptinfo=(files ✘ duration ✘)
    fi
    zle && zle .reset-prompt
}

async_start_worker hab_prompt -u -n
async_register_callback hab_prompt hab_prompt_results

typeset -A promptinfo
typeset -a timer

local prompt_char='$'
local to_hash="$(whoami)@$(hostname)"
local host_color=$(hab-prompt-utils emit color-hash "${to_hash}")
local user_host='%{$FG[${host_color}]%}%n@%m%{$reset_color%}'
local current_dir='%{$fg[cyan]%}%~%{$reset_color%}'
local dircount='${promptinfo[files]}'
local vc_info='$(vc_prompt_info)%{$reset_color%}'
local return_code="  %(?.%{$fg[cyan]%}.%{$fg[red]%}%? )\${promptinfo[duration]} ↵%{$reset_color%}"

if [[ $(id -u) == 0 ]]; then
    prompt_char='#'
fi

function vc_prompt_info () {
    local vc_status="${promptinfo[vc]}"
    if [[ -n $vc_status ]]; then
	echo "$ZSH_THEME_VC_PROMPT_PREFIX${vc_status}$ZSH_THEME_VC_PROMPT_SUFFIX"
    fi
}

typeset -A jobtypes
jobtypes=(running r suspended s done d)
function hab_prompt_precmd () {
    promptinfo=(files ❖ duration ❖)
    async_job hab_prompt hab_prompt_run ${PWD} precmd ${timer} ${epochtime}
    timer=()
    local jobs=''
    for type in ${(k)jobtypes}; do
	count="${(Mw)#jobstates#${type}}"
	if [[ $count > 0 ]]; then
	    jobs="${count}${jobtypes[$type]}${jobs}"
	fi
    done
    job_counts=${jobs:+" %{$fg[magenta]%}‹$jobs›%{$reset_color%}"}
}
add-zsh-hook precmd hab_prompt_precmd

function hab_prompt_preexec () {
  timer=($epochtime)
}
add-zsh-hook preexec hab_prompt_preexec

ZSH_THEME_VC_PROMPT_PREFIX="  %{$fg[yellow]%}«"
ZSH_THEME_VC_PROMPT_SUFFIX="»%{$reset_color%}"
VIRTUAL_ENV_DISABLE_PROMPT=1

PROMPT="
╭── ${current_dir}: ${dircount}${vc_info}${return_code}
╰─ ${user_host} %{$fg[blue]%}${prompt_char}%{$reset_color%} "
RPROMPT="\${job_counts}"
