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
	hab_promptinfo=("${(0)3}")
    else
	hab_promptinfo=(files ✘ duration ✘)
    fi
    zle && zle .reset-prompt
}

async_start_worker hab_prompt -u -n
async_register_callback hab_prompt hab_prompt_results

typeset -A hab_colors
typeset -A hab_promptinfo
typeset -a timer

local prompt_char='$'
local to_hash="${HAB_PROMPT_COLOR_BASE:-$(whoami)@$(hostname)}"
hab_colors=("${(0)$(hab-prompt-utils color-theme --format=zsh ${to_hash})}")
local user_host='%{$hab_colors[username]%}%n@%{$hab_colors[hostname]%}%m%{$reset_color%}'
local current_dir='%{$hab_colors[cwd]%}%~%{$reset_color%}'
local dircount='${hab_promptinfo[files]}'
local vc_info='$(vc_prompt_info)%{$reset_color%}'
local return_code="  %(?.%{$hab_colors[good_exit]%}.%{$hab_colors[bad_exit]%}%? )\${hab_promptinfo[duration]} ↵%{$reset_color%}"

if [[ $(id -u) == 0 ]]; then
    prompt_char='#'
fi

prompt_char="%{$hab_colors[prompt_char]%}${prompt_char}%{$reset_color%}"

function vc_prompt_info () {
    local vc_status="${hab_promptinfo[vc]}"
    if [[ -n $vc_status ]]; then
	echo "  %{$hab_colors[vcs]%}«${vc_status}»%{$reset_color%}"
    fi
}

typeset -A jobtypes
jobtypes=(running r suspended s done d)
function hab_prompt_precmd () {
    hab_promptinfo=(files ❖ duration ❖)
    async_job hab_prompt hab_prompt_run ${PWD} precmd ${timer} ${epochtime}
    timer=()
    local jobs=''
    for type in ${(k)jobtypes}; do
	count="${(Mw)#jobstates#${type}}"
	if [[ $count > 0 ]]; then
	    jobs="${count}${jobtypes[$type]}${jobs}"
	fi
    done
    job_counts=${jobs:+" ‹$jobs›"}
}
add-zsh-hook precmd hab_prompt_precmd

function hab_prompt_preexec () {
  timer=($epochtime)
}
add-zsh-hook preexec hab_prompt_preexec

VIRTUAL_ENV_DISABLE_PROMPT=1
PROMPT="
╭── ${current_dir}: ${dircount}${vc_info}${return_code}
╰─ ${user_host} ${prompt_char} "
RPROMPT="%{$hab_colors[rprompt]%}\${job_counts}%{$reset_color%}"
