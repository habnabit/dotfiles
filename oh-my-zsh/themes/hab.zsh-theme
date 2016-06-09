# -*- sh -*-
autoload -U add-zsh-hook

local prompt_char='$'
local to_hash="$(whoami)@$(hostname)"
local host_color=$(hab-prompt-utils emit color-hash "${to_hash}")
local user_host='%{$FG[${host_color}]%}%n@%m%{$reset_color%}'
local current_dir='%{$fg[cyan]%}%~%{$reset_color%}'
local dircount='$(hab-prompt-utils emit file-count)'
local vc_info='$(vc_prompt_info)%{$reset_color%}'
local return_code="  %(?.%{$fg[cyan]%}.%{$fg[red]%}%?) \${timer_show}s ↵%{$reset_color%}"
local timer
local timer_show="0"

if [[ $(id -u) = 0 ]]; then
  prompt_char='#'
fi

function vc_prompt_info () {
  local vc_status="$(hab-prompt-utils emit vc-status)"
  if [[ -n $vc_status ]]; then
      echo "$ZSH_THEME_VC_PROMPT_PREFIX${vc_status}$ZSH_THEME_VC_PROMPT_SUFFIX"
  fi
}

typeset -A jobtypes
jobtypes=(running r suspended s done d)
function prompt_hab_precmd () {
  local jobs=''
  for type in ${(k)jobtypes}; do
    count=${(Mw)#jobstates#${type}}
    if [[ $count > 0 ]]; then
      jobs="${count}${jobtypes[$type]}${jobs}"
    fi
  done
  job_counts=${jobs:+" %{$fg[magenta]%}‹$jobs›%{$reset_color%}"}

  environment_indicator="$sandbox"
  if [[ -n $VIRTUAL_ENV ]]; then
    local venv_name=$(basename "$VIRTUAL_ENV")
    environment_indicator="${environment_indicator:+$environment_indicator }${venv_name}"
  fi
  environment_indicator=${environment_indicator:+"%{$fg[magenta]$environment_indicator%}%{$reset_color%} "}
  if [ $timer ]; then
    timer_show=$(($SECONDS - $timer))
    unset timer
  fi
}
add-zsh-hook precmd prompt_hab_precmd

function prompt_hab_preexec () {
  timer=${timer:-$SECONDS}
}
add-zsh-hook preexec prompt_hab_preexec

ZSH_THEME_VC_PROMPT_PREFIX="  %{$fg[yellow]%}«"
ZSH_THEME_VC_PROMPT_SUFFIX="»%{$reset_color%}"
VIRTUAL_ENV_DISABLE_PROMPT=1

PROMPT="
╭── \${environment_indicator}${current_dir}: ${dircount}${vc_info}${return_code}
╰─ ${user_host} %{$fg[blue]%}${prompt_char}%{$reset_color%} "
RPROMPT="\${job_counts}"
