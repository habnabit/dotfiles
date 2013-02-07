# -*- sh -*-
autoload -U add-zsh-hook

if [[ $(id -u) = 0 ]]; then
  local prompt_char='#'
  local host_color='red'
else
  local prompt_char='$'
  local host_color='green'
fi
if [[ $YELP_IN_SANDBOX = 1 ]]; then
  local sandbox_indicator='%{$fg[magenta]$YELP_DOMAIN_PREFIX%}yelp.com%{$reset_color%} '
else
  local sandbox_indicator=''
fi
local user_host='%{$fg[${host_color}]%}%n@%m%{$reset_color%}'
local current_dir='%{$fg[cyan]%}%~%{$reset_color%}'
local dircount='$(ls -1 | wc -l | sed "s: ::g")'
local git_branch='$(git_prompt_info)%{$reset_color%}'
local return_code="%(?..%{$fg[red]%}  %? ↵%{$reset_color%})"

function git_prompt_status () {
  git status --porcelain | $ZSH/parse-git-status
}

function git_prompt_info () {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  gst=$(git_prompt_status)
  if [ $gst ]; then gst=": ${gst}"; fi
  echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}${gst}$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

typeset -A jobtypes
jobtypes=(running r suspended s done d)
function prompt_hab_precmd () {
  local jobs=''
  for type in ${(k)jobtypes}; do
    count=${(Mw)#jobstates#${type}#}
    if [[ $count > 0 ]]; then
      jobs="${count}${jobtypes[$type]}${jobs}"
    fi
  done
  job_counts=${jobs:+" %{$fg[magenta]%}[$jobs]%{$reset_color%}"}
}
add-zsh-hook precmd prompt_hab_precmd

ZSH_THEME_GIT_PROMPT_PREFIX="  %{$fg[yellow]%}«"
ZSH_THEME_GIT_PROMPT_SUFFIX="»%{$reset_color%}"

PROMPT="
╭── ${sandbox_indicator}${current_dir}: ${dircount}${git_branch}${return_code}
╰─ ${user_host} %{$fg[blue]%}${prompt_char}%{$reset_color%} "
RPROMPT="\${job_counts}"
