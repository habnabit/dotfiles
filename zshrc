# -*- sh -*-

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.dotfiles/oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="hab"

local helpers_dir=$(printf "${ZSH}/helper-bins/bin-%s-%s" $(uname -sm))
if [[ -d ${helpers_dir}-alt ]]; then
   local helpers_dir="${helpers_dir}-alt"
fi
export PATH="${PATH}:${helpers_dir}"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(zsh-syntax-highlighting passacre)

if [[ $TERM == "dumb" ]]; then
    PS1="$ "
else
    source $ZSH/oh-my-zsh.sh
fi

# Customize to your needs...

source $HOME/.dotfiles/compy-specific.sh
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"

bindkey '\e.' insert-last-word
setopt transientrprompt extendedhistory histignoredups histexpiredupsfirst \
    histfindnodups histsavenodups histreduceblanks
unsetopt correct_all
HISTSIZE=10000000
SAVEHIST=10000000

export OCAMLRUNPARAM=b
: ${LANG:=en_US.UTF-8}; export LANG
: ${remote_emacs_auth:="$HOME/.emacs.d/remote-server"}; export remote_emacs_auth
[[ -z $SSH_AUTH_SOCK || ! -e $SSH_AUTH_SOCK ]] && export SSH_AUTH_SOCK=$HOME/.ssh/auth-sock

alias HEAD="curl --head"
alias cleanpip="rm -vrf ${TMPDIR:-/tmp}/pip-build-${USER}"
alias pc="passacre generate -c"
alias pcc="pc -C"
alias pce="${EDITOR:-emacs} ~/.passacre.yaml"
alias pcs="passacre sitehash -c"
alias pcw="pc -C -w 10"
alias tpng="pngpaste - | tshort -e .png"
alias tpaste="pbpaste - | tshort -e .txt"

mkproj () {
    local dir=$HOME/Projects/$(date +"%Y%m%d-$1")
    mkdir -p "$dir"
    cd "$dir"
}

mkc () {
    mkdir -p "$1"
    cd "$1"
}

t () {
    host=$1
    shift
    ssh -tt "${host}" "$@"
}

irc () {
    ssh -tt \
        -D 1080 \
        "$@" \
        carlotta \
        "~/.local/bin/tmux attach -t irc"
}

irc443 () {
    irc -p 443 -o "ServerAliveInterval 60" "$@"
}

irc-mosh () {
     /usr/bin/perl =mosh "$@" carlotta -- \
         /home/habnabit/.local/bin/tmux -2u attach -tirc
}

gffm () {
    git merge --ff-only ${1:-origin}/$(current_branch)
}

grbo () {
    git rebase ${1:-origin}/$(current_branch)
}
