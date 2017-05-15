# -*- sh -*-

if [[ $TERM == "dumb" ]]; then
    zmodload zsh/zle
    unset zle_bracketed_paste
    PS1="$ "
    return 0
fi

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.dotfiles/oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="hab"

local helpers_dir=$(printf "${ZSH}/helper-bins/bin-%s-%s" $(uname -sm))
export PATH="${PATH}:${helpers_dir}"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(zsh-syntax-highlighting passacre)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

source $HOME/.dotfiles/compy-specific.sh
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"

bindkey '\e.' insert-last-word
setopt transientrprompt extendedhistory histignoredups histexpiredupsfirst \
    histfindnodups histsavenodups histreduceblanks auto_cd
unsetopt correct_all
HISTSIZE=10000000
SAVEHIST=10000000
cdpath=("$HOME/Projects")

export OCAMLRUNPARAM=b
: ${LANG:=en_US.UTF-8}; export LANG

export EDITOR="$(which emacsclient) --alternate-editor ex --server-file ${HOME}/.emacs.d/server/server"
alias e="${EDITOR} --no-wait"
alias HEAD="curl --head"
alias pc="passacre generate -c"
alias pcc="pc -C"
alias pce="${EDITOR:-emacs} ~/.passacre.yaml"
alias pcs="passacre sitehash -c"
alias pcw="pc -C -w 10"
alias tpng="pngpaste - | tshort -e .png"
alias tpaste="pbpaste - | tshort -e .txt"
alias reset="tput reset rmcup"

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

gffm () {
    git merge --ff-only ${1:-origin}/$(hab-prompt-utils emit git-head-branch)
}

grbo () {
    git rebase ${1:-origin}/$(hab-prompt-utils emit git-head-branch)
}
