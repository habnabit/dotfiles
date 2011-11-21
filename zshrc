# -*- sh -*-

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.dotfiles/oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="hab"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

source $HOME/.dotfiles/compy-specific.sh
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"
bindkey '\e.' insert-last-word

export OCAMLRUNPARAM=b

alias HEAD="curl --head"

hutchup () {
    open -a Propane &
    open -a RescueTime &
}

hutchdown () {
    killall Propane RescueTime
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

unpack () {
    url=$1
    ext=${${${${url:t}/*./}/\?*/}:-$2}
    case "$ext" in
        gz|tgz)
            utility='gunzip';
            ;;
        bz2|tbz2)
            utility='bunzip2';
            ;;
        Z)
            utility='uncompress';
            ;;
    esac
    utility=${utility:-$2}
    wget -O- "$url" | $utility | tar xf -
}

h.it () {
    if [ $1 = "-d" ]; then
        dest="$2"
        shift 2
    fi
    rsync -abv --progress "$@" "habnab.it:public_html/$dest"
}

reload_tmux_vars() {
  if [[ -n $TMUX ]]; then
    NEW_SSH_AUTH_SOCK=`tmux showenv|grep "^SSH_AUTH_SOCK"|cut -d = -f 2`
    if [[ -n $NEW_SSH_AUTH_SOCK ]] && [[ -S $NEW_SSH_AUTH_SOCK ]]; then
      SSH_AUTH_SOCK=$NEW_SSH_AUTH_SOCK
    fi
    NEW_DISPLAY=`tmux showenv|grep "^DISPLAY"|cut -d = -f 2`
    if [[ -n $NEW_DISPLAY ]]; then
      DISPLAY=$NEW_DISPLAY
    fi
  fi
}

gffm () {
	git merge --ff-only ${1:-origin}/$(current_branch)
}
