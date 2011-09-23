# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.dotfiles/oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="hab"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

source $HOME/.dotfiles/compy-specific.sh
export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"
bindkey '\e.' insert-last-word

alias HEAD="curl --head"

t () {
    host=$1
    shift
    ssh -tt "${host}" "$@"
}

irc () {
    ssh -tt \
        -D 1080 \
        carlotta \
        "~/.local/bin/tmux attach -t irc"
}

irc443 () {
    ssh -tt \
        -p 443 \
        -D 1080 \
        -o "ServerAliveInterval 60" \
        carlotta \
        "~/.local/bin/tmux attach -t irc"
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
