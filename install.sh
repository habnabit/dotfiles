#!/bin/sh
set -e
SRC=$(dirname $0)
DST=$HOME
LN='echo ln'
if [ "$1" = "-d" ]; then LN='ln -v'; fi
if [ "$1" = "-f" ]; then LN='ln -fsv'; fi
install() {
    $LN -s "$SRC/$1" "$DST/.$(basename $1 | sed 's:^\.::')"
}

(cd $SRC; git submodule init && git submodule update)
(cd $SRC/oh-my-zsh; gcc parse-git-status.c -o parse-git-status)
mkdir -p $SRC/emacs.d/compy-specific
touch $SRC/emacs.d/compy-specific/init.el
touch $SRC/compy-specific.sh
install emacs.d
install emacs.d/.emacs
install gitconfig
install gitignore_global
install zshrc
