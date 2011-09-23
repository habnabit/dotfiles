#!/bin/sh
SRC=$(dirname $0)
DST=$HOME
LN='echo ln'
if [ "$1" = "-d" ]; then LN=ln; fi
if [ "$1" = "-f" ]; then LN='ln -fs'; fi
install() {
    $LN -s "$SRC/$1" "$DST/.$(basename $1 | sed 's:^\.::')"
}

(cd $SRC; git submodule init && git submodule update)
mkdir -p $SRC/emacs.d/compy-specific
touch $SRC/emacs.d/compy-specific/init.el
install emacs.d
install emacs.d/.emacs
install gitconfig
install gitignore_global
install zshrc
