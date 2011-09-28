#!/bin/sh
set -e
SRC=$(dirname $0)
cd $SRC
git pull
cd $HOME
$SRC/install.sh -d
