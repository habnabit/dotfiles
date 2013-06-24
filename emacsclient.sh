#!/bin/sh
nowait=0
if [ "$1" = "-n" ]; then
    nowait=1
    shift
fi

if [ -z "$remote_emacs_auth" ] || [ ! -e "$remote_emacs_auth" ] || ! nc -z $(sed -n 1p "$remote_emacs_auth"); then
    echo "no emacs server"
    sleep 1
    exec emacs "$1"
fi

quote () {
    echo "$1" | sed 's:&:\&\&:g;s:-:\&-:g;s: :\&_:g'
}

quoted_pwd=$(quote $(pwd))
quoted_file=$(quote "$1")
case "$1" in
    /*) quoted_file_path="$quoted_file";;
    *)  quoted_file_path="${quoted_pwd}/${quoted_file}";;
esac

tramp_auth=$(sed -n 2p "$remote_emacs_auth")
tramp_prefix=$(sed -n 3p "$remote_emacs_auth")
args=$"${tramp_auth}\n-dir ${tramp_prefix}${quoted_pwd} -file ${tramp_prefix}${quoted_file_path}"
[ "$nowait" != 0 ] && args="$args -nowait"
echo "$args" | nc $(sed -n 1p "$remote_emacs_auth")
