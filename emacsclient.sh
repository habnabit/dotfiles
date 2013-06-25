#!/bin/sh
nowait=0
if [ "$1" = "-n" ]; then
    nowait=1
    shift
fi

if [ "$#" -eq 0 ]; then
    echo "no files specified"
    exit 0
fi

if [ -z "$remote_emacs_auth" ] || [ ! -e "$remote_emacs_auth" ] || ! nc -z $(sed -n 1p "$remote_emacs_auth"); then
    echo "no emacs server"
    sleep 1
    exec emacs "$@"
fi

quote () {
    echo "$1" | sed 's:&:\&\&:g;s:-:\&-:g;s: :\&_:g'
}

tramp_auth=$(sed -n 2p "${remote_emacs_auth}")
tramp_prefix=$(sed -n 3p "${remote_emacs_auth}")
args=$(printf "%s\n%s\n" "${tramp_auth}" "-dir ${tramp_prefix}${quoted_pwd}")
[ "${nowait}" != 0 ] && args="${args} -nowait"
quoted_pwd=$(quote "$(pwd)")

for file; do
    quoted_file=$(quote "${file}")
    case "${file}" in
        +*) argument="-position ${quoted_file}";;
        /*) argument="-file ${tramp_prefix}${quoted_file}";;
        *)  argument="-file ${tramp_prefix}${quoted_pwd}/${quoted_file}";;
    esac
    args="${args} ${argument}"
done

printf "%s\n" "${args}" | nc $(sed -n 1p "${remote_emacs_auth}")
