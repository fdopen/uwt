#!/bin/sh

set -eu

curdir="$(dirname "$0")"
if which readlink >/dev/null 2>&1 ; then
    curdir="$(readlink -f "$curdir")"
fi

cd "$curdir"
if [ ! -x distclean.sh ] || [ ! -f OMakeIncludes ]; then
    exit 1
fi
omake distclean
find . -type f -name '*.omc' -exec rm {} \+
find . -type f -name '.omakedb*' -exec rm {} \+
