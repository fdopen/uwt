#!/bin/sh

set -eu

curdir="$(dirname "$0")"
cd "$curdir"

if [ ! -x distclean.sh ] || [ ! -f OMakeIncludes ] || [ ! -d src ] ; then
    exit 1
fi
omake distclean
find . -type f -name '*.omc' -exec rm {} \+
find . -type f -name '.omakedb*' -exec rm {} \+

if [ -d .git ]; then
    if which git >/dev/null 2>&1; then
        git clean -dxf
    fi
fi
