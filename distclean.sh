#!/bin/sh

set -e
set -u

curdir="$(dirname "$0")"
cd "$curdir"
omake distclean
find . -type f -name '*.omc' -delete
find . -type f -name '.omakedb*' -delete
