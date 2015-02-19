#!/bin/sh

set -e
set -u

curdir="$(dirname "$0")"
cd "$curdir"
omake distclean
omake setup.ml src/configure
pkg="$(omake -s echo-pkg)"
rm -rf src/autom4te.cache
find . -type f \( -name '*~' -o -name '*.omc' -o -name '.omakedb*' \) -delete

mtmpf="$(mktemp)"
trap "rm -f \"${mtmpf}\"" EXIT

if which gtar >/dev/null 2>&1 ; then
    tar=gtar
else
    tar=tar
fi
$tar --transform "s|^.|${pkg}|" --format=ustar --numeric-owner -cf "$mtmpf" .

xz -9e -k "$mtmpf" -c > "${pkg}.tar.xz"
gzip -9 -k "$mtmpf" -c > "${pkg}.tar.gz"
