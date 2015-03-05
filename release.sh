#!/bin/sh

set -e
set -u

curdir="$(readlink -f "$0")"
curdir="$(dirname "$curdir")"
cd "$curdir"
omake distclean
omake setup.ml src/configure
pkg="$(omake -s echo-pkg)"
find . -type f \( -name '*~' -o -name '*.omc' -o -name '.omakedb*' \) -delete

mtmpf="$(mktemp -d)"
trap "rm -rf \"${mtmpf}\"" EXIT

if which gtar >/dev/null 2>&1 ; then
    tar=gtar
else
    tar=tar
fi

stash="$(git stash create)"
git archive --format=tar ${stash:-HEAD} | ( cd "$mtmpf" ; tar -xf- )

cp -p src/config.h.in src/configure "${mtmpf}/src"
cp -p setup.ml _oasis "$mtmpf"

if which gfind >/dev/null 2>&1 ; then
    find=gfind
else
    find=find
fi

$find "$mtmpf" -type f ! -executable ! -perm 644 -exec chmod 644 {} \+
$find "$mtmpf" -type f -executable ! -perm 755 -exec chmod 755 {} \+
$find "$mtmpf" -type d ! -perm 755 -exec chmod 755 {} \+

cd "$mtmpf"

$tar --transform "s|^.|${pkg}|" --format=ustar --numeric-owner -cf- . | \
    gzip -9 > "${curdir}/${pkg}.tar.gz"

omake all >/dev/null 2>&1
omake test
