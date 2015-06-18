#!/bin/sh

set -e
set -u

curdir="$(readlink -f "$0")"
curdir="$(dirname "$curdir")"
cd "$curdir"
omake distclean
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

cd src
omake error.ml uwt-error.h error_val.ml map_error.h configure
cp -p config.h.in configure uwt-error.h error.ml error_val.ml map_error.h "${mtmpf}/src"
cd ..
omake setup.ml _oasis
cp -p setup.ml _oasis "$mtmpf"

if which gfind >/dev/null 2>&1 ; then
    find=gfind
else
    find=find
fi

cd "$mtmpf"

$find . -type f ! -executable ! -perm 644 -exec chmod 644 {} \+
$find . -type f -executable ! -perm 755 -exec chmod 755 {} \+
$find . -type d ! -perm 755 -exec chmod 755 {} \+

$tar --transform "s|^.|${pkg}|" --format=ustar --numeric-owner -cf- . | \
    gzip -9 > "${curdir}/${pkg}.tar.gz"

omake all >/dev/null 2>&1
omake test
