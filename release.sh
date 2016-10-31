#!/bin/sh

set -eu

umask 022
curdir="$(readlink -f "$0")"
curdir="$(dirname "$curdir")"
cd "$curdir"
omake -s distclean
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
omake -s error.ml uwt-error.h error_val.ml map_error.h configure
cp -p config.h.in configure uwt-error.h error.ml error_val.ml map_error.h "${mtmpf}/src"
cd ..
omake -s setup.ml _oasis
cp -p setup.ml _oasis "$mtmpf"

cd libuv
mkdir -p "${mtmpf}/libuv"
git archive --format=tar HEAD | ( cd "${mtmpf}/libuv" ; tar -xf- )
./autogen.sh
cp -rp  Makefile.in aclocal.m4 ar-lib compile config.guess config.sub configure depcomp install-sh ltmain.sh m4 missing "${mtmpf}/libuv/."
cd ..


if which gfind >/dev/null 2>&1 ; then
    find=gfind
else
    find=find
fi

cd "$mtmpf"
touch src/configure libuv/configure
rm -f .git* libuv/.git*

for f in "${curdir}/../libuv32.vcxproj" "${curdir}/../libuv64.vcxproj" ; do
    if [ -f "$f" ]; then
        install -m 0644 "$f" libuv
    fi
done

$find . -type f ! -executable ! -perm 644 -exec chmod 644 {} \+
$find . -type f -executable ! -perm 755 -exec chmod 755 {} \+
$find . -type d ! -perm 755 -exec chmod 755 {} \+

$tar --transform "s|^.|${pkg}|" --format=ustar --numeric-owner -chf- . | \
    gzip -9 > "${curdir}/${pkg}.tar.gz"

#omake all >/dev/null 2>&1
#omake test
