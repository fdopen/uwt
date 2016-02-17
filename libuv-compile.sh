#!/bin/sh

set -eu

version=$1
CC=$2
CFLAGS=$3
unix=$4

archive="libuv-v${version}.tar.gz"
archive_url="http://dist.libuv.org/dist/v${version}/${archive}"

if [ ! -f "$archive" ]; then
    if which wget >/dev/null 2>&1 ; then
        wget --quiet "$archive_url" -O "$archive"
    else
        curl -f -L --silent "$archive_url" -o "$archive"
    fi
fi

tar -xzf "$archive"
cd "libuv-v${version}"

if [ "$unix" = "true" ]; then
    if [ ! -f .libs/libuv.a ]; then
        x="$(uname)"
        if [ "$x" = "OpenBSD" ] ; then
            AUTOCONF_VERSION="$( ls -1 /usr/local/bin/autoreconf-* | sort | tail -n 1 )"
            AUTOCONF_VERSION="${AUTOCONF_VERSION##*-}"

            AUTOMAKE_VERSION="$( ls -1 /usr/local/bin/automake-* | sort | tail -n 1 )"
            AUTOMAKE_VERSION="${AUTOMAKE_VERSION##*-}"

            export AUTOCONF_VERSION AUTOMAKE_VERSION
        fi
        if [ ! -x ./configure ]; then
            ./autogen.sh
        fi

        make=gmake
        if ! which gmake >/dev/null 2>&1 ; then
            make=make
        fi
        if [ -f Makefile ]; then
            $make clean || true
            $make distclean || true
        fi
        ./configure --enable-static --disable-shared CC="$CC" CFLAGS="$CFLAGS -DNDEBUG -Os -g0"
        $make all CC="$CC" CFLAGS="$CFLAGS -DNDEBUG -Os -g0"
    fi
    cd ..
    lib="../libuv-v${version}/.libs/libuv.a"
    rm -f src/libuv.a examples/libuv.a test/libuv.a
    ln -s "${lib}" src/libuv.a
    ln -s "${lib}" examples/libuv.a
    ln -s "${lib}" test/libuv.a
else
    if [ ! -f libuv.a ]; then
        AR=
        case "$CC" in
            *-gcc)
                AR=${CC%-*}
                AR="${AR}-ar"
                if which "$AR" >/dev/null 2>&1 ; then
                    export AR
                fi
                ;;
        esac
        export CC CFLAGS
        sed -i 's|D_WIN32_WINNT=0x0600|D_WIN32_WINNT=0x0502 -DNDEBUG -Os -g0|g' Makefile.mingw
        make -f Makefile.mingw
    fi
    cd ..
    lib="libuv-v${version}/libuv.a"
    rm -f src/libuv.a examples/libuv.a test/libuv.a
    cp "${lib}" src/libuv.a
    cp "${lib}" examples/libuv.a
    cp "${lib}" test/libuv.a
fi
