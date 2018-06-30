#!/bin/sh

set -eu

CC=$1
CFLAGS=$2
unix=$3
system=$4
set +u
build_jobs=$5
if [ -n "$MAKEFLAGS" ] || [ -z "$build_jobs" ] || [ "$build_jobs" = "0" ]; then
    make_parallel_flags=''
else
    make_parallel_flags="-j${build_jobs}"
fi
set -u

cd "libuv"

if [ "$unix" = "true" ]; then
    if [ ! -f .libs/libuv.a ]; then
        cp -p src/unix/stream.c src/unix/stream.c.n
        if ! patch -p1 <../dist/libuv.patch >/dev/null 2>&1; then
            mv src/unix/stream.c.n src/unix/stream.c
        else
            rm src/unix/stream.c.n
        fi
        cp -p src/unix/netbsd.c src/unix/netbsd.c.n
        if ! patch -p1 <../dist/patch-src_unix_netbsd.c >/dev/null 2>&1; then
            mv src/unix/netbsd.c.n src/unix/netbsd.c
        else
            rm src/unix/netbsd.c.n
        fi
        if [ ! -x ./configure ]; then
            x="$(uname)"
            if [ "$x" = "OpenBSD" ] ; then
                AUTOCONF_VERSION="$( ls -1 /usr/local/bin/autoreconf-* | sort | tail -n 1 )"
                AUTOCONF_VERSION="${AUTOCONF_VERSION##*-}"

                AUTOMAKE_VERSION="$( ls -1 /usr/local/bin/automake-* | sort | tail -n 1 )"
                AUTOMAKE_VERSION="${AUTOMAKE_VERSION##*-}"

                export AUTOCONF_VERSION AUTOMAKE_VERSION
            fi
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
        ./configure --enable-static --disable-shared CC="$CC" CFLAGS="$CFLAGS -DNDEBUG"
        $make all $make_parallel_flags
    fi
    cd ..
    lib="../libuv/.libs/libuv.a"
    rm -f src/libuv.a examples/libuv.a test/libuv.a
    ln -s "${lib}" src/libuv.a
    ln -s "${lib}" src-log/libuv.a
    ln -s "${lib}" examples/libuv.a
    ln -s "${lib}" test/libuv.a
else
    cp -p src/win/thread.c src/win/thread.c.n
    if ! patch -p1 <../dist/mingw.patch >/dev/null 2>&1; then
        mv src/win/thread.c.n src/win/thread.c
    else
        rm src/win/thread.c.n
    fi
    if [ "$system" = "win32" ] || [ "$system" = "win64" ] ; then
        if [ ! -f Release/lib/libuv.lib ]; then
            if [ ! -f libuv32.vcxproj ] && [ -f ../dist/libuv32.vcxproj ]; then
                cp ../dist/libuv32.vcxproj .
                cp ../dist/libuv64.vcxproj .
            fi
            if [ ! -f libuv32.vcxproj ]; then
                # checkout doesn't work at the moment. They've enabled
                # options inside common.gypi that lead to an ouptut
                # that is not understood by flexlink. Changing them via
                # grep/sed is too fragile. Things to disable: LTCG /
                # WholeProgramOptimization
                pform=x64
                target_arch=x64
                if [ "$system" = "win32" ]; then
                    target_arch=ia32
                    pform=x86
                fi
                set +u
                if [ "$PYTHON" = "" ]; then
                    # note: cygwin's python doesn't seem to work at the moment
                    PYTHON=python
                fi
                if [ "$GIT" = "" ]; then
                    GIT=git
                fi
                set -u
                if [ ! -d build/gyp ]; then
                    "$GIT" clone --depth=1 https://chromium.googlesource.com/external/gyp build/gyp
                fi
                "$PYTHON" gyp_uv.py -Dtarget_arch="$target_arch" -Duv_library=static_library
            else
                if [ "$system" = "win32" ]; then
                    cp libuv32.vcxproj libuv.vcxproj
                    pform=x86
                else
                    pform=x64
                    cp libuv64.vcxproj libuv.vcxproj
                fi
            fi
            set +u
            to_add=
            case "$WINDOWSSDKVERSION" in
                10*)
                    to_add="/p:WindowsTargetPlatformVersion=${WINDOWSSDKVERSION}"
                    ;;
            esac
            case "$VISUALSTUDIOVERSION" in
                15.*)
                    # TODO: how to detect special settings like v141_xp ?
                    to_add="${to_add} /p:PlatformToolset=v141"
                    ;;
            esac
            set -u
            MSBuild.exe libuv.vcxproj '/t:Build' "/p:Configuration=Release;Platform=$pform" '/clp:NoSummary;NoItemAndPropertyList;Verbosity=minimal' '/nologo' $to_add >&2
        fi
        lib="libuv/Release/lib/libuv.lib"
        ext_lib=".lib"
    else
        if [ ! -f .libs/libuv.a ]; then
            if [ ! -x ./configure ]; then
                ./autogen.sh
            fi
            case "$system" in
                mingw64*)
                    host='x86_64-w64-mingw32'
                    ;;
                *)
                    host='i686-w64-mingw32'
                    ;;
            esac
            if [ -x /bin/dash ]; then
                # help libtool to avoid forks
                export _G_HAVE_ARITH_OP="yes"
                export _G_HAVE_XSI_OPS="yes"
                export _G_HAVE_PLUSEQ_OP="no"
                /bin/dash ./configure --host=$host --enable-static --disable-shared SHELL=/bin/dash CC="$CC" CFLAGS="$CFLAGS -DNDEBUG" ac_cv_search_pthread_create=no
            else
                ./configure --host=$host --enable-static --disable-shared CC="$CC" CFLAGS="$CFLAGS -DNDEBUG" ac_cv_search_pthread_create=no
            fi
            set +u
            jflags=$make_parallel_flags
            if [ -z "$jflags" ] && [ -n "$NUMBER_OF_PROCESSORS" ] && [ -z "$MAKEFLAGS" ]; then
                case "$NUMBER_OF_PROCESSORS" in
                    ''|*[!0-9]*)
                        jflags=
                        ;;
                    *)
                        jflags="-j$NUMBER_OF_PROCESSORS"
                        ;;
                esac
            fi
            set -u
            make all $jflags
        fi
        lib="libuv/.libs/libuv.a"
        ext_lib=".a"
    fi
    cd ..
    rm -f src/libuv"${ext_lib}" examples/libuv"${ext_lib}" test/libuv"${ext_lib}"
    cp "${lib}" src/libuv"${ext_lib}"
    cp "${lib}" src-log/libuv"${ext_lib}"
    cp "${lib}" examples/libuv"${ext_lib}"
    cp "${lib}" test/libuv"${ext_lib}"
fi
