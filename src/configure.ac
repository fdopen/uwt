AC_INIT([uwt], [0.3.1],[andreashauptmann@t-online.de],[uwt])

AC_PROG_CC

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([config.inc])
AC_USE_SYSTEM_EXTENSIONS

AC_MSG_CHECKING([posix source 200809L])

AC_CHECK_HEADERS(errno.h stdint.h unistd.h errno.h limits.h sys/stat.h sys/types.h sys/socket.h fcntl.h netinet/in.h netdb.h grp.h pwd.h sys/param.h sys/un.h arpa/inet.h)
AC_CHECK_FUNCS(strdup)
AC_CHECK_DECLS([strnlen], [], [], [#include <string.h>])
AC_CHECK_DECLS([uv_os_tmpdir,uv_fs_copyfile,uv_fs_lchown,uv_os_get_passwd,uv_translate_sys_error,uv_os_getenv,uv_os_setenv,uv_os_unsetenv,uv_signal_start_oneshot,uv_os_getppid,uv_pipe_chmod,UV_DISCONNECT,UV_PRIORITIZED],[],[],[#include <uv.h>])

AC_HEADER_STDBOOL
AC_SYS_LARGEFILE
AC_C_TYPEOF

AC_C_BIGENDIAN(
  AC_DEFINE(ENDIAN_BIG, 1, [machine is bigendian]),
  AC_DEFINE(ENDIAN_LITTLE, 1, [machine is littleendian]),
  AC_MSG_ERROR(unknown endianess),
  AC_MSG_ERROR(universial endianess not supported)
)

AC_MSG_CHECKING([_Static_assert])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([_Static_assert (1, "");],
                                   [(void) 0])],
                  [AC_DEFINE([HAVE_STATIC_ASSERT], [1],
                             [Whether _Static_assert can be used or not])
                              AC_MSG_RESULT([yes])],
                               [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([attribute "noreturn"])
AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([[
#include <stdlib.h>

void bar(void) __attribute__ ((noreturn));

void
bar(void)
{
	exit(0);
}
]])],
    [AC_MSG_RESULT([yes]); AC_DEFINE(ATTR_NORETURN,
	[__attribute__ ((noreturn))], ["noreturn" attribute.])],
    [AC_MSG_RESULT([no]);  AC_DEFINE(ATTR_NORETURN,
	[], ["noreturn" attribute.])])

AC_MSG_CHECKING([attribute "packed"])
AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([[
struct bar {
	int x;
} __attribute__ ((packed));
]])],
    [AC_MSG_RESULT([yes]); AC_DEFINE(ATTR_PACKED,
	[__attribute__ ((packed))], ["packed" attribute.])],
    [AC_MSG_RESULT([no]);  AC_DEFINE(ATTR_PACKED,
	[], ["packed" attribute.])])

AC_MSG_CHECKING([attribute "always_inline"])
AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([[
static inline __attribute__ ((always_inline)) int bar(int foo) {
   return (foo*2);
} ;
]])],
    [AC_MSG_RESULT([yes]); AC_DEFINE(FORCE_INLINE,
        [inline __attribute__ ((always_inline))], ["always_inline" attribute.])],
    [AC_MSG_RESULT([no]);  AC_DEFINE(FORCE_INLINE,
       [], ["always_inline" attribute.])])

AC_MSG_CHECKING([attribute "unused"])
AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([[
int x __attribute__((unused));

int
bar(int y __attribute__((unused)))
{
	return (0);
}
]])],
    [AC_MSG_RESULT([yes]); AC_DEFINE(ATTR_UNUSED,
	[__attribute__ ((unused))], ["unused" attribute.])],
    [AC_MSG_RESULT([no]);  AC_DEFINE(ATTR_UNUSED,
	[], ["unused" attribute.])])

AC_MSG_CHECKING(for __func__)
AH_TEMPLATE(__func__, [C99-specified function identifier])
AC_TRY_COMPILE([
#include <stdio.h>
],[printf ("%s", __func__);],
have_func=yes, have_func=no)
if test "x$have_func" = "xyes"; then
   AC_MSG_RESULT(yes)
else
   AC_MSG_RESULT(no)
   AC_MSG_CHECKING(for __FUNCTION__)
   AC_TRY_COMPILE([
#include <stdio.h>
],[printf ("%s", __FUNCTION__);],
have_function=yes, have_function=no)
   if test "x$have_function" = "xyes"; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(__func__, __FUNCTION__)
   else
      AC_MSG_RESULT(no)
      AC_DEFINE(__func__, __FILE__)
   fi
fi

AC_MSG_CHECKING([if the compiler supports __builtin_clz])
AC_LINK_IFELSE([AC_LANG_PROGRAM(,
return ( __builtin_clz(0x10000u) == 15) ? 1 : 0
)], [
have_builtin_clz=yes
AC_MSG_RESULT([yes])
], [
have_builtin_clz=no
AC_MSG_RESULT([no])
])
if test x$have_builtin_clz = xyes ; then
AC_DEFINE([HAVE_BUILTIN_CLZ], [1], [Define to 1 if the compiler support __builtin_clz.])
fi

# Check for __builtin_expect()
AC_MSG_CHECKING([for __builtin_expect()])
AC_LINK_IFELSE([AC_LANG_PROGRAM(, return __builtin_expect(main != 0, 1))],
[AC_DEFINE(HAVE_BUILTIN_EXPECT, 1,
Define to 1 if compiler supports __builtin_expect)
AC_MSG_RESULT([yes])],
[AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_types_compatible_p()])
AC_LINK_IFELSE(
[AC_LANG_SOURCE([int main(void) { return __builtin_types_compatible_p(char *, int) ? 1 : 0; }])],
[AC_DEFINE([HAVE_BUILTIN_TYPES_COMPATIBLE_P], [1],[Define if have __builtin_types_compatible_p])
 AC_MSG_RESULT([yes])],
[AC_MSG_RESULT([no])]
)

AC_CACHE_CHECK([for statement expressions],
  [uwt_cv_statement_expressions],
  [AC_COMPILE_IFELSE(
     [AC_LANG_PROGRAM([], [[return ({ int x = 5; x-x; });]])],
     [uwt_cv_statement_expressions=yes],
     [uwt_cv_statement_expressions=no])])

if test "$uwt_cv_statement_expressions" = yes; then
  AC_DEFINE([HAVE_STATEMENT_EXPRESSIONS], 1,
    [Define to 1 if statement expressions work.])
fi

AC_SEARCH_LIBS(gethostbyname_r, [socket nsl])
AC_SEARCH_LIBS(getservbyname_r, [socket nsl])

OS_MACRO=0
AC_CHECK_DECLS([_WIN32],
OS_MACRO="Windows"
HAVE_WINDOWS=1,
HAVE_WINDOWS=0)
AC_SUBST(HAVE_WINDOWS)

AC_CHECK_DECLS([__ANDROID__],OS_MACRO="Android")
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__linux__],OS_MACRO="Linux")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__APPLE__],OS_MACRO="Mac")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__FreeBSD__],OS_MACRO="Freebsd")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__OpenBSD__],OS_MACRO="Openbsd")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__CYGWIN__],OS_MACRO="Cygwin")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__NetBSD__],OS_MACRO="Netbsd")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__sun],OS_MACRO="Sun")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__hpux],OS_MACRO="Hp")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__DragonFly__],OS_MACRO="Dragonfly")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([_AIX],OS_MACRO="Aix")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__minix],OS_MACRO="Minix")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__FreeBSD_kernel__, __GLIBC__],OS_MACRO="KFreebsd")
fi
if test "$OS_MACRO" = "0" ; then
AC_CHECK_DECLS([__bsdi__],OS_MACRO="Bsd")
fi
if test "$OS_MACRO" = "0" ; then
OS_MACRO="Unknown"
fi
AC_SUBST(OS_MACRO)


AH_TEMPLATE([HAVE_GETHOSTBYyyyy_R_POSIX], [Define if POSIX-style gethostbyYYYY_r functions are available.])
AH_TEMPLATE([HAVE_GETSERVBYyyyy_R_POSIX], [Define if POSIX-style getservbyYYYY_r functions are available.])
AH_TEMPLATE([HAVE_GETPROTOBYyyyy_R_POSIX], [Define if POSIX-style getprotobyYYYY_r functions are available.])

AH_TEMPLATE([HAVE_GETHOSTBYyyyy_R_SUN], [Define if SUN-style gethostbyYYYY_r functions are available.])
AH_TEMPLATE([HAVE_GETSERVBYyyyy_R_SUN], [Define if SUN-style getservbyYYYY_r functions are available.])
AH_TEMPLATE([HAVE_GETPROTOBYyyyy_R_SUN], [Define if SUN-style getprotobyYYYY_r functions are available.])


if test "$OS_MACRO" = "Netbsd" -o "$OS_MACRO" = "Windows" ; then
HAVE_REENTRANT_HOST_POSIX=0
HAVE_REENTRANT_PROTO_POSIX=0
HAVE_REENTRANT_SERV_POSIX=0

HAVE_REENTRANT_HOST_SUN=0
HAVE_REENTRANT_PROTO_SUN=0
HAVE_REENTRANT_SERV_SUN=0
else
AC_MSG_CHECKING([for POSIX-style gethostbyXXXX_r])
AC_TRY_LINK([
#define _POSIX_PTHREAD_SEMANTICS
#include <netdb.h>
#include <stddef.h>
],
[
struct hostent *he;
he = gethostbyname_r((const char *)NULL, (struct hostent *)NULL,
(char *)NULL, (int)0, (struct hostent **)NULL, (int *)NULL);
he = gethostbyaddr_r((const char *)NULL, (int)0, (int)0,
(struct hostent *)NULL, (char *)NULL, (int)0, (struct hostent **)NULL,
(int *)NULL);
],
HAVE_REENTRANT_HOST_POSIX=1
AC_DEFINE(HAVE_GETHOSTBYyyyy_R_POSIX)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_HOST_POSIX=0
AC_MSG_RESULT([no])
)

AC_MSG_CHECKING([for SUN-style gethostbyXXXX_r])
AC_TRY_LINK([
#include <netdb.h>
#include <stddef.h>
],
[
struct hostent *he;
he = gethostbyname_r((const char *)NULL, (struct hostent *)NULL,
(char *)NULL, (int)0, (int *)NULL);
he = gethostbyaddr_r((const char *)NULL, (int)0, (int)0,
(struct hostent *)NULL, (char *)NULL, (int)0, (int *)NULL);
],
HAVE_REENTRANT_HOST_SUN=1
AC_DEFINE(HAVE_GETHOSTBYyyyy_R_SUN)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_HOST_SUN=0
AC_MSG_RESULT([no])
)

AC_MSG_CHECKING([for POSIX-style getservbyXXXX_r])
AC_TRY_LINK([
#define _POSIX_PTHREAD_SEMANTICS
#include <netdb.h>
#include <stddef.h>
],
[
struct servent *se;
se = getservbyname_r((const char *)NULL, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0, (struct servent **)NULL);
se = getservbyport_r((int)0, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0, (struct servent **)NULL);
],
HAVE_REENTRANT_SERV_POSIX=1
AC_DEFINE(HAVE_GETSERVBYyyyy_R_POSIX)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_SERV_POSIX=0
AC_MSG_RESULT([no])
)

AC_MSG_CHECKING([for SUN-style getservbyXXXX_r])
AC_TRY_LINK([
#include <netdb.h>
#include <stddef.h>
],
[
struct servent *se;
se = getservbyname_r((const char *)NULL, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0);
se = getservbyport_r((int)0, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0);
],
HAVE_REENTRANT_SERV_SUN=1
AC_DEFINE(HAVE_GETSERVBYyyyy_R_SUN)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_SERV_SUN=0
AC_MSG_RESULT([no])
)

AC_MSG_CHECKING([for POSIX-style getprotobyXXXX_r])
AC_TRY_LINK([
#define _POSIX_PTHREAD_SEMANTICS
#include <netdb.h>
#include <stddef.h>
],
[
int x;
x = getprotobyname_r(NULL,NULL,NULL,(size_t)0,(struct protoent **)NULL);
x = getprotobynumber_r(0,NULL,NULL,(size_t)0,(struct protoent **)NULL);
],
HAVE_REENTRANT_PROTO_POSIX=1
AC_DEFINE(HAVE_GETPROTOBYyyyy_R_POSIX)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_PROTO_POSIX=0
AC_MSG_RESULT([no])
)

AC_MSG_CHECKING([for SUN-style getprotobyXXXX_r])
AC_TRY_LINK([
#include <netdb.h>
#include <stddef.h>
],
[
int x;
x = getprotobyname_r(NULL,NULL,NULL,(size_t)0);
x = getprotobynumber_r(0,NULL,NULL,(size_t)0);
],
HAVE_REENTRANT_PROTO_SUN=1
AC_DEFINE(HAVE_GETPROTOBYyyyy_R_SUN)
AC_MSG_RESULT([yes]),
HAVE_REENTRANT_PROTO_SUN=0
AC_MSG_RESULT([no])
)
fi

AC_SUBST(HAVE_REENTRANT_HOST_POSIX)
AC_SUBST(HAVE_REENTRANT_SERV_POSIX)
AC_SUBST(HAVE_REENTRANT_PROTO_POSIX)
AC_SUBST(HAVE_REENTRANT_HOST_SUN)
AC_SUBST(HAVE_REENTRANT_SERV_SUN)
AC_SUBST(HAVE_REENTRANT_PROTO_SUN)


AC_CHECK_FUNCS(getlogin cuserid getpwnam getgrnam getpwuid getgrgid)
AC_CHECK_FUNCS(getlogin_r getpwnam_r getgrnam_r getpwuid_r getgrgid_r)
AC_CHECK_FUNCS(chroot lockf pipe2)

if test "$ac_cv_func_getlogin_r" = "yes" ; then
HAVE_GETLOGIN_R=1
else
HAVE_GETLOGIN_R=0
fi
AC_SUBST(HAVE_GETLOGIN_R)

if test "$ac_cv_func_getgrnam_r" = "yes" \
     -a "$ac_cv_func_getpwnam_r" = "yes" \
     -a "$ac_cv_func_getpwuid_r" = "yes" \
     -a "$ac_cv_func_getgrgid_r" = "yes" \
; then
HAVE_REENTRANT_PASSWD=1
else
HAVE_REENTRANT_PASSWD=0
fi
AC_SUBST(HAVE_REENTRANT_PASSWD)

AC_MSG_CHECKING([uv_translate_sys_error])
LIBS="-luv $LIBS"
AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([[
#include <uv.h>
extern int uv_translate_sys_error(int);
int
uwt_translate(void)
{
    int er = uv_translate_sys_error(30);
    return (er);
}
]])],
    [AC_MSG_RESULT([yes]);
     AC_DEFINE([HAVE_UV_TRANSLATE_SYSERROR], [1], [Define to 1 if libuv supports it.])],
    [AC_MSG_RESULT([no])])

AC_CHECK_MEMBER(struct sockaddr_in.sin_len,
                AC_DEFINE(HAVE_SIN_LEN, 1, [Define this if your IPv4 has sin_len in sockaddr_in struct.]),,
                [#ifdef HAVE_SYS_TYPES_H
                 #include <sys/types.h>
                 #endif
                 #ifdef _WIN32
                 #include <winsock2.h>
                 #include <windows.h>
                 #else
                 #include <netinet/in.h>
                 #endif])

AC_CHECK_MEMBER(struct sockaddr_in6.sin6_len,
                AC_DEFINE(HAVE_SIN6_LEN, 1, [Define this if your IPv6 has sin6_len in sockaddr_in6 struct.]),,
                [#ifdef HAVE_SYS_TYPES_H
                 #include <sys/types.h>
                 #endif
                 #ifdef _WIN32
                 #include <winsock2.h>
                 #include <windows.h>
                 #else
                 #include <netinet/in.h>
                 #endif])

AC_OUTPUT
