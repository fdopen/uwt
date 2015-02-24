AC_INIT([uwt], [0.0.1],[uwt@ml.ignorelist.com],[uwt])

AC_PROG_CC

AC_CONFIG_HEADERS([config.h])
AC_USE_SYSTEM_EXTENSIONS

AC_MSG_CHECKING([posix source 200809L])

AC_CHECK_HEADERS(errno.h stdint.h unistd.h errno.h limits.h sys/stat.h sys/types.h sys/socket.h fcntl.h netinet/in.h netdb.h)
AC_CHECK_FUNCS(strdup)
AC_CHECK_DECLS([strnlen], [], [], [#include <string.h>])
AC_HEADER_STDBOOL
AC_C_BIGENDIAN
AC_SYS_LARGEFILE

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

AC_MSG_CHECKING([if the compiler supports __builtin_ctz])
AC_TRY_COMPILE(, [
return ( __builtin_ctz(0x10000u) == 16) ? 1 : 0
], [
have_builtin_ctz=yes
AC_MSG_RESULT([yes])
], [
have_builtin_ctz=no
AC_MSG_RESULT([no])
])
if test x$have_builtin_ctz = xyes ; then
AC_DEFINE([HAVE_BUILTIN_CTZ], [1], [Define to 1 if the compiler support __builtin_ctz.])
fi

# Check for __builtin_expect()
AC_MSG_CHECKING([for __builtin_expect()])
AC_LINK_IFELSE([AC_LANG_PROGRAM(, return __builtin_expect(main != 0, 1))],
[AC_DEFINE(HAVE_BUILTIN_EXPECT, 1,
Define to 1 if compiler supports __builtin_expect)
AC_MSG_RESULT([yes])],
[AC_MSG_RESULT([no])])


AC_SEARCH_LIBS(gethostbyname_r, [socket nsl])
AC_SEARCH_LIBS(getservbyname_r, [socket nsl])

AH_TEMPLATE([HAVE_GETxxxxBYyyyy_R_POSIX], [Define if POSIX-style getXXXXbyYYYY_r functions are available.])
AC_MSG_CHECKING([for POSIX-style gethostbyXXXX_r and getservbyXXXX_r])
AC_TRY_LINK([
#define _POSIX_PTHREAD_SEMANTICS
#include <netdb.h>
#include <stddef.h>
],
[
struct hostent *he;
struct servent *se;
he = gethostbyname_r((const char *)NULL, (struct hostent *)NULL,
(char *)NULL, (int)0, (struct hostent **)NULL, (int *)NULL);
he = gethostbyaddr_r((const char *)NULL, (int)0, (int)0,
(struct hostent *)NULL, (char *)NULL, (int)0, (struct hostent **)NULL,
(int *)NULL);
se = getservbyname_r((const char *)NULL, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0, (struct servent **)NULL);
se = getservbyport_r((int)0, (const char *)NULL,
(struct servent *)NULL, (char *)NULL, (int)0, (struct servent **)NULL);
],
AC_DEFINE(HAVE_GETxxxxBYyyyy_R_POSIX)
AC_MSG_RESULT(yes),
AC_MSG_RESULT(no)
)



AC_OUTPUT
