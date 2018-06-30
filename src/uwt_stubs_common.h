/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_COMMON_H
#define __UWT_STUBS_COMMON_H

#include "config.h"
#include <uv.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#define CAML_NAME_SPACE 1
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#ifdef _WIN32
#include <io.h>
#endif

#include "uwt-worker.h"
#include "macros.h"
#include "uwt-error.h"

#define UWT_EXTERN1(x)                          \
  CAMLprim value x(value)

#define UWT_EXTERN2(x)                          \
  CAMLprim value x(value,value)

#define UWT_EXTERN3(x)                          \
  CAMLprim value x(value,value,value)

#define UWT_EXTERN4(x)                          \
  CAMLprim value x(value,value,value,value)

#define UWT_EXTERN5(x)                            \
  CAMLprim value x(value,value,value,value,value)

#define UWT_EXTERN6(x)                                  \
  CAMLprim value x(value,value,value,value,value,value)

#define UWT_EXTERN7(x)                                        \
  CAMLprim value x(value,value,value,value,value,value,value)

#define UWT_EXTERN8(x)                                              \
  CAMLprim value x(value,value,value,value,value,value,value,value)

#define UWT_EXTERN_BYTE(x)                      \
  CAMLprim value x(value*,int)

#define DEF_ALLOC_SIZE 65536
#define Some_tag 0
#define Ba_buf_val(x)  ((char*)Caml_ba_data_val(x))

#define VAL_UWT_UNIT_RESULT(x)                      \
  ( (x) < 0 ? Val_uwt_int_result(x) : Val_long(0) )

#define VAL_UWT_INT_RESULT(x)                       \
  ( (x) < 0 ? Val_uwt_int_result(x) : Val_long(x) )

#ifndef UWT_LOCAL
#if (defined(__GNUC__) && __GNUC__ >= 4) || defined(__clang__)
#define UWT_LOCAL __attribute__((visibility ("hidden")))
#else
#define UWT_LOCAL
#endif
#endif

#if !defined(_WIN32) && (UV_VERSION_MAJOR == 1)
#define UWT_NO_COPY_READ 1
#endif

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_strerror);

UWT_LOCAL int
uwt__safe_convert_flag_list(value, const int [],size_t );
#define SAFE_CONVERT_FLAG_LIST(a,b)             \
  uwt__safe_convert_flag_list(a,b,AR_SIZE(b))

UWT_LOCAL value
uwt__safe_rev_convert_flag_list(int res, const int flags[],size_t flags_size);
#define SAFE_REV_CONVERT_FLAG_LIST(a,b)           \
  uwt__safe_rev_convert_flag_list(a,b,AR_SIZE(b))

UWT_LOCAL value
uwt__alloc_sockaddr(const struct sockaddr *);

UWT_LOCAL bool
uwt__get_sockaddr(value o_addr,struct sockaddr *saddr);

UWT_LOCAL value
uwt__alloc_eresult(val_uwt_error_t er);

extern int caml_convert_signal_number(int);
extern int caml_rev_convert_signal_number(int);

#ifndef _WIN32
#define uwt__convert_signal_number caml_convert_signal_number
#define uwt__rev_convert_signal_number caml_rev_convert_signal_number
#define FD_VAL(x) (Long_val(x))
#else
/* Windows: libuv's kill will handle SIGTERM, SIGKILL, SIGINT (kill).
   It wil emulate SIGBREAK, SIGHUP, SIGWINCH.
   However, the caml_* functions above can't translate all */
UWT_LOCAL int
uwt__convert_signal_number(int);

UWT_LOCAL int
uwt__rev_convert_signal_number(int);

UWT_LOCAL bool
uwt__set_crt_fd(value o_fd);

#define FD_VAL(x) (CRT_fd_val(x))

#endif

UWT_LOCAL value
uwt__stat_to_value(const uv_stat_t *);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_COMMON_H */
