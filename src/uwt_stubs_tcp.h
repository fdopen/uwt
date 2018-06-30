/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_TCP_H
#define __UWT_STUBS_TCP_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_tcp_init);
UWT_EXTERN1(uwt_udp_init);
UWT_EXTERN2(uwt_tcp_init_ex);
UWT_EXTERN2(uwt_udp_init_ex);
UWT_EXTERN2(uwt_tcp_open_na);
UWT_EXTERN2(uwt_udp_open_na);
UWT_EXTERN3(uwt_tcp_bind_na);
UWT_EXTERN2(uwt_tcp_nodelay_na);
UWT_EXTERN3(uwt_tcp_keepalive_na);
UWT_EXTERN2(uwt_tcp_simultaneous_accepts_na);
UWT_EXTERN1(uwt_tcp_getsockname);
UWT_EXTERN1(uwt_tcp_getpeername);
UWT_EXTERN1(uwt_udp_getsockname);
UWT_EXTERN3(uwt_tcp_connect);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_TCP_H */
