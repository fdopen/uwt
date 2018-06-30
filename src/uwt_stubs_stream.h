/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_STREAM_H
#define __UWT_STUBS_STREAM_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_is_readable_na);
UWT_EXTERN1(uwt_is_writable_na);
UWT_EXTERN1(uwt_write_queue_size_na);
UWT_EXTERN2(uwt_shutdown);
UWT_EXTERN3(uwt_listen);
UWT_EXTERN1(uwt_accept);
UWT_EXTERN2(uwt_accept_raw_na);
UWT_EXTERN2(uwt_read_start);
UWT_EXTERN2(uwt_read_stop);
UWT_EXTERN4(uwt_read_own);
UWT_EXTERN6(uwt_write_send_native);
UWT_EXTERN_BYTE(uwt_write_send_byte);
UWT_EXTERN6(uwt_write2_native);
UWT_EXTERN_BYTE(uwt_write2_byte);
UWT_EXTERN5(uwt_udp_try_send_na);
UWT_EXTERN4(uwt_try_write_na);
UWT_EXTERN2(uwt_stream_set_blocking_na);
UWT_EXTERN5(uwt_writev);
UWT_EXTERN3(uwt_try_writev_na);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_STREAM_H */
