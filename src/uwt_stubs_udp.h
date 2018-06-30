/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_UDP_H
#define __UWT_STUBS_UDP_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN3(uwt_udp_bind_na);
UWT_EXTERN4(uwt_udp_set_membership_na);
UWT_EXTERN2(uwt_udp_set_multicast_loop_na);
UWT_EXTERN2(uwt_udp_set_multicast_ttl_na);
UWT_EXTERN2(uwt_udp_set_multicast_interface_na);
UWT_EXTERN2(uwt_udp_set_broadcast_na);
UWT_EXTERN2(uwt_udp_set_ttl_na);
UWT_EXTERN2(uwt_udp_recv_start);
UWT_EXTERN2(uwt_udp_recv_stop);
UWT_EXTERN4(uwt_udp_recv_own);
UWT_EXTERN1(uwt_udp_send_queue_size_na);
UWT_EXTERN1(uwt_udp_send_queue_count_na);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_UDP_H */
