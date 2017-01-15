/* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
 * Copyright (C) 2015-2016 Andreas Hauptmann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

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
UWT_EXTERN5(uwt_read_own);
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
