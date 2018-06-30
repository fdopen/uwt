/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_ASYNC_H
#define __UWT_STUBS_ASYNC_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN2(uwt_async_create);
UWT_EXTERN1(uwt_async_start_na);
UWT_EXTERN1(uwt_async_stop_na);
UWT_EXTERN1(uwt_async_send_na);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_ASYNC_H */
