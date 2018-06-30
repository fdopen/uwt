/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_TIMER_H
#define __UWT_STUBS_TIMER_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

CAMLprim value uwt_timer_start(value, value , value, value);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_TIMER_H */
