/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_WORKER_H
#define __UWT_STUBS_WORKER_H

#include "uwt_stubs_base.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_workreq_create);
UWT_EXTERN1(uwt_workreq_cancel_na);
UWT_EXTERN5(uwt_lseek);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_WORKER_H */
