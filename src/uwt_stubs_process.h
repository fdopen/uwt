/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_PROCESS_H
#define __UWT_STUBS_PROCESS_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_disable_stdio_inheritance_na);
UWT_EXTERN4(uwt_spawn);
UWT_EXTERN1(uwt_pid_na);
UWT_EXTERN2(uwt_process_kill_na);
UWT_EXTERN2(uwt_kill_na);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_PROCESS_H */
