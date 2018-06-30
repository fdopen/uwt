/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_PIPE_H
#define __UWT_STUBS_PIPE_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN3(uwt_pipe_open);
UWT_EXTERN2(uwt_pipe_init);
UWT_EXTERN2(uwt_pipe_bind_na);
UWT_EXTERN1(uwt_pipe_getsockname);
UWT_EXTERN1(uwt_pipe_getpeername);
UWT_EXTERN2(uwt_pipe_pending_instances_na);
UWT_EXTERN1(uwt_pipe_pending_count_na);
UWT_EXTERN1(uwt_pipe_pending_type_na);
UWT_EXTERN3(uwt_pipe_connect);
UWT_EXTERN2(uwt_pipe_chmod);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_PIPE_H */
