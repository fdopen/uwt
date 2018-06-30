/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_TTY_H
#define __UWT_STUBS_TTY_H

#include "uwt_stubs_handle.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN3(uwt_tty_init);
UWT_EXTERN2(uwt_tty_set_mode_na);
UWT_EXTERN1(uwt_tty_reset_mode_na);
UWT_EXTERN1(uwt_tty_get_winsize);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_TTY_H */
