/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UV_BACK_ERRNO_H
#define __UV_BACK_ERRNO_H

#include <uv.h>
#include <errno.h>

/* uwt currently supports libuv 1.8 and later, not only the bundled
   libuv version. */

#if EDOM > 0
#define UWT__ERR(x) (-(x))
#else
#define UWT__ERR(x) (x)
#endif

#define UWT_DEFINED_UV_UWT_EFATAL 1

#if !defined(UV_EREMOTEIO) /* 1.14.0 */
#define UWT_DEFINED_UV_EREMOTEIO 1
#if defined(EREMOTEIO) && !defined(_WIN32)
#define UV_EREMOTEIO UWT__ERR(EREMOTEIO)
#else
#define UV_EREMOTEIO (-4030)
#endif
#endif

#if !defined(UV_ENOTTY) /* 1.16.0 */
#define UWT_DEFINED_UV_ENOTTY 1
#if defined(ENOTTY) && !defined(_WIN32)
#define UV_ENOTTY UWT__ERR(ENOTTY)
#else
#define UV_ENOTTY (-4029)
#endif
#endif

#if !defined(UV_EFTYPE) /* 1.21.0 */
#define UWT_DEFINED_UV_EFTYPE 1
#if defined(EFTYPE) && !defined(_WIN32)
#define UV_EFTYPE UWT__ERR(EFTYPE)
#else
#define UV_EFTYPE (-4028)
#endif
#endif

#endif /* __UV_BACK_ERRNO_H */
