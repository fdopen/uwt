/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_STUBS_FS_H
#define __UWT_STUBS_FS_H

#include "uwt_stubs_base.h"

#ifdef __cplusplus
extern "C" {
#endif

#define X7(n)                                   \
  UWT_EXTERN7(uwt_fs_ ## n ## _native);         \
  UWT_EXTERN_BYTE(uwt_fs_ ## n ## _byte);       \
  UWT_EXTERN5(uwt_fs_ ## n ## _sync)

#define X6(n)                                   \
  UWT_EXTERN6(uwt_fs_ ## n ## _native);         \
  UWT_EXTERN_BYTE(uwt_fs_ ## n ## _byte);       \
  UWT_EXTERN4(uwt_fs_ ## n ## _sync)

#define X5(n)                                   \
  UWT_EXTERN5(uwt_fs_ ## n );                   \
  UWT_EXTERN3(uwt_fs_ ## n ## _sync)

#define X4(n)                                   \
  UWT_EXTERN4(uwt_fs_ ## n );                   \
  UWT_EXTERN2(uwt_fs_ ## n ## _sync)

#define X3(n)                                   \
  UWT_EXTERN3(uwt_fs_ ## n );                   \
  UWT_EXTERN1(uwt_fs_ ## n ## _sync)

X5(open);
X7(read);
X7(write);

X3(close);
X3(unlink);
X4(mkdir);
X3(rmdir);
X4(rename);
X4(link);
X3(fsync);
X3(fdatasync);
X4(ftruncate);
X6(sendfile);
X3(scandir);
X3(mkdtemp);
X3(readlink);
X4(access);
X4(chmod);
X4(fchmod);
X5(chown);
X5(fchown);
X5(lchown);
X5(utime);
X5(futime);
X5(symlink);
X3(stat);
X3(lstat);
X3(fstat);
X6(writev);
X3(realpath);
X5(copyfile);

#undef X7
#undef X6
#undef X5
#undef X4
#undef X3

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_FS_H */
