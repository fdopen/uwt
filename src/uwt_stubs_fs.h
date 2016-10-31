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

#ifndef __UWT_STUBS_FS_H
#define __UWT_STUBS_FS_H

#include "uwt_stubs_base.h"

#ifdef __cplusplus
extern "C" {
#endif

UWT_EXTERN1(uwt_fs_free);
UWT_EXTERN1(uwt_get_fs_result);
UWT_EXTERN6(uwt_fs_open_native);
UWT_EXTERN_BYTE(uwt_fs_open_byte);
UWT_EXTERN7(uwt_fs_read_native);
UWT_EXTERN_BYTE(uwt_fs_read_byte);
UWT_EXTERN7(uwt_fs_write_native);
UWT_EXTERN_BYTE(uwt_fs_write_byte);
UWT_EXTERN4(uwt_fs_close);
UWT_EXTERN4(uwt_fs_unlink);
UWT_EXTERN5(uwt_fs_mkdir);
UWT_EXTERN4(uwt_fs_rmdir);
UWT_EXTERN5(uwt_fs_rename);
UWT_EXTERN5(uwt_fs_link);
UWT_EXTERN4(uwt_fs_fsync);
UWT_EXTERN4(uwt_fs_fdatasync);
UWT_EXTERN5(uwt_fs_ftruncate);
UWT_EXTERN7(uwt_fs_sendfile_native);
UWT_EXTERN_BYTE(uwt_fs_sendfile_byte);
UWT_EXTERN4(uwt_fs_scandir);
UWT_EXTERN4(uwt_fs_mkdtemp);
UWT_EXTERN4(uwt_fs_readlink);
UWT_EXTERN5(uwt_fs_access);
UWT_EXTERN5(uwt_fs_chmod);
UWT_EXTERN5(uwt_fs_fchmod);
UWT_EXTERN6(uwt_fs_chown_native);
UWT_EXTERN_BYTE(uwt_fs_chown_byte);
UWT_EXTERN6(uwt_fs_fchown_native);
UWT_EXTERN_BYTE(uwt_fs_fchown_byte);
UWT_EXTERN6(uwt_fs_utime_native);
UWT_EXTERN_BYTE(uwt_fs_utime_byte);
UWT_EXTERN6(uwt_fs_futime_native);
UWT_EXTERN_BYTE(uwt_fs_futime_byte);
UWT_EXTERN6(uwt_fs_symlink_native);
UWT_EXTERN_BYTE(uwt_fs_symlink_byte);
UWT_EXTERN4(uwt_fs_stat);
UWT_EXTERN4(uwt_fs_lstat);
UWT_EXTERN4(uwt_fs_fstat);

#if HAVE_DECL_UV_FS_REALPATH
UWT_EXTERN4(uwt_fs_realpath);
#endif

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_FS_H */
