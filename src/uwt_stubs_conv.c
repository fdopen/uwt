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

#include "uwt_stubs_conv.h"

CAMLprim value
uwt_set_crtfd_na(value o_fd)
{
#ifndef _WIN32
  (void)o_fd;
  return (Val_long(1));
#else
  struct filedescr * t = (struct filedescr *) Data_custom_val(o_fd);
  if ( t->kind == KIND_SOCKET ){
    return (Val_long(0));
  }
  return (Val_long(uwt__set_crt_fd(o_fd) == true));
#endif
}

CAMLprim value
uwt_fileno(value ohandle)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(ohandle);
  CAMLparam1(ohandle);
  CAMLlocal2(ocont,ofd);
  struct handle * s = Handle_val(ohandle);
  uv_os_fd_t fd;
  const int r = uv_fileno(s->handle,&fd);
  if ( r < 0 ){
    ocont = caml_alloc_small(1,Error_tag);
    Field(ocont,0) = Val_uwt_error(r);
  }
  else {
#ifndef _WIN32
    ofd = Val_long(fd);
#else
    ofd = win_alloc_handle(fd);
    CRT_fd_val(ofd) = s->orig_fd;
#endif
    ocont = caml_alloc_small(1,Ok_tag);
    Field(ocont,0) = ofd;
  }
  CAMLreturn(ocont);
}
