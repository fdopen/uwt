/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_conv.h"

CAMLprim value
uwt_set_crtfd_na(value o_fd)
{
#ifndef _WIN32
  (void)o_fd;
  return (Val_long(1));
#else
  if ( Descr_kind_val(o_fd) == KIND_SOCKET ){
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
