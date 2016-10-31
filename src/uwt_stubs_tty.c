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

#include "uwt_stubs_tty.h"

CAMLprim value
uwt_tty_init(value o_loop,value o_fd, value o_readable)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam1(o_loop);
  CAMLlocal1(dc);
  value ret;
  const int fd = FD_VAL(o_fd);
  dc = uwt__handle_create(UV_TTY,l);
  struct handle * h =  Handle_val(dc);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  h->close_executed = 0;
  h->initialized = 1;
  Field(ret,0) = dc;

  const int erg = uv_tty_init(&l->loop,
                              (uv_tty_t*)h->handle,
                              fd,
                              Long_val(o_readable) == 1 );
  if ( erg < 0 ){
    uwt__free_mem_uv_handle_t(h);
    uwt__free_struct_handle(h);
    Field(dc,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_tty_set_mode_na(value o_tty,value o_mode)
{
  HANDLE_NINIT_NA(s,o_tty);
  HANDLE_NO_UNINIT_NA(s);
#if HAVE_DECL_UV_TTY_MODE_IO && HAVE_DECL_UV_TTY_MODE_NORMAL && HAVE_DECL_UV_TTY_MODE_RAW
  int mode ;
  switch ( Long_val(o_mode) ){
  case 2: mode = UV_TTY_MODE_IO; break;
  case 1: mode = UV_TTY_MODE_RAW; break;
  default: assert(false); /* fall */
  case 0: mode = UV_TTY_MODE_NORMAL; break;
  }
#else
  int mode = Long_val(o_mode) == 0 ? 0 : 1;
#endif
  int ret = uv_tty_set_mode((uv_tty_t*)s->handle,mode);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tty_reset_mode_na(value unit)
{
  (void) unit;
  int x = uv_tty_reset_mode();
  return (VAL_UWT_UNIT_RESULT(x));
}

CAMLprim value
uwt_tty_get_winsize(value o_tty)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(o_tty);
  CAMLparam1(o_tty);
  CAMLlocal1(tup);
  struct handle * s = Handle_val(o_tty);
  value ret;
  int width;
  int height;
  const int erg = uv_tty_get_winsize((uv_tty_t*)s->handle,&width,&height);
  if ( erg < 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(erg);
  }
  else {
    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(width);
    Field(tup,1) = Val_long(height);
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = tup;
  }
  CAMLreturn(ret);
}
