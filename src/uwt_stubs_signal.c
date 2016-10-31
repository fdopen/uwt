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

#include "uwt_stubs_signal.h"

static void
signal_cb(uv_signal_t* handle, int signum)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if ( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID ){
    DEBUG_PF("callback lost");
  }
  else {
    const int x = uwt__rev_convert_signal_number(signum);
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,Val_long(x));
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_signal_start(value o_loop,
                 value o_sig,
                 value o_cb)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  uv_signal_t * t;
  GR_ROOT_ENLARGE();
  v = uwt__handle_create(UV_SIGNAL,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = v;
  h->close_executed = 0;
  t = (uv_signal_t *)h->handle;
  int erg = uv_signal_init(&l->loop,t);
  if ( erg < 0 ){
    uwt__free_mem_uv_handle_t(h);
    uwt__free_struct_handle(h);
  }
  else {
    int signum = uwt__convert_signal_number(Long_val(o_sig));
    erg = uv_signal_start(t,signal_cb,signum);
    if ( erg < 0 ){
      h->finalize_called = 1;
      uwt__handle_finalize_close(h);
    }
    else {
      ++h->in_use_cnt;
      h->initialized = 1;
      uwt__gr_register(&h->cb_read,o_cb);
      uwt__gr_register(&h->cb_listen,v);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}
