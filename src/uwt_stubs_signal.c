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
  HANDLE_CB_START(h, handle);
  const int x = uwt__rev_convert_signal_number(signum);
  value cb = GET_CB_VAL(h->cb_read);
  value t = GET_CB_VAL(h->cb_listen);
  t = caml_callback2_exn(cb,t,Val_long(x));
  HANDLE_CB_END(t);
}

CAMLprim value
uwt_signal_start(value o_loop,
                 value o_sig,
                 value o_cb)
{
  INT_VAL_RET_WRAP_EINVAL(sig, o_sig);
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam1(o_cb);
  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_SIGNAL, false);
  value v = Field(ret,0);
  struct handle * h = Handle_val(v);
  uv_signal_t * t = (uv_signal_t *)h->handle;
  int erg = uv_signal_init(&l->loop,t);
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    int signum = uwt__convert_signal_number(sig);
    erg = uv_signal_start(t,signal_cb,signum);
    if ( erg < 0 ){
      uwt__handle_finalize_close(h);
    }
    else {
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
