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

#include "uwt_stubs_timer.h"

static void
timer_repeating_cb(uv_timer_t * handle)
{
  HANDLE_CB_INIT(handle);
  value exn = Val_unit;
  struct handle * wp = handle->data;
  if (unlikely( wp->cb_read == CB_INVALID ||
                wp->cb_listen == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value t = GET_CB_VAL(wp->cb_listen);
    exn = GET_CB_VAL(wp->cb_read);
    exn = caml_callback_exn(exn,t);
  }
  HANDLE_CB_RET(exn);
}

static void
timer_once_cb(uv_timer_t * handle)
{
  HANDLE_CB_INIT(handle);
  value exn = Val_unit;
  struct handle * wp = handle->data;
  if (unlikely( wp->cb_read == CB_INVALID || wp->cb_listen == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value timer = GET_CB_VAL(wp->cb_listen);
    exn = GET_CB_VAL(wp->cb_read);
    uwt__gr_unregister(&wp->cb_read);
    if ( wp->in_use_cnt ){
      wp->in_use_cnt--;
    }
    exn = caml_callback_exn(exn,timer);
    if ( wp->close_called == 0 ){
      if ( wp->cb_listen != CB_INVALID ){
        timer = GET_CB_VAL(wp->cb_listen); /* might have changed */
        Field(timer,1) = 0;
      }
      wp->finalize_called = 1;
      uwt__handle_finalize_close(wp);
    }
    uwt__gr_unregister(&wp->cb_listen);
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_timer_start(value o_loop, value o_cb,
                value o_timeout, value o_repeat)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  const intnat l_timeout = Long_val(o_timeout);
  const intnat l_repeat = Long_val(o_repeat);

  GR_ROOT_ENLARGE();
  v = uwt__handle_create(UV_TIMER,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  h->close_executed = 0;
  Field(ret,0) = v;
  int erg = uv_timer_init(&l->loop,(uv_timer_t *)h->handle);
  if ( erg < 0 ){
    uwt__free_mem_uv_handle_t(h);
    uwt__free_struct_handle(h);
  }
  else {
    erg = uv_timer_start((uv_timer_t*)h->handle,
                         l_repeat != 0 ? timer_repeating_cb : timer_once_cb,
                         l_timeout,
                         l_repeat);
    if ( erg >= 0 ){
      h->in_use_cnt++;
      h->initialized = 1;
      uwt__gr_register(&h->cb_read,o_cb);
      uwt__gr_register(&h->cb_listen,v);
    }
    else {
      h->finalize_called = 1;
      uwt__handle_finalize_close(h);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}
