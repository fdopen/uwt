/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_timer.h"

static void
timer_repeating_cb(uv_timer_t * handle)
{
  HANDLE_CB_START(wp, handle);
  struct loop * l = handle->loop->data;
  l->stop_loop_earlier = 1;
  value t = GET_CB_VAL(wp->cb_listen);
  value exn = GET_CB_VAL(wp->cb_read);
  exn = caml_callback_exn(exn,t);
  HANDLE_CB_END(exn);
}

static void
timer_once_cb(uv_timer_t * handle)
{
  HANDLE_CB_START(wp, handle);
  struct loop * l = handle->loop->data;
  l->stop_loop_earlier = 1;
  value timer = GET_CB_VAL(wp->cb_listen);
  value exn = GET_CB_VAL(wp->cb_read);
  uwt__gr_unregister(&wp->cb_read);
  exn = caml_callback_exn(exn,timer);
  if ( wp->close_called == 0 ){
    timer = GET_CB_VAL(wp->cb_listen); /* might have changed */
    Field(timer,1) = 0;
    uwt__handle_finalize_close(wp);
  }
  uwt__gr_unregister(&wp->cb_listen);
  HANDLE_CB_END(exn);
}

CAMLprim value
uwt_timer_start(value o_loop, value o_cb,
                value o_timeout, value o_repeat)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam1(o_cb);
  const intnat l_timeout = Long_val(o_timeout);
  const intnat l_repeat = Long_val(o_repeat);

  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_TIMER, false);
  value v = Field(ret,0);
  struct handle * h = Handle_val(v);
  int erg = uv_timer_init(&l->loop,(uv_timer_t *)h->handle);
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    erg = uv_timer_start((uv_timer_t*)h->handle,
                         l_repeat != 0 ? timer_repeating_cb : timer_once_cb,
                         l_timeout, l_repeat);
    if ( erg >= 0 ){
      uwt__gr_register(&h->cb_read,o_cb);
      uwt__gr_register(&h->cb_listen,v);
    }
    else {
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
