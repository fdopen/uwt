/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_async.h"

static void
uwt_async_cb(uv_async_t* handle)
{
  HANDLE_CB_START(wp, handle);
  value t = GET_CB_VAL(wp->cb_listen);
  value exn = GET_CB_VAL(wp->cb_read);
  exn = caml_callback_exn(exn,t);
  HANDLE_CB_END(exn);
}

CAMLprim value
uwt_async_create(value o_loop, value o_cb)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam1(o_cb);
  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_ASYNC, false);
  value v = Field(ret,0);
  struct handle *h = Handle_val(v);
  uv_async_t * a = (uv_async_t *)h->handle;
  const int erg = uv_async_init(&l->loop,a,uwt_async_cb);
  if ( erg < 0 ){
    uwt__free_handle(h);
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  else {
    uwt__gr_register(&h->cb_read,o_cb);
    uwt__gr_register(&h->cb_listen,v);
    uv_unref((uv_handle_t*)a);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_async_start_na(value o_async)
{
  HANDLE_INIT_NA(a, o_async);
  uv_ref(a->handle);
  return Val_unit;
}

CAMLprim value
uwt_async_stop_na(value o_async)
{
  HANDLE_INIT_NA(a, o_async);
  uv_unref(a->handle);
  return Val_unit;
}

CAMLprim value
uwt_async_send_na(value o_async)
{
  HANDLE_INIT_NA(a, o_async);
  /* async send doesn't return error codes.
     It only returns `-1`, if the handle is
     closed or not of type UV_ASYNC.
     Both issues should be covered by either
     OCaml's type system or the check above. */
  uv_async_send((uv_async_t*)a->handle);
  return Val_unit;
}
