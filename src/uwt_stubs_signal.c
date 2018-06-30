/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

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
                 value o_cb,
                 value o_oneshot)
{
  const int oneshot = Long_val(o_oneshot);
#if !HAVE_DECL_UV_SIGNAL_START_ONESHOT
  if (oneshot != 0 ){
    return (uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS));
  }
#endif
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
    if (oneshot){
#if HAVE_DECL_UV_SIGNAL_START_ONESHOT
      erg = uv_signal_start_oneshot(t,signal_cb,signum);
#else
      erg = UV_ENOSYS;
#endif
    }
    else {
      erg = uv_signal_start(t,signal_cb,signum);
    }
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
