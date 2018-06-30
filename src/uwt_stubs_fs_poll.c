/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_fs_poll.h"

static void
fs_poll_cb(uv_fs_poll_t* handle,
           int status,
           const uv_stat_t* prev,
           const uv_stat_t* curr)
{
  HANDLE_CB_START(h, handle);
  value param;
  if ( status < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(status);
  }
  else {
    value s2 = Val_unit;
    value tup = Val_unit;
    value s1 = uwt__stat_to_value(prev);
    Begin_roots3(tup,s1,s2);
    s2 = uwt__stat_to_value(curr);
    tup = caml_alloc_small(2,0);
    Field(tup,0) = s1;
    Field(tup,1) = s2;
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = tup;
    End_roots();
  }
  value cb = GET_CB_VAL(h->cb_read);
  value t = GET_CB_VAL(h->cb_listen);
  param = caml_callback2_exn(cb,t,param);
  HANDLE_CB_END(param);
}

CAMLprim value
uwt_fs_poll_start(value o_loop,
                  value o_path,
                  value o_interval,
                  value o_cb)
{
  if (unlikely( !uwt_is_safe_string(o_path) )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ECHARSET);
  }
  if (unlikely( String_val(o_path)[0] == '\0' )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);
  }
  UINT_VAL_RET_WRAP_EINVAL(interval, o_interval);
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam2(o_path,o_cb);
  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_FS_POLL, false);
  value v = Field(ret,0);
  struct handle * h = Handle_val(v);
  uv_fs_poll_t * f = (uv_fs_poll_t*)h->handle;
  int erg = uv_fs_poll_init(&l->loop,f);
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    erg = uv_fs_poll_start(f, fs_poll_cb, String_val(o_path), interval);
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
    Tag_val(ret) = Error_tag;
    Field(ret,0) = Val_uwt_error(erg);
  }
  CAMLreturn(ret);
}
