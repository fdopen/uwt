/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_fs_event.h"

static void
event_cb(uv_fs_event_t* handle,
         const char* filename,
         int events,
         int status)
{
  HANDLE_CB_START(h, handle);
  value param;
  if ( status < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(status);
  }
  else {
    value list = Val_unit;
    value str = Val_unit;
    value tup = Val_unit;
    Begin_roots3(list,str,tup);
    if ( events & UV_RENAME ){
      tup = caml_alloc_small(2,0);
      Field(tup,0) = Val_long(0);
      Field(tup,1) = list;
      list = tup;
    }
    if ( events & UV_CHANGE ){
      tup = caml_alloc_small(2,0);
      Field(tup,0) = Val_long(1);
      Field(tup,1) = list;
      list = tup;
    }
    str = s_caml_copy_string(filename);
    tup = caml_alloc_small(2,0);
    Field(tup,0) = str;
    Field(tup,1) = list;
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = tup;
    End_roots();
  }
  value cb = GET_CB_VAL(h->cb_read);
  value t = GET_CB_VAL(h->cb_listen);
  param = caml_callback2_exn(cb,t,param);
  HANDLE_CB_END(param);
}

static const int fs_event_flags[3] = {
  UV_FS_EVENT_WATCH_ENTRY,
  UV_FS_EVENT_STAT,
  UV_FS_EVENT_RECURSIVE };

CAMLprim value
uwt_fs_event_start(value o_loop,
                   value o_path,
                   value o_flags,
                   value o_cb)
{
  if (unlikely( !uwt_is_safe_string(o_path) )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ECHARSET);
  }
  if (unlikely( String_val(o_path)[0] == '\0' )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);
  }
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam2(o_path,o_cb);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_flags,fs_event_flags);
  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_FS_EVENT, false);
  value v = Field(ret,0);
  struct handle * h = Handle_val(v);
  uv_fs_event_t * f = (uv_fs_event_t*)h->handle;
  int erg = uv_fs_event_init(&l->loop,f);
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    erg = uv_fs_event_start(f,event_cb,String_val(o_path),flags);
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
