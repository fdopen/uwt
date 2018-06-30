/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_handle.h"

static void
close_cb(uv_handle_t* handle)
{
  GET_RUNTIME();
  struct handle *s = handle->data;
  value exn = GET_CB_VAL(s->cb_close);
  uwt__handle_unreg_camlval(s);
  uwt__free_handle(s);
  exn = caml_callback2_exn(*uwt__global_wakeup, exn, Val_unit);
  if (unlikely( Is_exception_result(exn) )){
    caml_callback_exn(*uwt_global_exception_fun, Extract_exception(exn));
  }
}

CAMLprim value
uwt_close_wait(value o_stream,value o_cb)
{
  struct handle * s = Handle_val(o_stream);
  if ( HANDLE_IS_INVALID(s) ){
    return VAL_UWT_INT_RESULT_EBADF;
  }
  CAMLparam2(o_stream,o_cb);
  GR_ROOT_ENLARGE();
  s->finalize_called = 1;
  s->close_called = 1;
  Field(o_stream,1) = 0;
  uwt__gr_register(&s->cb_close,o_cb);
  uv_close(s->handle,close_cb);
  if ( s->read_waiting ){
    uwt__cancel_reader(s);
  }
  CAMLreturn(Val_long(0));
}

CAMLprim value
uwt_close_nowait(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  value ret = VAL_UWT_INT_RESULT_EBADF;
  if ( s && s->close_called == 0 ){
    Field(o_stream,1) = 0;
    /* possibly cancels reader. noalloc impossible */
    uwt__handle_finalize_close(s);
    ret = Val_unit;
  }
  return ret;
}

/*
 the sole purpose of uwt_close__1 and uwt_close__2 is to produce
 garbage descriptors for the test suite
*/
CAMLprim value
uwt_close__1(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  value ret = VAL_UWT_INT_RESULT_EBADF;
  if ( s && s->close_called == 0 ){
    uv_close(s->handle, uwt__handle_finalize_close_cb);
    ret = Val_unit;
  }
  return ret;
}

CAMLprim value
uwt_close__2(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  value ret = VAL_UWT_INT_RESULT_EBADF;
  if ( s ){
    Field(o_stream,1) = 0;
    s->finalize_called = 1;
    s->close_called = 1;
    ret = Val_unit;
  }
  return ret;
}

UV_HANDLE_BOOL(uv_handle_t,is_active,true)
UV_HANDLE_BOOL(uv_handle_t,has_ref,true)
UV_HANDLE_VOID(ref)
UV_HANDLE_VOID(unref)

/* {{{ Handle ext start */
CAMLprim value
uwt_get_buffer_size_common_na(value o_stream, value o)
{
  HANDLE_INIT_NOUNINIT_NA(s, o_stream);
  int ret;
  int x = 0;
  if ( Long_val(o) == 0 ){
    ret = uv_send_buffer_size(s->handle,&x);
  }
  else {
    ret = uv_recv_buffer_size(s->handle,&x);
  }
  return (VAL_UWT_INT_RESULT(ret == 0 ? x : ret));
}

CAMLprim value
uwt_set_buffer_size_common_na(value o_stream, value o_len, value o)
{
  HANDLE_INIT_NOUNINIT_NA(s, o_stream);
  int ret;
  const intnat len = Long_val(o_len);
  if ( len <= 0 || len > INT_MAX ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  int x = len;
  if ( Long_val(o) == 0 ){
    ret = uv_send_buffer_size(s->handle,&x);
  }
  else {
    ret = uv_recv_buffer_size(s->handle,&x);
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}
/* }}} Handle ext end */

UWT_LOCAL void
uwt__alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf)
{
  const struct handle * h = handle->data;
  const size_t len = UMIN(suggested_size,h->c_read_size);
  uwt__malloc_uv_buf_t(buf,len);
}

UWT_LOCAL void
uwt__alloc_own_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf)
{
  const struct handle * h = handle->data;
  (void) suggested_size;
  if ( h->use_read_ba ){
    buf->base = h->x.ba_read;
    buf->len = h->c_read_size;
  }
  else {
#ifdef UWT_NO_COPY_READ
    GET_RUNTIME();
    value tp = Field(GET_CB_VAL(h->cb_read),0);
    buf->base = String_val(tp) + h->x.obuf_offset;
    buf->len = h->c_read_size;
#else
    size_t len;
    len = UMIN(suggested_size,h->c_read_size);
    uwt__malloc_uv_buf_t(buf,len);
#endif
  }
}

UWT_LOCAL void
uwt__pipe_tcp_connect_cb(uv_connect_t* req, int status)
{
  REQ_CB_INIT(req);
  struct handle * s = req->handle->data;
  if ( status >= 0 ){
    s->initialized = 1;
  }
  REQ_CB_CALL(VAL_UWT_UNIT_RESULT(status));
  --s->in_use_cnt;
  CLOSE_HANDLE_IF_UNREFERENCED(s);
}

CAMLprim value
uwt_handle_type_na(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  if ( s && s->close_called == 0 ){
    switch (s->handle->type){
    case UV_FILE: return (Val_long(0));
    case UV_TTY: return (Val_long(1));
    case UV_NAMED_PIPE: return (Val_long(2));
    case UV_TCP: return (Val_long(3));
    case UV_UDP: return (Val_long(4));
    default: /* fall */
    case UV_UNKNOWN_HANDLE:
      return (Val_long(5));
    }
  }
  return Val_long(5);
}
