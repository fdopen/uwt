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

#include "uwt_stubs_handle.h"

static void
close_cb(uv_handle_t* handle)
{
  struct handle *s = handle->data;
  if (unlikely( !s || s->cb_close == CB_INVALID )){
    DEBUG_PF("data lost");
  }
  else {
    value exn = Val_unit;
    GET_RUNTIME();
    ++s->in_callback_cnt;
    exn = CAML_CALLBACK1(s,cb_close,Val_unit);
    if (unlikely( Is_exception_result(exn) )){
      uwt__add_exception(s->loop,exn);
    }
    --s->in_callback_cnt;
    uwt__handle_free_common(s);
    s->close_executed = 1;
    if (likely( s->in_callback_cnt == 0 )){
      uwt__free_mem_uv_handle_t(s);
      if ( s->finalize_called ){
        uwt__free_struct_handle(s);
      }
    }
    else {
      DEBUG_PF("close_cb not the last callback?");
    }
  }
}

CAMLprim value
uwt_close_wait(value o_stream,value o_cb)
{
  struct handle * s = Handle_val(o_stream);
  if ( HANDLE_IS_INVALID(s) ){
    return VAL_UWT_INT_RESULT_EBADF;
  }
  if (unlikely( s->cb_close != CB_INVALID )){
    return VAL_UWT_INT_RESULT_EBUSY;
  }
  CAMLparam2(o_stream,o_cb);
  GR_ROOT_ENLARGE();
  ++s->in_use_cnt;
  s->close_called = 1;
  /* This way, we can't wrap uv_is_closing.
     But otherwise we "leak" memory until
     the ocaml grabage collector finalizes
     the handle  */
  Field(o_stream,1) = 0;
  uwt__gr_register(&s->cb_close,o_cb);
  uv_close(s->handle,close_cb);
  s->finalize_called = 1;
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
  if ( s && s->handle && s->close_called == 0 ){
    s->close_called = 1;
    Field(o_stream,1) = 0;
    if ( s->read_waiting ){
      uwt__cancel_reader(s);
    }
    s->finalize_called = 1;
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
  if ( s && s->handle && s->close_called == 0 ){
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
  if ( s && s->handle ){
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
  HANDLE_NINIT_NA(s,o_stream);
  HANDLE_NO_UNINIT_NA(s);
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
  HANDLE_NINIT_NA(s,o_stream);
  HANDLE_NO_UNINIT_NA(s);
  int ret;
  INT_VAL_RET_IR_EINVAL(x,o_len);
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
  if ( !handle || !handle->data ){
    DEBUG_PF("no data");
    buf->base = NULL;
    buf->len = 0;
  }
  else {
    const struct handle * h = handle->data;
    const size_t len = UMIN(suggested_size,h->c_read_size);
    uwt__malloc_uv_buf_t(buf,len,h->cb_type);
  }
}

UWT_LOCAL void
uwt__alloc_own_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf)
{
  struct handle * h;
  (void) suggested_size;
  if (unlikely( !handle || (h = handle->data) == NULL )){
    DEBUG_PF("no data");
    buf->len = 0;
    buf->base = NULL;
  }
  else if ( h->use_read_ba ){
    buf->base = h->ba_read;
    buf->len = h->c_read_size;
  }
  else {
#ifdef UWT_NO_COPY_READ
    GET_RUNTIME();
    value tp = Field(GET_CB_VAL(h->cb_read),0);
    buf->base = String_val(tp) + h->obuf_offset;
    buf->len = h->c_read_size;
#else
    size_t len;
    len = UMIN(suggested_size,h->c_read_size);
    uwt__malloc_uv_buf_t(buf,len,h->cb_type);
#endif
  }
}

UWT_LOCAL void
uwt__pipe_tcp_connect_cb(uv_connect_t* req, int status)
{
  struct handle * s = req->handle->data;
  struct req * r = req->data;
  if ( !s || !r ){
    DEBUG_PF("leaking data");
  }
  else {
    ++s->in_callback_cnt;
    --s->in_use_cnt;
    r->c_param = status;
    if ( status >= 0 ){
      s->initialized = 1;
    }
    uwt__req_callback((void*)req);
    --s->in_callback_cnt;
    CLOSE_HANDLE_IF_UNREFERENCED(s);
  }
}

CAMLprim value
uwt_handle_type_na(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  if ( s && s->handle && s->close_called == 0 ){
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
