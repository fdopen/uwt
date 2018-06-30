/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_pipe.h"

#define FD_INVALID 2
CAMLprim value
uwt_pipe_open(value o_loop, value o_fd,value o_ipc)
{
  INIT_LOOP_RESULT(l,o_loop);
  int fd = -1;
  if ( o_fd != FD_INVALID ) {
    fd = FD_VAL(o_fd);
#ifdef _WIN32
    if ( Descr_kind_val(o_fd) == KIND_SOCKET ){
      return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);
    }
    if ( fd == NO_CRT_FD ){
      if ( uwt__set_crt_fd(o_fd) == false ){
        return uwt__alloc_eresult(VAL_UWT_ERROR_EBADF);
      }
      fd = FD_VAL(o_fd);
    }
#endif
  }
  value ret = uwt__handle_res_create(UV_NAMED_PIPE, true);
  value dc = Field(ret,0);
  struct handle * h = Handle_val(dc);
  uv_pipe_t * p = (uv_pipe_t*)h->handle;
  int erg = uv_pipe_init(&l->loop, p, Long_val(o_ipc) == 1);
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    if ( o_fd != FD_INVALID ){
#ifdef _WIN32
      h->orig_fd = fd;
#endif
      h->initialized = 1;
      erg = uv_pipe_open(p,fd);
      if ( erg < 0 ){
        uwt__handle_finalize_close(h);
      }
    }
  }
  if ( erg < 0 ){
    Field(dc,1) = 0;
    Tag_val(ret) = Error_tag;
    Field(ret,0) = Val_uwt_error(erg);
  }
  return ret;
}

CAMLprim value
uwt_pipe_init(value o_loop, value o_ipc)
{
  _Static_assert( Is_long(FD_INVALID) == 0 , "int representation has changed");
  return (uwt_pipe_open(o_loop,FD_INVALID,o_ipc));
}
#undef FD_INVALID

CAMLprim value
uwt_pipe_bind_na(value o_pipe, value o_name)
{
  HANDLE_INIT_NA(p, o_pipe);
  if ( !uwt_is_safe_string(o_name) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  /* really don't support abstract sockets. Empty string not caputured
     by uwt_is_safe_string. */
  if ( (String_val(o_name))[0] == '\0' ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  /* string duplicated by libuv */
  int ret = uv_pipe_bind((uv_pipe_t*)p->handle,String_val(o_name));
  if ( ret >= 0 ){
    p->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

typedef int(*pipe_name)(const uv_pipe_t*, char*, size_t*);
static value
uwt_pipe_name(value o_pipe,pipe_name pfunc)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(o_pipe);
  CAMLparam1(o_pipe);
  CAMLlocal1(o_str);
  enum { init_size = 2048 };
  struct handle * op = Handle_val(o_pipe);
  size_t s = init_size;
  char name[init_size];
  o_str = Val_unit;
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  int erg = pfunc(p,name,&s);
  uint8_t etag;
  if ( erg == UV_ENOBUFS ){
    ++s;
    o_str = caml_alloc_string(s);
    erg = pfunc(p,String_val(o_str),&s);
  }
  if ( erg < 0 ){
    o_str = Val_uwt_error(erg);
    etag = Error_tag;
  }
  else {
    etag = Ok_tag;
    if ( o_str != Val_unit ){
      value tmp = caml_alloc_string(s);
      memcpy(String_val(tmp),String_val(o_str),s);
      o_str = tmp;
    }
    else {
      o_str =  caml_alloc_string(s);
      memcpy(String_val(o_str),name,s);
    }
  }
  value ret = caml_alloc_small(1,etag);
  Field(ret,0) = o_str;
  CAMLreturn(ret);
}

CAMLprim value
uwt_pipe_getsockname(value o_pipe)
{
  return(uwt_pipe_name(o_pipe,uv_pipe_getsockname));
}

CAMLprim value
uwt_pipe_getpeername(value o_pipe)
{
  return (uwt_pipe_name(o_pipe,uv_pipe_getpeername));
}

CAMLprim value
uwt_pipe_pending_instances_na(value o_pipe, value o_count)
{
  HANDLE_INIT_NA(op, o_pipe);
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  INT_VAL_RET_IR_EINVAL(count, o_count);
  uv_pipe_pending_instances(p, count);
  return (Val_long(0));
}

CAMLprim value
uwt_pipe_pending_count_na(value o_pipe)
{
  HANDLE_INIT_NOUNINIT_NA(op, o_pipe);
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  int ret = uv_pipe_pending_count(p);
  return (VAL_UWT_INT_RESULT(ret));
}

CAMLprim value
uwt_pipe_pending_type_na(value o_pipe)
{
  struct handle * h = Handle_val(o_pipe);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    return (Val_long(0));
  }
  uv_handle_type x = uv_pipe_pending_type((uv_pipe_t*)h->handle);
  switch( x ){
  case UV_UNKNOWN_HANDLE: /*fall */
  default: return (Val_long(0));
  case UV_TCP: return (Val_long(1));
  case UV_UDP: return (Val_long(2));
  case UV_NAMED_PIPE: return (Val_long(3));
  }
}

CAMLprim value
uwt_pipe_connect(value o_pipe,value o_path,value o_cb)
{
  /* linux: abstract sockets are currently not supported by libuv.
     Therefore the test uwt_is_safe_string is correct.  Remember to
     check it again with, if a new major versions of libuv is released */
  if ( !uwt_is_safe_string(o_path) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  if ( String_val(o_path)[0] == '\0' ){
    /* because libuv's broken handling of empty strings on linux */
    return VAL_UWT_INT_RESULT_ENOENT;
  }
  HANDLE_INIT3(s,o_pipe,o_cb,o_path);
  struct req * wp = uwt__req_create(UV_CONNECT);
  uv_pipe_t* tpipe = (uv_pipe_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  /* pipe_connect is void in order to mimic the windows api,
     the string won't be used internally, we don't need to create a copy */
  uv_pipe_connect(req,tpipe,String_val(o_path),uwt__pipe_tcp_connect_cb);
  uwt__gr_register(&wp->cb,o_cb);
  ++s->in_use_cnt;
  CAMLreturn(Val_long(0));
}

CAMLprim value
uwt_pipe_chmod(value o_pipe, value o_flags)
{
#if HAVE_DECL_UV_PIPE_CHMOD
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_pipe);
  CAMLparam1(o_pipe);
  struct handle * op = Handle_val(o_pipe);
  int res;
  switch ( Long_val(o_flags) ){
  case 0: res = UV_READABLE; break;
  case 1: res = UV_WRITABLE; break;
  default:
    res = UV_WRITABLE | UV_READABLE;
  }
  caml_enter_blocking_section();
  res = uv_pipe_chmod((uv_pipe_t*)op->handle, res);
  caml_leave_blocking_section();
  CAMLreturn(VAL_UWT_UNIT_RESULT(res));
#else
  (void) o_pipe;
  (void) o_flags;
  return VAL_UWT_INT_RESULT_ENOSYS;
#endif
}
