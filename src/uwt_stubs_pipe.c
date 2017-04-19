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

#include "uwt_stubs_pipe.h"

#define FD_INVALID 2
CAMLprim value
uwt_pipe_open(value o_loop, value o_fd,value o_ipc)
{
  INIT_LOOP_RESULT(l,o_loop);
#ifdef _WIN32
  if ( o_fd != FD_INVALID ) {
    if ( Descr_kind_val(o_fd) == KIND_SOCKET ){
      value ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_EINVAL;
      return ret;
    }
    else {
      if ( uwt__set_crt_fd(o_fd) == false ){
        value ret = caml_alloc_small(1,Error_tag);
        Field(ret,0) = VAL_UWT_ERROR_EBADF;
        return ret;
      }
    }
  }
#endif
  CAMLparam1(o_loop);
  CAMLlocal1(dc);
  value ret;
  int erg;
  int fd = -1;
  if ( o_fd != FD_INVALID ){
    fd = FD_VAL(o_fd);
  }
  dc = uwt__handle_create(UV_NAMED_PIPE,l);
  struct handle * h = Handle_val(dc);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = dc;
  h->close_executed = 0;
  uv_pipe_t * p = (uv_pipe_t*)h->handle;
  erg = uv_pipe_init(&l->loop,
                     p,
                     Long_val(o_ipc) == 1);
  if ( erg < 0 ){
    uwt__free_mem_uv_handle_t(h);
    uwt__free_struct_handle(h);
  }
  else {
    if ( o_fd != FD_INVALID ){
#ifdef _WIN32
      h->orig_fd = fd;
#endif
      h->initialized = 1;
      erg = uv_pipe_open(p,fd);
      if ( erg < 0 ){
        h->finalize_called = 1;
        uwt__handle_finalize_close(h);
      }
    }
  }
  if ( erg < 0 ){
    Field(dc,1) = 0;
    Tag_val(ret) = Error_tag;
    Field(ret,0) = Val_uwt_error(erg);
  }
  CAMLreturn(ret);
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
  HANDLE_NINIT_NA(p,o_pipe);
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
  int etag;
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
#if (UV_VERSION_MAJOR == 1) && (UV_VERSION_MINOR < 3)
    --s;
#endif
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
#if HAVE_DECL_UV_PIPE_GETPEERNAME
  return (uwt_pipe_name(o_pipe,uv_pipe_getpeername));
#else
  (void) o_pipe;
  return (uwt__result_enosys());
#endif
}

CAMLprim value
uwt_pipe_pending_instances_na(value o_pipe, value o_count)
{
  HANDLE_NINIT_NA(op,o_pipe);
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  INT_VAL_RET_IR_EINVAL(count, o_count);
  uv_pipe_pending_instances(p, count);
  return (Val_long(0));
}

CAMLprim value
uwt_pipe_pending_count_na(value o_pipe)
{
  HANDLE_NINIT_NA(op,o_pipe);
  HANDLE_NO_UNINIT_NA(op);
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
  struct req * wp = uwt__req_create(UV_CONNECT,s->loop);
  uv_pipe_t* tpipe = (uv_pipe_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  /* pipe_connect is void in order to mimic the windows api,
     the string won't be used internally, we don't need to create a copy */
  uv_pipe_connect(req,tpipe,String_val(o_path),uwt__pipe_tcp_connect_cb);
  wp->c_cb = uwt__ret_unit_cparam;
  uwt__gr_register(&wp->cb,o_cb);
  wp->in_use = 1;
  wp->finalize_called = 1;
  ++s->in_use_cnt;
  CAMLreturn(Val_long(0));
}
