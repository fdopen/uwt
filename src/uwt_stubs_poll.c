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

#include "uwt_stubs_poll.h"

static const int poll_flag_table[3] = {
  UV_READABLE, UV_WRITABLE,
#if HAVE_DECL_UV_DISCONNECT
  UV_DISCONNECT
#else
  4
#endif
};

static void
poll_cb(uv_poll_t* handle, int status, int events)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if (unlikely( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID )){
    DEBUG_PF("callback lost");
  }
  else {
    int tag = Ok_tag;
    value val = Val_unit;
    value param = Val_unit;
    Begin_roots2(val,param);
    if ( status < 0 ){
      tag = Error_tag;
      val = Val_uwt_error(status);
    }
    else {
      val = SAFE_REV_CONVERT_FLAG_LIST(events,poll_flag_table);
      if ( val == Val_unit ){
        tag = Error_tag;
        val = VAL_UWT_ERROR_UWT_EFATAL;
      }
    }
    param = caml_alloc_small(1,tag);
    Field(param,0) = val;
    End_roots();
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,param);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_poll_start(value o_loop,
               value o_sock_or_fd,
               value o_event,
               value o_cb)
{
  INIT_LOOP_RESULT(l,o_loop);
#ifdef _WIN32
  if ( Descr_kind_val(o_sock_or_fd) != KIND_SOCKET ){
    value ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_EINVAL;
    return ret;
  }
#endif
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  ret = Val_unit;
#ifdef _WIN32
  const uv_os_sock_t sock = Socket_val(o_sock_or_fd);
  const int orig_fd = CRT_fd_val(o_sock_or_fd);
#endif
  const int event = SAFE_CONVERT_FLAG_LIST(o_event,poll_flag_table);
#if !HAVE_DECL_UV_DISCONNECT
  if ( event & 4 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_ENOSYS;
    goto endp;
  }
#endif
  GR_ROOT_ENLARGE();
  v = uwt__handle_create(UV_POLL,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = v;
  h->close_executed = 0;
  uv_poll_t * p = (uv_poll_t*)h->handle;
#ifdef _WIN32
  int erg = uv_poll_init_socket(&l->loop, p, sock);
  h->orig_fd = orig_fd;
#else
  int erg = uv_poll_init(&l->loop, p, Long_val(o_sock_or_fd));
#endif
  if ( erg < 0 ){
    uwt__free_mem_uv_handle_t(h);
    uwt__free_struct_handle(h);
  }
  else {
    erg = uv_poll_start(p,event,poll_cb);
    if ( erg < 0 ){
      h->finalize_called = 1;
      uwt__handle_finalize_close(h);
    }
    else {
      h->initialized = 1;
      ++h->in_use_cnt;
      uwt__gr_register(&h->cb_read,o_cb);
      uwt__gr_register(&h->cb_listen,v);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
#if !HAVE_DECL_UV_DISCONNECT
endp:
#endif
  CAMLreturn(ret);
}
