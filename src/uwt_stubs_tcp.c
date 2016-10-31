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

#include "uwt_stubs_tcp.h"

#define UDP_TCP_INIT(name,TYPE)                                         \
  CAMLprim value                                                        \
  uwt_ ## name ## _init(value o_loop)                                   \
  {                                                                     \
    INIT_LOOP_RESULT(l,o_loop);                                         \
    CAMLparam1(o_loop);                                                 \
    CAMLlocal1(v);                                                      \
    v = uwt__handle_create(TYPE,l);                                     \
    struct handle * h = Handle_val(v);                                  \
    h->close_executed = 1;                                              \
    value ret = caml_alloc_small(1,Ok_tag);                             \
    Field(ret,0) = v;                                                   \
    h->close_executed = 0;                                              \
    const int erg = uv_ ## name ## _init(&l->loop,                      \
                                         (uv_ ## name ## _t*)h->handle); \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      uwt__free_mem_uv_handle_t(h);                                     \
      uwt__free_struct_handle(h);                                       \
    }                                                                   \
    CAMLreturn(ret);                                                    \
  }

UDP_TCP_INIT(tcp,UV_TCP)
UDP_TCP_INIT(udp,UV_UDP)
#undef UDP_TCP_INIT

#if HAVE_DECL_UV_UDP_INIT_EX || HAVE_DECL_UV_TCP_INIT_EX
#define UDP_TCP_INIT_EX(name,TYPE)                                      \
  CAMLprim value                                                        \
  uwt_ ## name ## _init_ex(value o_loop, value o_mode)                  \
  {                                                                     \
    INIT_LOOP_RESULT(l,o_loop);                                         \
    CAMLparam1(o_loop);                                                 \
    CAMLlocal1(v);                                                      \
    v = uwt__handle_create(TYPE,l);                                     \
    struct handle * h = Handle_val(v);                                  \
    h->close_executed = 1;                                              \
    value ret = caml_alloc_small(1,Ok_tag);                             \
    Field(ret,0) = v;                                                   \
    h->close_executed = 0;                                              \
    h->initialized = 1;                                                 \
    const int mode = Long_val(o_mode) == 0 ? AF_INET : AF_INET6 ;       \
    const int erg = uv_ ## name ## _init_ex(&l->loop,                   \
                                            (uv_ ## name ## _t*)h->handle, \
                                            mode );                     \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      uwt__free_mem_uv_handle_t(h);                                     \
      uwt__free_struct_handle(h);                                       \
    }                                                                   \
    CAMLreturn(ret);                                                    \
  }
#endif

#if !HAVE_DECL_UV_UDP_INIT_EX || !HAVE_DECL_UV_TCP_INIT_EX
#define UDP_TCP_INIT_EX_NO(name)                \
  CAMLprim value                                \
  uwt_ ## name ## _init_ex(value a, value b)    \
  {                                             \
    (void)a;                                    \
    (void)b;                                    \
    return (uwt__result_enosys());              \
  }
#endif

#if HAVE_DECL_UV_TCP_INIT_EX
UDP_TCP_INIT_EX(tcp,UV_TCP)
#else
UDP_TCP_INIT_EX_NO(tcp)
#endif

#if HAVE_DECL_UV_UDP_INIT_EX
UDP_TCP_INIT_EX(udp,UV_UDP)
#else
UDP_TCP_INIT_EX_NO(udp)
#endif

#ifdef UDP_TCP_INIT_EX
#undef UDP_TCP_INIT_EX
#endif

#ifdef UDP_TCP_INIT_EX_NO
#undef UDP_TCP_INIT_EX_NO
#endif

static value
uwt_tcp_udp_open(value o_tcp, value o_fd, bool tcp)
{
  HANDLE_NINIT_NA(t,o_tcp);
  int ret;
#ifndef _WIN32
  uv_os_sock_t s = Long_val(o_fd);
#else
  uv_os_sock_t s;
  if ( Descr_kind_val(o_fd) != KIND_SOCKET ){
    ret = UV_EINVAL;
    goto endp;
  }
  s = Socket_val(o_fd);
  t->orig_fd = CRT_fd_val(o_fd);
#endif
  if ( tcp ){
    ret = uv_tcp_open((uv_tcp_t*)t->handle,s);
  }
  else {
    ret = uv_udp_open((uv_udp_t*)t->handle,s);
  }
  if ( ret >= 0 ){
    t->initialized = 1;
  }
#ifdef _WIN32
 endp:
#endif
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a,b,true));
}

CAMLprim value
uwt_udp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a,b,false));
}

CAMLprim value
uwt_tcp_bind_na(value o_tcp, value o_sock, value o_flags)
{
  HANDLE_NINIT_NA(t,o_tcp);
  struct sockaddr_storage addr;
  if ( !uwt__get_sockaddr(o_sock, (struct sockaddr *)&addr) ){
    return VAL_UWT_INT_RESULT_UNKNOWN;
  }
  unsigned int flags = o_flags == Val_unit ? 0 : UV_TCP_IPV6ONLY;
  int ret = uv_tcp_bind((uv_tcp_t *)t->handle, (struct sockaddr *)&addr, flags);
  if ( ret >= 0 ){
    t->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_nodelay_na(value o_tcp,value o_enable)
{
  HANDLE_NINIT_NA(th,o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_nodelay(t,Long_val(o_enable));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_keepalive_na(value o_tcp,value o_enable, value o_delay)
{
  HANDLE_NINIT_NA(th,o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_keepalive(t,Long_val(o_enable),Long_val(o_delay));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_simultaneous_accepts_na(value o_tcp,value o_enable)
{
  HANDLE_NINIT_NA(th,o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_simultaneous_accepts(t,Long_val(o_enable));
  return (VAL_UWT_UNIT_RESULT(ret));
}

typedef int (*getsock_f)(uv_handle_t *, struct sockaddr*, int *);
static value
uwt_tcp_getsockpeername(value o_tcp, getsock_f func)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(o_tcp);
  CAMLparam1(o_tcp);
  CAMLlocal1(sock);
  value ret;
  struct handle * th = Handle_val(o_tcp);
  uv_handle_t * t = th->handle;
  int s = sizeof(struct sockaddr_storage);
  int r;
  struct sockaddr_storage addr;
  r = func(t,(struct sockaddr*)&addr,&s);
  if ( r < 0 ) {
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(r);
  }
  else {
    sock = uwt__alloc_sockaddr((struct sockaddr*)&addr);
    if ( sock == Val_unit ){
      ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_UNKNOWN;
    }
    else {
      ret = caml_alloc_small(1,Ok_tag);
      Field(ret,0) = sock;
    }
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_tcp_getsockname(value o_tcp)
{
  return (uwt_tcp_getsockpeername(o_tcp,(getsock_f)uv_tcp_getsockname));
}

CAMLprim value
uwt_tcp_getpeername(value o_tcp)
{
  return (uwt_tcp_getsockpeername(o_tcp,(getsock_f)uv_tcp_getpeername));
}

CAMLprim value
uwt_udp_getsockname(value o_tcp)
{
  return (uwt_tcp_getsockpeername(o_tcp,(getsock_f)uv_udp_getsockname));
}

CAMLprim value
uwt_tcp_connect(value o_tcp,value o_sock,value o_cb)
{
  struct sockaddr_storage addr;
  if ( ! uwt__get_sockaddr(o_sock,(struct sockaddr*) &addr) ){
    return VAL_UWT_INT_RESULT_UNKNOWN;
  }
  HANDLE_INIT3(s,o_tcp,o_sock,o_cb);
  struct req * wp = uwt__req_create(UV_CONNECT,s->loop);
  uv_tcp_t* tcp = (uv_tcp_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  const int ret = uv_tcp_connect(req, tcp, (struct sockaddr*) &addr,
                                 uwt__pipe_tcp_connect_cb);
  if ( ret >= 0 ){
    wp->c_cb = uwt__ret_unit_cparam;
    uwt__gr_register(&wp->cb,o_cb);
    wp->in_use = 1;
    wp->finalize_called = 1;
    ++s->in_use_cnt;
  }
  else {
    uwt__free_mem_uv_req_t(wp);
    uwt__free_struct_req(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(ret));
}
