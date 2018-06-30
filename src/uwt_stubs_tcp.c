/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_tcp.h"

#define UDP_TCP_INIT(name,TYPE)                                         \
  CAMLprim value                                                        \
  uwt_ ## name ## _init(value o_loop)                                   \
  {                                                                     \
    INIT_LOOP_RESULT(l,o_loop);                                         \
    value ret = uwt__handle_res_create(TYPE, true);                     \
    value v = Field(ret,0);                                             \
    struct handle * h = Handle_val(v);                                  \
    const int erg = uv_ ## name ## _init(&l->loop,                      \
                                         (uv_ ## name ## _t*)h->handle); \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      uwt__free_handle(h);                                              \
    }                                                                   \
    return ret;                                                         \
  }

UDP_TCP_INIT(tcp,UV_TCP)
UDP_TCP_INIT(udp,UV_UDP)
#undef UDP_TCP_INIT

#define UDP_TCP_INIT_EX(name,TYPE)                                      \
  CAMLprim value                                                        \
  uwt_ ## name ## _init_ex(value o_loop, value o_mode)                  \
  {                                                                     \
    INIT_LOOP_RESULT(l,o_loop);                                         \
    value ret = uwt__handle_res_create(TYPE, true);                     \
    value v = Field(ret,0);                                             \
    struct handle * h = Handle_val(v);                                  \
    h->initialized = 1;                                                 \
    const int mode = Long_val(o_mode) == 0 ? AF_INET : AF_INET6 ;       \
    const int erg = uv_ ## name ## _init_ex(&l->loop,                   \
                                            (uv_ ## name ## _t*)h->handle, \
                                            mode );                     \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      uwt__free_handle(h);                                              \
    }                                                                   \
    return ret;                                                         \
  }

UDP_TCP_INIT_EX(tcp,UV_TCP)
UDP_TCP_INIT_EX(udp,UV_UDP)
#undef UDP_TCP_INIT_EX

typedef int (*udp_tcp_openf)(uv_handle_t *, uv_os_sock_t);
static value
uwt_tcp_udp_open(value o_tcp, value o_fd, udp_tcp_openf fun)
{
  HANDLE_INIT_NA(t, o_tcp);
  int ret;
#ifndef _WIN32
  uv_os_sock_t s = Long_val(o_fd);
#else
  uv_os_sock_t s;
  if ( Descr_kind_val(o_fd) != KIND_SOCKET ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  s = Socket_val(o_fd);
  t->orig_fd = CRT_fd_val(o_fd);
#endif
  ret = fun(t->handle,s);
  if ( ret >= 0 ){
    t->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a, b, (udp_tcp_openf)uv_tcp_open));
}

CAMLprim value
uwt_udp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a, b, (udp_tcp_openf)uv_udp_open));
}

CAMLprim value
uwt_tcp_bind_na(value o_tcp, value o_sock, value o_flags)
{
  HANDLE_INIT_NA(t, o_tcp);
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
  HANDLE_INIT_NA(th, o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_nodelay(t,Long_val(o_enable));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_keepalive_na(value o_tcp,value o_enable, value o_delay)
{
  HANDLE_INIT_NA(th, o_tcp);
  UINT_VAL_RET_IR_EINVAL(d,o_delay);
  int enable = Long_val(o_enable);
  /* uninit allowed, but forbidden by me. delay is ignored by libuv,
     when no socket was created */
  if ( enable && th->initialized == 0 ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  int ret = uv_tcp_keepalive((uv_tcp_t *)th->handle, enable, d);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_simultaneous_accepts_na(value o_tcp,value o_enable)
{
  HANDLE_INIT_NA(th, o_tcp);
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
  struct req * wp = uwt__req_create(UV_CONNECT);
  uv_tcp_t* tcp = (uv_tcp_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  const int ret = uv_tcp_connect(req, tcp, (struct sockaddr*) &addr,
                                 uwt__pipe_tcp_connect_cb);
  if ( ret >= 0 ){
    uwt__gr_register(&wp->cb,o_cb);
    ++s->in_use_cnt;
  }
  else {
    uwt__req_free(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(ret));
}
