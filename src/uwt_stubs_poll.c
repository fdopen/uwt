/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_poll.h"

static const int poll_flag_table[4] = {
  UV_READABLE, UV_WRITABLE,
#if HAVE_DECL_UV_DISCONNECT
  UV_DISCONNECT,
#else
  4,
#endif
#if HAVE_DECL_UV_PRIORITIZED
  UV_PRIORITIZED,
#else
  8,
#endif
};

static void
poll_cb(uv_poll_t* handle, int status, int events)
{
  HANDLE_CB_START(h, handle);
  uint8_t tag = Ok_tag;
  value val;
  value param;
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
  Begin_roots1(val);
  param = caml_alloc_small(1,tag);
  Field(param,0) = val;
  End_roots();
  value cb = GET_CB_VAL(h->cb_read);
  value t = GET_CB_VAL(h->cb_listen);
  param = caml_callback2_exn(cb,t,param);
  HANDLE_CB_END(param);
}

CAMLprim value
uwt_poll_start(value o_loop,
               value o_sock_or_fd,
               value o_event,
               value o_cb)
{
  const int event = SAFE_CONVERT_FLAG_LIST(o_event,poll_flag_table);
#if !defined(HAVE_DECL_UV_DISCONNECT) || !HAVE_DECL_UV_DISCONNECT
  if (unlikely( event & 4 )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS);
  }
#endif
#if !defined(HAVE_DECL_UV_PRIORITIZED) || !HAVE_DECL_UV_PRIORITIZED
  if (unlikely( event & 8 )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS);
  }
#endif
#ifdef _WIN32
  if (unlikely( Descr_kind_val(o_sock_or_fd) != KIND_SOCKET )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);
  }
#endif
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam1(o_cb);
#ifdef _WIN32
  const uv_os_sock_t sock = Socket_val(o_sock_or_fd);
  const int orig_fd = CRT_fd_val(o_sock_or_fd);
#endif
  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_POLL, false);
  value v = Field(ret,0);
  struct handle * h = Handle_val(v);
  uv_poll_t * p = (uv_poll_t*)h->handle;
#ifdef _WIN32
  int erg = uv_poll_init_socket(&l->loop, p, sock);
  h->orig_fd = orig_fd;
#else
  int erg = uv_poll_init(&l->loop, p, Long_val(o_sock_or_fd));
#endif
  if ( erg < 0 ){
    uwt__free_handle(h);
  }
  else {
    erg = uv_poll_start(p,event,poll_cb);
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
