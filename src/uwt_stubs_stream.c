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

#include "uwt_stubs_stream.h"

CAMLprim value
uwt_write_queue_size_na(value o_s)
{
  value ret;
  struct handle * h = Handle_val(o_s);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    ret = Val_long(0);
  }
  else {
    uv_stream_t* s = (uv_stream_t*)h->handle;
    ret = Val_long((intnat)s->write_queue_size);
  }
  return ret;
}

static void
shutdown_cb(uv_shutdown_t* req, int status)
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
    uwt__req_callback((void*)req);
    --s->in_callback_cnt;
    CLOSE_HANDLE_IF_UNREFERENCED(s);
  }
}

CAMLprim value
uwt_shutdown(value o_stream,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_INIT2(s,o_stream,o_cb);
  uv_stream_t* stream = (uv_stream_t*)s->handle;
  struct req * wp = uwt__req_create(UV_SHUTDOWN,s->loop);
  uv_shutdown_t * req = (uv_shutdown_t*)wp->req;
  const int erg = uv_shutdown(req,stream,shutdown_cb);
  value ret;
  if ( erg < 0 ){
    uwt__free_mem_uv_req_t(wp);
    uwt__free_struct_req(wp);
    ret = Val_uwt_int_result(erg);
  }
  else {
    wp->c_cb = uwt__ret_unit_cparam;
    uwt__gr_register(&wp->cb,o_cb);
    wp->in_use = 1;
    wp->finalize_called = 1;
    ++s->in_use_cnt;
    ret = Val_unit;
  }
  CAMLreturn(ret);
}

static void
listen_cb(uv_stream_t *server,int status)
{
  HANDLE_CB_INIT(server);
  value exn = Val_unit;
  struct handle * h = server->data;
  if (unlikely( h->cb_listen == CB_INVALID ||
                h->cb_listen_server == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value param = VAL_UWT_UNIT_RESULT(status);
    value s = GET_CB_VAL(h->cb_listen_server);
    exn = GET_CB_VAL(h->cb_listen);
    exn = caml_callback2_exn(exn,s,param);
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_listen(value o_stream,value o_backlog,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_INIT2(s,o_stream,o_cb);
  int ret;
  if ( s->cb_listen_server != CB_INVALID ||
       s->cb_listen != CB_INVALID ){
    ret = UV_EBUSY;
  }
  else {
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    ret = uv_listen(stream,Long_val(o_backlog),listen_cb);
    if ( ret >= 0 ){
      ++s->in_use_cnt;
      uwt__gr_register(&s->cb_listen,o_cb);
      uwt__gr_register(&s->cb_listen_server,o_stream);
    }
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_accept_raw_na(value o_serv,
                  value o_client)
{
  struct handle * serv = Handle_val(o_serv);
  struct handle * client = Handle_val(o_client);
  if ( HANDLE_IS_INVALID_UNINIT(serv) || HANDLE_IS_INVALID(client) ){
    return VAL_UWT_INT_RESULT_EBADF;
  }
  int ret = uv_accept((uv_stream_t*)serv->handle,
                      (uv_stream_t*)client->handle);
  if ( ret >= 0 ){
    client->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

static void
read_start_cb(uv_stream_t* stream,ssize_t nread, const uv_buf_t * buf)
{
  HANDLE_CB_INIT_WITH_CLEAN(stream);
  struct handle * h = stream->data;
  value ret = Val_unit;
  bool buf_not_cleaned = true;
  /*
    read zero: EAGAIN or EWOULDBLOCK / WSAEWOULDBLOCK (user not interested)
    https://github.com/joyent/libuv/blob/52ae456b0d666f6f4dbb7f52675f4f131855bd22/src/unix/stream.c#L1116
    https://github.com/joyent/libuv/blob/52ae456b0d666f6f4dbb7f52675f4f131855bd22/src/win/tcp.c#L973
  */
  if ( h->close_called == 0 && nread != 0 ){
    if ( h->cb_read == CB_INVALID ){
      DEBUG_PF("callbacks lost");
    }
    else {
      value o;
      value cb;
      int finished;
      int tag;
      if ( nread < 0 ){
        ret = Val_uwt_error(nread);
        finished = 1;
        tag = Error_tag;
      }
      else if ( (nread && !buf->base) || (size_t)nread > buf->len ){
        ret = VAL_UWT_ERROR_UWT_EFATAL;
        tag = Error_tag;
        finished = 1;
      }
      else {
        ret = caml_alloc_string(nread);
        memcpy(String_val(ret), buf->base, nread);
        finished = 0;
        tag = Ok_tag;
      }
      buf_not_cleaned = false;
      uwt__free_uv_buf_t_const(buf,h->cb_type);
      Begin_roots1(ret);
      o = caml_alloc_small(1,tag);
      Field(o,0) = ret;
      cb = GET_CB_VAL(h->cb_read);
      if ( finished == 1 ){
        h->cb_read_removed_by_cb = 1;
        uwt__gr_unregister(&h->cb_read);
        if ( h->in_use_cnt ){
          h->in_use_cnt--;
        }
      }
      End_roots();
      ret = caml_callback_exn(cb,o);
    }
  }
  if ( buf_not_cleaned && buf->base ){
    uwt__free_uv_buf_t_const(buf,h->cb_type);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_read_start(value o_stream,
               value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_INIT2(s,o_stream,o_cb);
  value ret;
  if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    int erg = 0;
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    if ( s->can_reuse_cb_read == 1 ){
      s->can_reuse_cb_read = 0;
      s->read_waiting = 0;
      erg = uv_read_stop(stream);
    }
    if ( erg >= 0 ){
      erg = uv_read_start(stream,uwt__alloc_cb,read_start_cb);
      if ( erg >= 0 ){
        s->c_read_size = DEF_ALLOC_SIZE;
        s->cb_read_removed_by_cb = 0;
        uwt__gr_register(&s->cb_read,o_cb);
        ++s->in_use_cnt;
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_read_stop(value o_stream, value o_abort)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_INIT(s,o_stream);
  value ret;
  if ( (s->read_waiting == 1 && Long_val(o_abort) == 0 ) ||
       (s->cb_read == CB_INVALID && s->cb_read_removed_by_cb != 1) ){
    /* Yes, dubious, but upstream libuv also doesn't report any error */
    ret = Val_long(0);
  }
  else {
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    int erg = uv_read_stop(stream);
    if ( erg >= 0 ){
      s->can_reuse_cb_read = 0;
      if ( s->in_use_cnt && s->cb_read_removed_by_cb == 0 ){
        --s->in_use_cnt;
      }
      if ( s->cb_read != CB_INVALID && s->cb_read_removed_by_cb != 1 ){
        uwt__gr_unregister(&s->cb_read);
      }
      s->cb_read_removed_by_cb = 0;
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

static void
read_own_cb(uv_stream_t* stream,ssize_t nread, const uv_buf_t * buf)
{
  HANDLE_CB_INIT_WITH_CLEAN(stream);
  struct handle * h = stream->data;
  value ret = Val_unit;
  const bool read_ba = h->use_read_ba == 1;
  bool buf_not_cleaned = true;
  if ( h->close_called == 0 && nread != 0 ){
    if (unlikely( h->cb_read == CB_INVALID ||
                  h->obuf == CB_INVALID )){
      DEBUG_PF("callbacks lost");
    }
    else {
      value o;
      value cb;
      bool finished;
      h->read_waiting = 0;
      if ( nread < 0 ){
        if ( nread == UV_ENOBUFS && buf->len == 0 ){
          o = Val_long(0);
          finished = false;
        }
        else {
          if ( nread == UV_EOF ){
            o = Val_long(0);
          }
          else {
            o = Val_uwt_int_result(nread);
          }
          finished = true;
        }
      }
      else {
        assert(buf->len >= (size_t)nread);
        assert((size_t)nread <= h->c_read_size);
        finished = false;
        o = Val_long(nread);
        if ( read_ba == false ){
          if (unlikely( !buf->base || (size_t)nread > buf->len )){
            o = VAL_UWT_INT_RESULT_UWT_EFATAL;
          }
          else {
            value tp = GET_CB_VAL(h->obuf);
            memcpy(String_val(tp) + h->obuf_offset, buf->base, (size_t)nread);
          }
        }
      }
      if ( read_ba == false ){
        buf_not_cleaned = false;
        uwt__free_uv_buf_t_const(buf,h->cb_type);
      }
      cb = GET_CB_VAL(h->cb_read);
      uwt__gr_unregister(&h->cb_read);
      uwt__gr_unregister(&h->obuf);
      h->can_reuse_cb_read = finished == false;
      if ( h->in_use_cnt ){
        h->in_use_cnt--;
      }
      ret = caml_callback2_exn(*uwt__global_wakeup,cb,o);
      /* it's not clear in older versions, how to handle this case,...
         https://github.com/joyent/libuv/issues/1534 */
      if ( h->close_called == 0 &&
           finished == false &&
           h->can_reuse_cb_read == 1 ){
        uv_read_stop(stream);
      }
      h->can_reuse_cb_read = 0;
    }
  }
  if ( buf_not_cleaned && read_ba == false && buf->base ){
    uwt__free_uv_buf_t_const(buf,h->cb_type);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_read_own(value o_s,value o_buf,value o_offset,value o_len,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_s);
  HANDLE_INIT3(s,o_s,o_buf,o_cb);
  const int ba = Tag_val(o_buf) != String_tag;
  value ret;
  const size_t len = Long_val(o_len);
  assert( s->cb_type == CB_LWT );
  if ( len > ULONG_MAX ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
  }
  else if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    int erg;
    if ( s->can_reuse_cb_read == 1 ){
      s->can_reuse_cb_read = 0;
      erg = 0;
    }
    else {
      uv_stream_t* stream = (uv_stream_t*)s->handle;
      erg = uv_read_start(stream,uwt__alloc_own_cb,read_own_cb);
    }
    if ( erg >= 0 ){
      size_t offset = Long_val(o_offset);
      uwt__gr_register(&s->cb_read,o_cb);
      uwt__gr_register(&s->obuf,o_buf);
      ++s->in_use_cnt;
      s->c_read_size = len;
      s->read_waiting = 1;
      s->use_read_ba = ba;
      if ( ba == 0 ){
        s->obuf_offset = offset;
      }
      else {
        s->ba_read = Ba_buf_val(o_buf) + offset;
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

#define XX(name,type)                                                   \
  static void name (type * req, int status)                             \
  {                                                                     \
    struct handle * s;                                                  \
    if (unlikely( !req || !req->data || !req->handle ||                 \
                  (s = req->handle->data) == NULL )){                   \
      DEBUG_PF("leaking data");                                         \
    }                                                                   \
    else {                                                              \
      struct req * r = req->data;                                       \
      ++s->in_callback_cnt;                                             \
      --s->in_use_cnt;                                                  \
      r->c_param = status;                                              \
      uwt__req_callback((void*)req);                               \
      --s->in_callback_cnt;                                             \
      CLOSE_HANDLE_IF_UNREFERENCED(s);                                  \
    }                                                                   \
  }

/* TODO: check the alignment, if we can cast or not */
XX(write_send_cb,uv_write_t)
XX(udp_send_cb,uv_udp_send_t)
#undef XX

CAMLprim value
uwt_write_send_native(value o_stream,value o_buf,value o_pos,value o_len,
                      value o_sock,value o_cb)
{
  const size_t len = Long_val(o_len);
  if ( len > ULONG_MAX ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  struct sockaddr_storage addr;
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  else {
    if ( !uwt__get_sockaddr(o_sock,(struct sockaddr*)&addr) ){
      return VAL_UWT_INT_RESULT_UNKNOWN;
    }
  }
  HANDLE_INIT4(s,o_stream,o_buf,o_sock,o_cb);
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  struct req * wp;
  wp = uwt__req_create( o_sock == Val_unit ? UV_WRITE : UV_UDP_SEND,
                   s->loop );
  value ret = Val_unit;
  if ( ba ){
    wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
    wp->buf.len = len;
    wp->buf_contains_ba = 1;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    uwt__malloc_uv_buf_t(&wp->buf,len,wp->cb_type);
    if ( wp->buf.base != NULL  ){
      memcpy(wp->buf.base,
             String_val(o_buf) + Long_val(o_pos),
             len);
    }
    else {
      ret = VAL_UWT_INT_RESULT_ENOMEM;
      uwt__free_mem_uv_req_t(wp);
      uwt__free_struct_req(wp);
    }
  }
  if ( ret == Val_unit ){
    int erg;
    void * req = wp->req;
    void * handle = s->handle;
    if ( o_sock == Val_unit ){
      erg = uv_write(req,handle,&wp->buf,1,write_send_cb);
    }
    else {
      erg = uv_udp_send(req, handle, &wp->buf, 1,
                        (struct sockaddr*)&addr, udp_send_cb);
    }
    if ( erg < 0 ){
      if ( ba == 0 ){
        uwt__free_uv_buf_t(&wp->buf,wp->cb_type);
      }
      uwt__free_mem_uv_req_t(wp);
      uwt__free_struct_req(wp);
    }
    else {
      wp->c_cb = uwt__ret_unit_cparam;
      wp->cb_type = s->cb_type;
      wp->in_use = 1;
      s->initialized = 1;
      uwt__gr_register(&wp->cb,o_cb);
      wp->finalize_called = 1;
      ++s->in_use_cnt;
      wp->buf_contains_ba = ba;
      if ( ba ){
        uwt__gr_register(&wp->sbuf,o_buf);
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}
BYTE_WRAP6(uwt_write_send)

CAMLprim value
uwt_writev(value o_stream, value o_ios, value o_sock, value o_iosave, value o_cb)
{
  const size_t ar_size = Wosize_val(o_ios);
#ifdef _WIN32
  size_t i;
  for (i = 0; i < ar_size; ++i){
    if ( (size_t)Long_val(Field(Field(o_ios,i),2)) > ULONG_MAX ){
      return VAL_UWT_INT_RESULT_EINVAL;
    }
  }
#endif
  struct sockaddr_storage addr;
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  else {
    if ( !uwt__get_sockaddr(o_sock,(struct sockaddr*)&addr) ){
      return VAL_UWT_INT_RESULT_UNKNOWN;
    }
  }
  HANDLE_INIT4(s, o_stream, o_ios, o_iosave, o_cb);
  struct req * wp;
  wp = uwt__req_create( o_sock == Val_unit ? UV_WRITE : UV_UDP_SEND,
                        s->loop );
  int erg = uwt__build_iovecs(o_ios, wp);
  if ( erg == 0 ){
    uv_buf_t * buf = (uv_buf_t *)&wp->c;
    uv_buf_t *bufs = (uv_buf_t *)buf->base;
    if ( o_sock == Val_unit ){
      erg = uv_write((uv_write_t*)wp->req,
                     (uv_stream_t*)s->handle,
                     bufs,
                     ar_size,
                     write_send_cb);
    }
    else {
      erg = uv_udp_send((uv_udp_send_t*)wp->req,
                        (uv_udp_t*)s->handle,
                        bufs,
                        ar_size,
                        (struct sockaddr*)&addr,
                        udp_send_cb);
    }
    if ( erg < 0 ){
      uwt__free_uv_buf_t((uv_buf_t *)&wp->c,wp->cb_type);
      uwt__free_uv_buf_t(&wp->buf, wp->cb_type);
    }
    else {
      wp->c_cb = uwt__ret_unit_cparam;
      wp->cb_type = s->cb_type;
      wp->in_use = 1;
      wp->buf_contains_ba = 0;
      s->initialized = 1;
      uwt__gr_register(&wp->cb,o_cb);
      wp->finalize_called = 1;
      ++s->in_use_cnt;
      if ( o_iosave != Val_unit ){
        uwt__gr_register(&wp->sbuf,o_iosave);
      }
      wp->clean_cb = uwt__clean_iovecs;
    }
  }
  if ( erg < 0 ){
    uwt__free_mem_uv_req_t(wp);
    uwt__free_struct_req(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}

CAMLprim value
uwt_try_writev_na(value o_stream, value o_ios, value o_sock)
{
  const size_t ar_size = Wosize_val(o_ios);
  size_t i;
#ifdef _WIN32
  for (i = 0; i < ar_size; ++i) {
    if ( (size_t)Long_val(Field(Field(o_ios,i),2)) > ULONG_MAX ){
      return VAL_UWT_INT_RESULT_EINVAL;
    }
  }
#endif
  struct sockaddr_storage addr;
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  else {
    if ( ! uwt__get_sockaddr(o_sock,(struct sockaddr *)&addr) ) {
      return VAL_UWT_INT_RESULT_UNKNOWN;
    }
  }
  HANDLE_NINIT_NA(s,o_stream);
#define N_BUFS 48
  uv_buf_t s_bufs[N_BUFS];
  int ret = 0 ;
  uv_buf_t * bufs = s_bufs;
  if ( ar_size > N_BUFS ){
    bufs = malloc(sizeof(*bufs) * ar_size);
    if ( bufs == NULL ){
      return VAL_UWT_INT_RESULT_ENOMEM;
    }
  }
  for ( i = 0; i< ar_size ; i++ ){
    value cur = Field(o_ios,i);
    const size_t offset = Long_val(Field(cur,1));
    bufs[i].len = Long_val(Field(cur,2));
    if ( Tag_val(cur) == 0 ){
      bufs[i].base = Ba_buf_val(Field(cur,0)) + offset;
    }
    else {
      bufs[i].base = String_val(Field(cur,0)) + offset;
    }
  }
  if ( o_sock == Val_unit ){
    ret = uv_try_write((uv_stream_t*)s->handle, bufs, ar_size);
  }
  else {
    ret = uv_udp_try_send((uv_udp_t*)s->handle, bufs, ar_size,
                          (struct sockaddr *)&addr );
    if ( ret >= 0 ){
      s->initialized = 1;
    }
  }
  if ( bufs != s_bufs ){
    free(bufs);
  }
  return (VAL_UWT_INT_RESULT(ret));
#undef N_BUFS
}

static void
cb_uwt_write2(uv_write_t* req, int status)
{
  struct handle * s1 = req->handle->data;
  struct handle * s2 = req->send_handle->data;
  struct req * r = req->data;
  if ( !s1 || !s2 || !r ){
    DEBUG_PF("leaking data");
  }
  else {
    ++s1->in_callback_cnt;
    ++s2->in_callback_cnt;
    --s1->in_use_cnt;
    --s2->in_use_cnt;

    r->c_param = status;
    uwt__req_callback((void*)req);

    --s1->in_callback_cnt;
    --s2->in_callback_cnt;
    CLOSE_HANDLE_IF_UNREFERENCED(s1);
    CLOSE_HANDLE_IF_UNREFERENCED(s2);
  }
}

CAMLprim value
uwt_write2_native(value o_stream,
                  value o_stream_send,
                  value o_buf,
                  value o_pos,
                  value o_len,
                  value o_cb)
{
  const size_t len = Long_val(o_len);
  if ( len > ULONG_MAX ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream_send);
  HANDLE2_INIT(s1,o_stream,s2,o_stream_send,o_cb,o_buf);
  value ret = Val_unit;
  struct req * wp = uwt__req_create(UV_WRITE,s1->loop);
  uv_write_t* req = (uv_write_t*)wp->req;
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  if ( ba ){
    wp->buf_contains_ba = 1;
    wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
    wp->buf.len = len;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    uwt__malloc_uv_buf_t(&wp->buf,len,wp->cb_type);
    if ( wp->buf.base != NULL ){
      memcpy(wp->buf.base,
             String_val(o_buf) + Long_val(o_pos),
             len);
    }
    else {
      ret = VAL_UWT_INT_RESULT_ENOMEM;
      uwt__free_mem_uv_req_t(wp);
      uwt__free_struct_req(wp);
    }
  }
  if ( ret == Val_unit ){
    int erg = 0;
    uv_stream_t* stream = (uv_stream_t*)s1->handle;
    uv_stream_t* stream_send = (uv_stream_t*)s2->handle;
    assert(s1->cb_type == s2->cb_type);
    erg = uv_write2(req,stream,&wp->buf,1,stream_send,cb_uwt_write2);
    if ( erg < 0 ){
      if ( ba == 0 ){
        uwt__free_uv_buf_t(&wp->buf,wp->cb_type);
      }
      uwt__free_mem_uv_req_t(wp);
      uwt__free_struct_req(wp);
    }
    else {
      wp->c_cb = uwt__ret_unit_cparam;
      wp->in_use = 1;
      uwt__gr_register(&wp->cb,o_cb);
      wp->finalize_called = 1;
      ++s1->in_use_cnt;
      ++s2->in_use_cnt;
      if ( ba ){
        uwt__gr_register(&wp->sbuf,o_buf);
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}
BYTE_WRAP6(uwt_write2)

CAMLprim value
uwt_udp_try_send_na(value o_stream,value o_buf,value o_pos,
                    value o_len,value o_sock)
{
  const size_t len = Long_val(o_len);
  if ( len > ULONG_MAX ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  HANDLE_NINIT_NA(s,o_stream);
  uv_buf_t buf;
  int ret;
  buf.len = len;
  if ( Tag_val(o_buf) != String_tag ){
    buf.base = Ba_buf_val(o_buf);
  }
  else {
    buf.base = String_val(o_buf);
  }
  buf.base+= Long_val(o_pos);
  if ( o_sock == Val_unit ){
    ret = uv_try_write((uv_stream_t*)s->handle,&buf,1);
  }
  else {
    struct sockaddr_storage addr;
    if ( ! uwt__get_sockaddr(o_sock,(struct sockaddr *)&addr) ) {
      return VAL_UWT_INT_RESULT_UNKNOWN;
    }
    else {
      ret = uv_udp_try_send((uv_udp_t*)s->handle, &buf, 1,
                            (struct sockaddr *)&addr );
      if ( ret >= 0 ){
        s->initialized = 1;
      }
    }
  }
  return (VAL_UWT_INT_RESULT(ret));
}

CAMLprim value
uwt_try_write_na(value o_stream,value o_buf,value o_pos,value o_len)
{
  return (uwt_udp_try_send_na(o_stream,o_buf,o_pos,o_len,Val_unit));
}

UV_HANDLE_BOOL(uv_stream_t,is_readable,false)
UV_HANDLE_BOOL(uv_stream_t,is_writable,false)

CAMLprim value
uwt_stream_set_blocking_na(value o_stream, value o_blocking)
{
  const struct handle * s = Handle_val(o_stream);
  value ret = VAL_UWT_INT_RESULT_EBADF;
  if ( s && s->handle && s->close_called == 0 && s->initialized == 1 ){
    const int block = Int_val(o_blocking);
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    const int r = uv_stream_set_blocking(stream,block);
    ret = VAL_UWT_UNIT_RESULT(r);
  }
  return ret;
}
