/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_stream.h"

CAMLprim value
uwt_write_queue_size_na(value o_s)
{
  struct handle * h = Handle_val(o_s);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    return Val_long(0);
  }
  uv_stream_t* s = (uv_stream_t*)h->handle;
  return Val_long((intnat)s->write_queue_size);
}

static void
shutdown_cb(uv_shutdown_t* req, int status)
{
  REQ_CB_INIT(req);
  struct handle * s = req->handle->data;
  REQ_CB_CALL(VAL_UWT_UNIT_RESULT(status));
  --s->in_use_cnt;
  CLOSE_HANDLE_IF_UNREFERENCED(s);
}

CAMLprim value
uwt_shutdown(value o_stream,value o_cb)
{
  HANDLE_INIT2_NO_UNINIT(s, o_stream, o_cb);
  value ret;
  uv_stream_t* stream = (uv_stream_t*)s->handle;
  struct req * wp = uwt__req_create(UV_SHUTDOWN);
  uv_shutdown_t * req = (uv_shutdown_t*)wp->req;
  const int erg = uv_shutdown(req,stream,shutdown_cb);
  if ( erg < 0 ){
    uwt__req_free(wp);
    ret = Val_uwt_int_result(erg);
  }
  else {
    uwt__gr_register(&wp->cb,o_cb);
    ++s->in_use_cnt;
    ret = Val_unit;
  }
  CAMLreturn(ret);
}

static void
listen_cb(uv_stream_t *server,int status)
{
  HANDLE_CB_START(h, server);
  value param = VAL_UWT_UNIT_RESULT(status);
  value s = GET_CB_VAL(h->cb_listen);
  s = caml_callback2_exn(Field(s,1),Field(s,0),param);
  HANDLE_CB_END(s);
}

CAMLprim value
uwt_listen(value o_stream,value o_backlog,value o_stream_cb)
{
  INT_VAL_RET_IR_EINVAL(backlog, o_backlog);
  HANDLE_INIT2_NO_UNINIT(s, o_stream, o_stream_cb);
  int ret;
  if ( s->cb_listen != CB_INVALID ){
    ret = UV_EBUSY;
  }
  else {
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    ret = uv_listen(stream, backlog, listen_cb);
    if ( ret >= 0 ){
      ++s->in_use_cnt;
      uwt__gr_register(&s->cb_listen,o_stream_cb);
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
  if ( client->initialized == 1 ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  else if ( serv->handle->type != client->handle->type ){
    if ( serv->handle->type != UV_NAMED_PIPE ){
      return VAL_UWT_INT_RESULT_EINVAL;
    }
#ifdef _WIN32
    if ( client->handle->type != UV_TCP ){
      return VAL_UWT_INT_RESULT_EINVAL;
    }
#endif
  }
#ifdef _WIN32
  else {
    /* libuv asssumes the client to be a tcp handle,
       if the server is initialized with ipc */
    if ( serv->handle->type == UV_NAMED_PIPE ){
      uv_pipe_t* server = (uv_pipe_t*) serv->handle;
      if ( server->ipc ){
        return VAL_UWT_INT_RESULT_EINVAL;
      }
    }
  }
#endif
  int ret = uv_accept((uv_stream_t*)serv->handle, (uv_stream_t*)client->handle);
  if ( ret >= 0 ){
    client->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

static void
read_start_cb(uv_stream_t* stream,ssize_t nread, const uv_buf_t * buf)
{
  HANDLE_CB_INIT_WITH_CLEAN(h, stream);
  value ret = Val_unit;
  bool buf_not_cleaned = true;
  /*
    read zero: EAGAIN or EWOULDBLOCK / WSAEWOULDBLOCK (user not interested)
    https://github.com/joyent/libuv/blob/52ae456b0d666f6f4dbb7f52675f4f131855bd22/src/unix/stream.c#L1116
    https://github.com/joyent/libuv/blob/52ae456b0d666f6f4dbb7f52675f4f131855bd22/src/win/tcp.c#L973
  */
  if ( h->close_called == 0 && nread != 0 ){
    assert ( h->cb_read != CB_INVALID );
    value o;
    value cb;
    bool finished;
    uint8_t tag;
    if ( nread < 0 ){
      ret = Val_uwt_error(nread);
      tag = Error_tag;
      finished = nread != UV_ENOBUFS;
    }
    else {
      ret = caml_alloc_string(nread);
      memcpy(String_val(ret), buf->base, nread);
      finished = false;
      tag = Ok_tag;
    }
    buf_not_cleaned = false;
    uwt__free_uv_buf_t_const(buf);
    Begin_roots1(ret);
    o = caml_alloc_small(1,tag);
    Field(o,0) = ret;
    End_roots();
    cb = GET_CB_VAL(h->cb_read);
    if ( finished ){
      uwt__gr_unregister(&h->cb_read);
      h->in_use_cnt--;
    }
    ret = caml_callback_exn(cb,o);
  }
  if ( buf_not_cleaned && buf->base ){
    uwt__free_uv_buf_t_const(buf);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_read_start(value o_stream,
               value o_cb)
{
  HANDLE_INIT2_NO_UNINIT(s, o_stream, o_cb);
  value ret;
  if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    int erg = 0;
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    if ( s->can_reuse_cb_read == 1 ){
      erg = uv_read_stop(stream);
    }
    if ( erg >= 0 ){
      erg = uv_read_start(stream,uwt__alloc_cb,read_start_cb);
      if ( erg >= 0 ){
        s->c_read_size = DEF_ALLOC_SIZE;
        uwt__gr_register(&s->cb_read,o_cb);
        ++s->in_use_cnt;
      }
      s->can_reuse_cb_read = 0;
      s->read_waiting = 0;
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
  if ( /* Don't allow to abort parallel Uwt.Stream.read threads */
    (s->read_waiting == 1 && Long_val(o_abort) == 0 ) ||
    /* Not reading at all. libuv doesn't report an error in this case, too */
    s->cb_read == CB_INVALID ){
    ret = Val_long(0);
  }
  else {
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    int erg = uv_read_stop(stream);
    if ( erg >= 0 ){
      s->can_reuse_cb_read = 0;
      --s->in_use_cnt;
      uwt__gr_unregister(&s->cb_read);
      s->read_waiting = 0;
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

static void
read_own_cb(uv_stream_t* stream,ssize_t nread, const uv_buf_t * buf)
{
  HANDLE_CB_INIT_WITH_CLEAN(h, stream);
  value ret = Val_unit;
#ifndef UWT_NO_COPY_READ
  const bool read_ba = h->use_read_ba == 1;
  bool buf_not_cleaned = true;
#else
  (void) buf;
#endif
  if ( h->close_called == 0 && nread != 0 ){
    assert ( h->cb_read != CB_INVALID );
    value o;
    value cb;
    bool finished;
    if ( nread < 0 ){
      if ( nread == UV_ENOBUFS ){
        o = h->c_read_size ? VAL_UWT_INT_RESULT_ENOMEM : Val_long(0);
        finished = false;
      }
      else {
        o = nread == UV_EOF ? Val_long(0) : Val_uwt_int_result(nread);
        finished = true;
      }
    }
    else {
      finished = false;
      o = Val_long(nread);
#ifndef UWT_NO_COPY_READ
      if ( read_ba == false ){
        value tp = Field(GET_CB_VAL(h->cb_read),0);
        memcpy(String_val(tp) + h->x.obuf_offset, buf->base, (size_t)nread);
      }
#endif
    }
#ifndef UWT_NO_COPY_READ
    if ( read_ba == false ){
      buf_not_cleaned = false;
      uwt__free_uv_buf_t_const(buf);
    }
#endif
    h->read_waiting = 0;
    h->can_reuse_cb_read = finished == false;
    h->in_use_cnt--;
    cb = Field(GET_CB_VAL(h->cb_read),1);
    uwt__gr_unregister(&h->cb_read);
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
#ifndef UWT_NO_COPY_READ
  if ( buf_not_cleaned && read_ba == false && buf->base ){
    uwt__free_uv_buf_t_const(buf);
  }
#endif
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_read_own(value o_s,value o_offset,value o_len,value o_buf_cb)
{
  HANDLE_INIT2_NO_UNINIT(s, o_s, o_buf_cb);
  const int ba = Tag_val(Field(o_buf_cb,0)) != String_tag;
  value ret;
  const size_t len = Long_val(o_len);
  if ( len > ULONG_MAX ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
  }
  else if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    int erg = 0;
    if ( s->can_reuse_cb_read == 0 ){
      uv_stream_t* stream = (uv_stream_t*)s->handle;
      erg = uv_read_start(stream,uwt__alloc_own_cb,read_own_cb);
    }
    if ( erg >= 0 ){
      size_t offset = Long_val(o_offset);
      uwt__gr_register(&s->cb_read,o_buf_cb);
      ++s->in_use_cnt;
      s->c_read_size = len;
      s->read_waiting = 1;
      s->can_reuse_cb_read = 0;
      s->use_read_ba = ba;
      if ( ba == 0 ){
        s->x.obuf_offset = offset;
      }
      else {
        s->x.ba_read = Ba_buf_val(Field(o_buf_cb,0)) + offset;
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

static void write_cb(uv_req_t * req, int status)
{
  REQ_CB_INIT(req);
  struct req * wp = req->data;
  struct handle * s;
  if ( req->type == UV_WRITE ){
    s = ((uv_write_t*) req)->handle->data;
  }
  else {
    s = ((uv_udp_send_t*) req)->handle->data;
  }
  if ( wp->sbuf != CB_INVALID ){
    uwt__gr_unregister(&wp->sbuf);
  }
  if ( wp->buf_contains_ba == 0 ){
    uwt__free_uv_buf_t(&wp->buf);
  }
  REQ_CB_CALL(VAL_UWT_UNIT_RESULT(status));
  --s->in_use_cnt;
  CLOSE_HANDLE_IF_UNREFERENCED(s);
}

CAMLprim value
uwt_write_send_native(value o_stream,value o_buf,value o_pos,value o_len,
                      value o_sock,value o_cb)
{
  const size_t len = Long_val(o_len);
  const size_t pos = Long_val(o_pos);
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
  HANDLE_INIT3(s,o_stream,o_buf,o_cb);
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  struct req * wp;
  int erg;
  wp = uwt__req_create( o_sock == Val_unit ? UV_WRITE : UV_UDP_SEND );
  if ( ba ){
    wp->buf.base = Ba_buf_val(o_buf) + pos;
    wp->buf.len = len;
    wp->buf_contains_ba = 1;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    uwt__malloc_uv_buf_t(&wp->buf,len);
    if ( wp->buf.base == NULL ){
      erg = UV_ENOMEM;
      goto endp;
    }
    memcpy(wp->buf.base, String_val(o_buf) + pos, len);
  }
  void * req = wp->req;
  void * handle = s->handle;
  if ( o_sock == Val_unit ){
    erg = uv_write(req,handle,&wp->buf,1,(uv_write_cb)write_cb);
  }
  else {
    erg = uv_udp_send(req, handle, &wp->buf, 1, (struct sockaddr*)&addr,
                      (uv_udp_send_cb)write_cb);
  }
  if ( erg >= 0 ){
    s->initialized = 1;
    uwt__gr_register(&wp->cb,o_cb);
    ++s->in_use_cnt;
    if ( ba ){
      uwt__gr_register(&wp->sbuf,o_buf);
    }
  }
  else {
    if ( ba == 0 ){
      uwt__free_uv_buf_t(&wp->buf);
    }
  endp:
    uwt__req_free(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}
BYTE_WRAP6(uwt_write_send)

CAMLprim value
uwt_writev(value o_stream, value o_ios, value o_sock, value o_iosave, value o_cb)
{
  const size_t ar_size = Wosize_val(o_ios);
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
  int erg;
  wp = uwt__req_create( o_sock == Val_unit ? UV_WRITE : UV_UDP_SEND );
  erg = uwt__build_iovecs(o_ios, wp);
  if ( erg != 0 ){
    goto endp;
  }
  uv_buf_t *bufs = (uv_buf_t *)wp->buf.base;
  if ( o_sock == Val_unit ){
    erg = uv_write((uv_write_t*)wp->req,
                   (uv_stream_t*)s->handle,
                   bufs,
                   ar_size,
                   (uv_write_cb)write_cb);
  }
  else {
    erg = uv_udp_send((uv_udp_send_t*)wp->req,
                      (uv_udp_t*)s->handle,
                      bufs,
                      ar_size,
                      (struct sockaddr*)&addr,
                      (uv_udp_send_cb)write_cb);
  }
  if ( erg >= 0 ){
    s->initialized = 1;
    uwt__gr_register(&wp->cb,o_cb);
    ++s->in_use_cnt;
    if ( o_iosave != Val_unit ){
      uwt__gr_register(&wp->sbuf,o_iosave);
    }
  }
  else {
    uwt__free_uv_buf_t(&wp->buf);
endp:
    uwt__req_free(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}

CAMLprim value
uwt_try_writev_na(value o_stream, value o_ios, value o_sock)
{
  const size_t ar_size = Wosize_val(o_ios);
  size_t i;
  struct sockaddr_storage addr;
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  else {
    if ( ! uwt__get_sockaddr(o_sock,(struct sockaddr *)&addr) ) {
      return VAL_UWT_INT_RESULT_UNKNOWN;
    }
  }
  HANDLE_INIT_NA(s, o_stream);
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
  for ( i = 0; i < ar_size ; i++ ){
    value cur = Field(o_ios,i);
    const size_t offset = Long_val(Field(cur,1));
    const size_t len = Long_val(Field(cur,2));
#if defined(_WIN32) && defined(ARCH_SIXTYFOUR)
    if (unlikely( len > ULONG_MAX )){
      ret = VAL_UWT_INT_RESULT_EINVAL;
      goto endp;
    }
#endif
    bufs[i].len = len;
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
#if defined(_WIN32) && defined(ARCH_SIXTYFOUR)
endp:
#endif
  if ( bufs != s_bufs ){
    free(bufs);
  }
  return (VAL_UWT_INT_RESULT(ret));
#undef N_BUFS
}

static void
cb_uwt_write2(uv_write_t* req, int status)
{
  struct req * r = req->data;
  struct handle * s1 = r->c.p1;
  struct handle * s2 = r->c.p2;
  REQ_CB_INIT(req);
  if ( r->buf_contains_ba ){
    uwt__gr_unregister(&r->sbuf);
  }
  else {
    uwt__free_uv_buf_t(&r->buf);
  }
  REQ_CB_CALL(VAL_UWT_UNIT_RESULT(status));
  --s1->in_use_cnt;
  --s2->in_use_cnt;
  CLOSE_HANDLE_IF_UNREFERENCED(s1);
  CLOSE_HANDLE_IF_UNREFERENCED(s2);
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
  const size_t pos = Long_val(o_pos);
  if ( len > ULONG_MAX ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream_send);
  HANDLE2_INIT(s1,o_stream,s2,o_stream_send,o_cb,o_buf);
  int erg;
#ifdef _WIN32
  if ( s2->handle->type != UV_TCP ){
    erg = UV_EINVAL;
    goto endp;
  }
#endif
  struct req * wp = uwt__req_create(UV_WRITE);
  uv_write_t* req = (uv_write_t*)wp->req;
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  if ( ba ){
    wp->buf_contains_ba = 1;
    wp->buf.base = Ba_buf_val(o_buf) + pos;
    wp->buf.len = len;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    uwt__malloc_uv_buf_t(&wp->buf,len);
    if ( wp->buf.base == NULL ){
      erg = UV_ENOMEM;
      goto endp2;
    }
    memcpy(wp->buf.base, String_val(o_buf) + pos, len);
  }
  uv_stream_t* stream = (uv_stream_t*)s1->handle;
  uv_stream_t* stream_send = (uv_stream_t*)s2->handle;
  erg = uv_write2(req,stream,&wp->buf,1,stream_send,cb_uwt_write2);
  if ( erg >= 0 ){
    uwt__gr_register(&wp->cb,o_cb);
    wp->c.p1 = s1;
    wp->c.p2 = s2;
    ++s1->in_use_cnt;
    ++s2->in_use_cnt;
    if ( ba ){
      uwt__gr_register(&wp->sbuf,o_buf);
    }
  }
  else {
    if ( ba == 0 ){
      uwt__free_uv_buf_t(&wp->buf);
    }
  endp2:
    uwt__req_free(wp);
  }
#ifdef _WIN32
endp:
#endif
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
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
  HANDLE_INIT_NA(s, o_stream);
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
  if ( s && s->close_called == 0 && s->initialized == 1 ){
    const int block = Int_val(o_blocking);
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    const int r = uv_stream_set_blocking(stream,block);
    ret = VAL_UWT_UNIT_RESULT(r);
  }
  return ret;
}
