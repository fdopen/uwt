/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_fs.h"

#define CB_SYNC 0
#define CB_LWT 1

#define NO_FS_3(name)                                                   \
  value uwt_ ## name (value a, value b, value c, value d, value e) {    \
    (void) a; (void) b; (void) c; (void) d; (void) e;                   \
    value ret = caml_alloc_small(1,Error_tag);                          \
    Field(ret,0) = VAL_UWT_ERROR_ENOSYS;                                \
    return ret;                                                         \
  }                                                                     \
  value uwt_ ## name ## _sync(value a, value b, value c) {              \
    (void) a; (void) b; (void) c;                                       \
    return VAL_UWT_INT_RESULT_ENOSYS;                                   \
  }

#define MALLOC_UV_BUF_T(buf,xlen)                       \
  do {                                                  \
    uv_buf_t * buf__ = buf;                             \
    if ( callback_type == CB_SYNC ){                    \
      buf__->base = stack_buf;                          \
      buf__->len = UMIN(xlen,(size_t)UNIX_BUFFER_SIZE); \
    }                                                   \
    else {                                              \
      uwt__malloc_uv_buf_t(buf__,xlen);                 \
    }                                                   \
  } while (0)

#define CHECK_STRING(s)                         \
  do {                                          \
    if ( !uwt_is_safe_string(s) ){              \
      ret = UV_ECHARSET;                        \
      goto eend;                                \
    }                                           \
  } while (0)

#define ALLOC_WP()                                  \
  do {                                              \
    if ( callback_type == CB_LWT ){                 \
      GR_ROOT_ENLARGE();                            \
      wp = uwt__req_create_res(UV_FS, &o_ret);      \
      req =(uv_fs_t*) wp->req;                      \
    }                                               \
  } while (0)

#define BLOCK(call)                             \
  do {                                          \
    if ( callback_type == CB_SYNC ){            \
      caml_enter_blocking_section();            \
    }                                           \
    else {                                      \
      if ( wp == NULL ){                        \
        ALLOC_WP();                             \
      }                                         \
    }                                           \
    ret = call;                                 \
    if ( callback_type == CB_SYNC ){            \
      caml_leave_blocking_section();            \
    }                                           \
  } while (0)

#define COPY_STR1(x,code)                       \
  do {                                          \
    char * x##_dup = NULL;                      \
    CHECK_STRING(x);                            \
    if ( callback_type == CB_SYNC ){            \
      x ## _dup = strdup(String_val(x));        \
      if ( x ## _dup == NULL ){                 \
        ret = UV_ENOMEM;                        \
        goto eend;                              \
      }                                         \
    }                                           \
    do code while(0);                           \
    if ( callback_type == CB_SYNC ){            \
      free( x##_dup);                           \
    }                                           \
  } while(0)

#define COPY_STR2(x,y,code)                                 \
  do {                                                      \
    char * x##_dup = NULL;                                  \
    char * y##_dup = NULL;                                  \
    CHECK_STRING(x);                                        \
    CHECK_STRING(y);                                        \
    if ( callback_type == CB_SYNC ){                        \
      const size_t xdup_len = strlen(String_val(x)) + 1;    \
      const size_t ydup_len = strlen(String_val(y)) + 1;    \
      x ## _dup = malloc(xdup_len + ydup_len);              \
      if ( x ## _dup == NULL ){                             \
        ret = UV_ENOMEM;                                    \
        goto eend;                                          \
      }                                                     \
      memcpy(x##_dup, String_val(x), xdup_len);             \
      y ## _dup = x##_dup + xdup_len;                       \
      memcpy(y##_dup, String_val(y), ydup_len);             \
    }                                                       \
    do code while(0);                                       \
    if ( callback_type == CB_SYNC ){                        \
      free( x##_dup );                                      \
    }                                                       \
  } while(0)

#define STRING_VAL(x)                           \
  (callback_type == CB_SYNC ?                   \
    (x ## _dup) :                               \
   String_val(x))

#define R_WRAP_LWT(name,tz,code)                \
  value o_ret;                                  \
  struct req * wp = NULL;                       \
  uv_fs_t * req = NULL;                         \
  ATTR_UNUSED                                   \
  int ret = UV_UWT_EFATAL;                      \
  uv_loop_t * loop = Uv_loop_val(o_loop);       \
  enum { callback_type = CB_LWT };              \
  const uv_fs_cb cb = fs_callback;              \
  do                                            \
    code                                        \
      while(0);                                 \
  if ( ret >= 0  ){                             \
    wp->c_cb = tz;                              \
    uwt__gr_register__(&wp->cb,o_cb);           \
  }                                             \
  else {                                        \
  eend:                                         \
    ATTR_UNUSED;                                \
    if (wp){                                    \
      uwt__req_free(wp);                        \
    }                                           \
    o_ret = caml_alloc_small(1,Error_tag);      \
    Field(o_ret,0) = Val_uwt_error(ret);        \
  }                                             \
  CAMLreturn(o_ret)

static value fs_read_cb(uv_req_t * r);
static value fs_write_cb(uv_req_t * r);

#define R_WRAP_SYNC(name,tz,code)                       \
  value o_ret;                                          \
  uv_fs_t req_;                                         \
  uv_fs_t * req = &req_;                                \
  struct req wp_req_;                                   \
  ATTR_UNUSED                                           \
  struct req * wp = &wp_req_;                           \
  int ret = UV_UWT_EFATAL;                              \
  uv_loop_t * loop = NULL;                              \
  enum { callback_type = CB_SYNC };                     \
  const uv_fs_cb cb = NULL;                             \
  req_.data = NULL;                                     \
  req_.fs_type = UV_FS_UNKNOWN;                         \
  do                                                    \
    code                                                \
      while(0);                                         \
  if ( ret >= 0  ){                                     \
    if ( tz == fs_result_unit ){                        \
      o_ret = VAL_UWT_UNIT_RESULT(ret);                 \
    }                                                   \
    else if ( tz == fs_read_cb || tz == fs_write_cb ){  \
      o_ret = VAL_UWT_INT_RESULT(req->result);          \
    }                                                   \
    else {                                              \
      o_ret = tz((uv_req_t*)req);                       \
    }                                                   \
  }                                                     \
  else {                                                \
  eend:                                                 \
    ATTR_UNUSED;                                        \
    if ( tz == fs_result_unit || tz == fs_read_cb ||    \
         tz == fs_write_cb  ){                          \
      o_ret = VAL_UWT_UNIT_RESULT(ret);                 \
    }                                                   \
    else {                                              \
      o_ret = caml_alloc_small(1,Error_tag);            \
      Field(o_ret,0) = Val_uwt_error(ret);              \
    }                                                   \
  }                                                     \
  if ( req_.fs_type != UV_FS_UNKNOWN ){                 \
    uv_fs_req_cleanup(&req_);                           \
  }                                                     \
  CAMLreturn(o_ret)

#define FSFUNC_5(name,tz,a,b,c,d,e,code)                                \
  CAMLprim value                                                        \
  uwt_ ## name ## _byte(value *a, int argn)                             \
  {                                                                     \
    (void)argn;                                                         \
    return (uwt_ ## name ## _native(a[0],a[1],a[2],a[3],                \
                                    a[4],a[5],a[6]));                   \
  }                                                                     \
  CAMLprim value                                                        \
  uwt_ ## name ## _native (value a, value b, value c,value d, value e,  \
                           value o_loop, value o_cb ){                  \
    CAMLparam3(a,b,o_cb);                                               \
    R_WRAP_LWT(name,tz,code);                                           \
  }                                                                     \
  CAMLprim value                                                        \
  uwt_ ## name ## _sync (value a, value b, value c, value d, value e){  \
    CAMLparam2(a,b);                                                    \
    R_WRAP_SYNC(name,tz,code);                                          \
  }

#define FSFUNC_4(name,tz,a,b,c,d,code)                              \
  CAMLprim value                                                    \
  uwt_ ## name ## _byte(value *a, int argn)                         \
  {                                                                 \
    (void)argn;                                                     \
    return (uwt_ ## name ## _native(a[0],a[1],a[2],a[3],            \
                                    a[4],a[5]));                    \
  }                                                                 \
  CAMLprim value                                                    \
  uwt_ ## name ## _native (value a, value b, value c,value d,       \
                           value o_loop, value o_cb ){              \
    CAMLparam3(a,b,o_cb);                                           \
    R_WRAP_LWT(name,tz,code);                                       \
  }                                                                 \
  CAMLprim value                                                    \
  uwt_ ## name ## _sync (value a, value b, value c, value d ){      \
    CAMLparam2(a,b);                                                \
    R_WRAP_SYNC(name,tz,code);                                      \
  }

#define FSFUNC_3(name,tz,a,b,c,code)                                    \
  CAMLprim value                                                        \
  uwt_ ## name (value a, value b, value c, value o_loop, value o_cb ){  \
    CAMLparam3(a,b,o_cb);                                               \
    R_WRAP_LWT(name,tz,code);                                           \
  }                                                                     \
  CAMLprim value                                                        \
  uwt_ ## name ## _sync (value a, value b, value c ){                   \
    CAMLparam2(a,b);                                                    \
    R_WRAP_SYNC(name,tz,code);                                          \
  }

#define FSFUNC_2(name,tz,a,b,code)                            \
  CAMLprim value                                              \
  uwt_ ## name  (value a,value b, value o_loop, value o_cb ){ \
    CAMLparam3(a,b,o_cb);                                     \
    R_WRAP_LWT(name,tz,code);                                 \
  }                                                           \
  CAMLprim value                                              \
  uwt_ ## name ## _sync  (value a,value b ){                  \
    CAMLparam2(a,b);                                          \
    R_WRAP_SYNC(name,tz,code);                                \
  }

#define FSFUNC_1(name,tz,a,code)                          \
  CAMLprim value                                          \
  uwt_ ## name  (value a, value o_loop, value o_cb ){     \
    CAMLparam2(a,o_cb);                                   \
    R_WRAP_LWT(name,tz,code);                             \
  }                                                       \
  CAMLprim value                                          \
  uwt_ ## name ##_sync  (value a){                        \
    CAMLparam1(a);                                        \
    R_WRAP_SYNC(name,tz,code);                            \
  }

static value
fs_result_unit(uv_req_t * r)
{
  uv_fs_t * req = (uv_fs_t*)r;
  return (VAL_UWT_UNIT_RESULT(req->result));
}
#define runit fs_result_unit

static void
fs_callback(uv_fs_t * req)
{
  REQ_CB_INIT(req);
  struct req * wp = req->data;
  value v;
  if ( wp->c_cb == fs_result_unit ){
    v = VAL_UWT_UNIT_RESULT(((uv_fs_t*)req)->result);
  }
  else {
    v = wp->c_cb((uv_req_t*)req);
  }
  uv_fs_req_cleanup(req);
  REQ_CB_CALL(v);
}

static const int open_flag_table[22] = {
#ifdef _WIN32
  _O_RDONLY, _O_WRONLY, _O_RDWR, 0, _O_CREAT , _O_EXCL , _O_TRUNC, _O_APPEND,
  0, /* O_NOCTTY */
#ifdef UV_FS_O_DSYNC
  UV_FS_O_DSYNC,
#else
  0,
#endif
#ifdef UV_FS_O_SYNC
  UV_FS_O_SYNC,
#else
  0,
#endif
  0, /* O_RSYNC */
  _O_TEMPORARY, _O_SHORT_LIVED, _O_SEQUENTIAL, _O_RANDOM,
#ifdef UV_FS_O_DIRECT
  UV_FS_O_DIRECT,
#else
  0,
#endif
#ifdef UV_FS_O_EXLOCK
  UV_FS_O_EXLOCK,
#else
  0,
#endif
#ifdef UV_FS_O_NOATIME
  UV_FS_O_NOATIME,
#else
  0,
#endif
#ifdef UV_FS_O_SYMLINK
  UV_FS_O_SYMLINK,
#else
  0,
#endif
#ifdef UV_FS_O_NOFOLLOW
  UV_FS_O_NOFOLLOW,
#else
  0,
#endif
#ifdef UV_FS_O_DIRECTORY
  UV_FS_O_DIRECTORY,
#else
  0,
#endif

#else /* _WIN32 */
#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#define DEF_O_NONBLOCK
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#define DEF_O_DSYNC
#endif
#ifndef O_SYNC
#define O_SYNC 0
#define DEF_O_SYNC
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#define DEF_O_RSYNC
#endif
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_CREAT , O_EXCL , O_TRUNC, O_APPEND,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC,
  0, /* _O_TEMPORARY */
  0, /* _O_SHORT_LIVED */
  0, /* _O_SEQUENTIAL */
  0,  /* _O_RANDOM */
#if defined(UV_FS_O_DIRECT)
  UV_FS_O_DIRECT,
#elif defined(O_DIRECT)
  O_DIRECT,
#else
  0,
#endif
#if defined(UV_FS_O_EXLOCK)
  UV_FS_O_EXLOCK,
#elif defined(O_EXLOCK)
  O_EXLOCK,
#else
  0,
#endif
#if defined(UV_FS_O_NOATIME)
  UV_FS_O_NOATIME,
#elif defined(O_NOATIME)
  O_NOATIME,
#else
  0,
#endif
#if defined(UV_FS_O_SYMLINK)
  UV_FS_O_SYMLINK,
#elif defined(O_SYMLINK)
  O_SYMLINK,
#else
  0,
#endif
#if defined(UV_FS_O_NOFOLLOW)
  UV_FS_O_NOFOLLOW,
#elif defined(O_NOFOLLOW)
  O_NOFOLLOW,
#else
  0,
#endif
#if defined(UV_FS_O_DIRECTORY)
  UV_FS_O_DIRECTORY,
#elif defined(O_DIRECTORY)
  O_DIRECTORY,
#else
  0,
#endif

#endif /* _WIN32 */
};
#ifdef _WIN32
static void
fs_open_clean_cb(uv_fs_t* req)
{
  uv_fs_req_cleanup(req);
  free(req);
}
#endif

static value
fs_open_cb(uv_req_t * r)
{
  uv_fs_t* req = (uv_fs_t*)r;
  value ret;
  const int fd = (int) req->result;
  if ( fd < 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(fd);
  }
  else {
#ifndef _WIN32
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = Val_long(fd);
#else
    HANDLE handle = (HANDLE)(0 + _get_osfhandle(fd));
    if (unlikely( handle == INVALID_HANDLE_VALUE )){
      if (req->loop == NULL){
        uv_fs_t creq;
        uv_fs_close(NULL, &creq, fd, NULL);
      }
      else {
        uv_fs_t* creq = malloc(sizeof *creq);
        if ( creq ){
          int cret = uv_fs_close(req->loop, creq, fd, fs_open_clean_cb);
          if ( cret < 0 ){
            free(creq);
          }
        }
      }
      ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_EBADF;
    }
    else {
      value p = win_alloc_handle(handle);
      CRT_fd_val(p) = fd;
      Begin_roots1(p);
      ret = caml_alloc_small(1,Ok_tag);
      Field(ret,0) = p;
      End_roots();
    }
#endif
  }
  return ret;
}

FSFUNC_3(fs_open, fs_open_cb, o_name, o_flag_list, o_perm, {
  const intnat perm = Long_val(o_perm);
  if ( perm < INT_MIN || perm > INT_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  const int flags = SAFE_CONVERT_FLAG_LIST(o_flag_list,open_flag_table);
  COPY_STR1(o_name,{
    BLOCK(uv_fs_open(loop, req, STRING_VAL(o_name), flags, perm, cb));
  });
})

static value
fs_read_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  const ssize_t result = req->result;
  const value param = VAL_UWT_INT_RESULT(result);
  struct req * wp = r->data;
  if ( result > 0 && wp->buf_contains_ba == 0 ){
    value o = GET_CB_VAL(wp->sbuf);
    memcpy(String_val(o) + wp->offset, wp->buf.base, result);
  }
  uwt__gr_unregister(&wp->sbuf);
  if (wp->buf_contains_ba == 0){
    uwt__free_uv_buf_t(&wp->buf);
  }
  return param;
}

FSFUNC_5(fs_read, fs_read_cb, o_file, o_buf, o_pos, o_len, o_fd_offset, {
  const size_t pos = (size_t)Long_val(o_pos);
  const size_t slen = (size_t)Long_val(o_len);
  const int64_t fd_offset = Int64_val(o_fd_offset);
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  char stack_buf[ callback_type == CB_LWT ? 1 : UNIX_BUFFER_SIZE ];
  if ( slen > ULONG_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  ALLOC_WP();
  if ( ba ){
    wp->buf_contains_ba = 1;
    wp->buf.len = slen;
    wp->buf.base = Ba_buf_val(o_buf) + pos;
  }
  else {
    wp->offset = pos;
    MALLOC_UV_BUF_T(&wp->buf,slen);
    if ( wp->buf.base == NULL && slen ){
      ret = UV_ENOMEM;
      goto eend;
    }
  }
  BLOCK(uv_fs_read(loop, req, fd, &wp->buf, 1, fd_offset, cb));
  if ( callback_type == CB_LWT ){
    if ( ret >= 0 ){
      uwt__gr_register__(&wp->sbuf,o_buf);
    }
    else {
      if ( ba == 0 ){
        uwt__free_uv_buf_t(&wp->buf);
      }
    }
  }
  else {
    if ( ret >= 0 && ba == 0 && req->result > 0 ){
      memcpy(String_val(o_buf) + pos, wp->buf.base, req->result);
    }
  }
})

static value
fs_write_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  struct req * wp = r->data;
  if ( wp->sbuf != CB_INVALID ){
    uwt__gr_unregister(&wp->sbuf);
  }
  if ( wp->buf_contains_ba == 0 ){
    uwt__free_uv_buf_t(&wp->buf);
  }
  return VAL_UWT_INT_RESULT(req->result);
}

FSFUNC_5(fs_write, fs_write_cb, o_file, o_buf, o_pos, o_len, o_fd_offset, {
  const size_t pos = (size_t)Long_val(o_pos);
  const size_t slen = (size_t)Long_val(o_len);
  const int64_t fd_offset = Int64_val(o_fd_offset);
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  char stack_buf[ callback_type == CB_LWT ? 1 : UNIX_BUFFER_SIZE ];
  if ( slen > ULONG_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  ALLOC_WP();
  if ( ba ){
    wp->buf_contains_ba = 1;
    wp->buf.base = Ba_buf_val(o_buf) + pos;
    wp->buf.len = slen;
  }
  else {
    MALLOC_UV_BUF_T(&wp->buf,slen);
    if ( slen != 0 ) {
      if ( wp->buf.base == NULL ){
        ret = UV_ENOMEM;
        goto eend;
      }
      memcpy(wp->buf.base, String_val(o_buf) + pos, wp->buf.len);
    }
  }
  BLOCK(uv_fs_write(loop, req, fd, &wp->buf, 1, fd_offset, cb));
  if ( callback_type == CB_LWT ) {
    if ( ret >= 0 ){
      if ( ba ){
        uwt__gr_register__(&wp->sbuf,o_buf);
      }
    }
    else {
      if ( ba == 0 ){
        uwt__free_uv_buf_t(&wp->buf);
      }
    }
  }
})

static unsigned int
build_iovecs_stack(value o_ios, uv_buf_t * bufs, char * memory)
{
  const size_t ar_size = UMIN(Wosize_val(o_ios), (size_t)64);
  size_t avail = UNIX_BUFFER_SIZE;
  unsigned int nbufs = 0;
  size_t i;
  char * p = memory;
  for ( i = 0; i < ar_size; ++i ){
    ++nbufs;
    value cur = Field(o_ios,i);
    const size_t len = Long_val(Field(cur,2));
    if ( Tag_val(cur) == 0 ){
      bufs[i].base = Ba_buf_val(Field(cur,0)) + Long_val(Field(cur,1));
      bufs[i].len = len;
    }
    else {
      const size_t rlen = UMIN(avail, len);
      bufs[i].len = rlen;
      bufs[i].base = p;
      const char *src = String_val(Field(cur,0)) + Long_val(Field(cur,1));
      memcpy(p, src, rlen);
      p+= rlen;
      avail-= rlen;
      if ( avail == 0 ){
        break;
      }
    }
  }
  return nbufs;
}

FSFUNC_4(fs_writev, fs_write_cb, o_ios, o_iosave, o_file, o_fd_offset, {
  const size_t ar_size = Wosize_val(o_ios);
  const int64_t fd_offset = Int64_val(o_fd_offset);
  const int fd = FD_VAL(o_file);
  char stack_buf[ callback_type == CB_LWT ? 1 : UNIX_BUFFER_SIZE ];
  uv_buf_t a_bufs[ callback_type == CB_LWT ? 1 : 64 ];
  unsigned int n_bufs = ar_size;
  uv_buf_t *bufs;
  ALLOC_WP();
  if ( callback_type == CB_LWT ){
    ret = uwt__build_iovecs(o_ios, wp);
    if ( ret != 0 ){
      goto eend;
    }
    bufs = (uv_buf_t *)wp->buf.base;
  }
  else {
    n_bufs = build_iovecs_stack(o_ios, a_bufs, stack_buf);
    bufs = a_bufs;
  }
  BLOCK(uv_fs_write(loop, req, fd, bufs, n_bufs, fd_offset, cb));
  if ( callback_type == CB_LWT ){
    if ( ret >= 0 ){
      if ( o_iosave != Val_unit ){
        uwt__gr_register__(&wp->sbuf,o_iosave);
      }
    }
    else {
      uwt__free_uv_buf_t(&wp->buf);
    }
  }
})

FSFUNC_1(fs_close, runit, o_fd, {
  const int fd = FD_VAL(o_fd);
  BLOCK(uv_fs_close(loop, req, fd, cb));
})

FSFUNC_1(fs_unlink, runit, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_unlink(loop, req, STRING_VAL(o_path), cb));
  });
})

FSFUNC_2(fs_mkdir, runit, o_path, o_mode, {
  const intnat mode = Long_val(o_mode);
  if ( mode < INT_MIN || mode > INT_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  COPY_STR1(o_path,{
    BLOCK(uv_fs_mkdir(loop, req, STRING_VAL(o_path), mode, cb));
  });
})

FSFUNC_1(fs_rmdir, runit, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_rmdir(loop, req, STRING_VAL(o_path), cb));
  });
})

FSFUNC_2(fs_rename, runit, o_old, o_new, {
  COPY_STR2(o_old, o_new, {
    BLOCK(uv_fs_rename(loop, req, STRING_VAL(o_old), STRING_VAL(o_new), cb));
  });
})

FSFUNC_2(fs_link, runit, o_old, o_new, {
  COPY_STR2(o_old, o_new,{
    BLOCK(uv_fs_link(loop, req, STRING_VAL(o_old), STRING_VAL(o_new), cb));
  });
})

FSFUNC_1(fs_fsync, runit, o_fd, {
  const int fd = FD_VAL(o_fd);
  BLOCK(uv_fs_fsync(loop, req, fd, cb));
})

FSFUNC_1(fs_fdatasync, runit, o_fd, {
  const int fd = FD_VAL(o_fd);
  BLOCK(uv_fs_fdatasync(loop, req, fd, cb));
})

FSFUNC_2(fs_ftruncate, runit, o_fd,o_off, {
  const int fd = FD_VAL(o_fd);
  const int64_t off = Int64_val(o_off);
  BLOCK(uv_fs_ftruncate(loop, req, fd, off, cb));
})

static value
fs_sendfile_cb(uv_req_t * r)
{
  value param;
  const uv_fs_t* req = (uv_fs_t*)r;
  const ssize_t result = req->result;
  if ( result < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(result);
  }
  else {
    value i = caml_copy_nativeint(result);
    Begin_roots1(i);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = i;
    End_roots();
  }
  return param;
}

FSFUNC_4(fs_sendfile, fs_sendfile_cb, o_outfd, o_infd, o_offset, o_len, {
  const int outfd = FD_VAL(o_outfd);
  const int infd = FD_VAL(o_infd);
  const int64_t offset = Int64_val(o_offset);
  const int64_t len = Int64_val(o_len);
  BLOCK(uv_fs_sendfile(loop, req, outfd, infd, offset, len, cb));
})

static value
fs_scandir_cb(uv_req_t * r)
{
  uv_fs_t* req = (uv_fs_t*)r;
  value param;
  const ssize_t result = req->result;
  if ( result < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(result);
    return param;
  }
  else if ( result == 0 ){
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = Atom(0);
    return param;
  }
  else {
    CAMLparam0();
    CAMLlocal3(ar,t,s);
    ssize_t i = 0;
    uv_dirent_t dent;
    ar = caml_alloc(result,0);
    while ( UV_EOF != uv_fs_scandir_next(req, &dent) ){
      intnat d;
      s = s_caml_copy_string(dent.name);
      t = caml_alloc_small(2,0);
      Field(t,1) = s;
      switch ( dent.type ){
      case UV_DIRENT_FILE: d = 0; break;
      case UV_DIRENT_DIR: d = 1; break;
      case UV_DIRENT_CHAR: d = 2; break;
      case UV_DIRENT_BLOCK: d = 3; break;
      case UV_DIRENT_LINK: d = 4; break;
      case UV_DIRENT_FIFO: d = 5; break;
      case UV_DIRENT_SOCKET: d = 6; break;
      case UV_DIRENT_UNKNOWN: /* fall through */
      default: d = 7; break;
      }
      Field(t,0) = Val_long(d);
      Store_field(ar,i,t);
      ++i;
    }
    if ( i != req->result ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = VAL_UWT_ERROR_UWT_EFATAL;
    }
    else {
      param = caml_alloc_small(1,Ok_tag);
      Field(param,0) = ar;
    }
    CAMLreturn(param);
  }
}

FSFUNC_1(fs_scandir, fs_scandir_cb, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_scandir(loop, req, STRING_VAL(o_path), 0, cb));
  });
})

static value
fs_mkdtemp_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  value param;
  const ssize_t result = req->result;
  if ( result < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(result);
  }
  else if ( req->path == NULL || req->path[0] == '\0' ){
    /* Protect against outdated or buggy mkdtemp implementations. The
     * original errno value is unfortunately lost. */
    param = caml_alloc_small(1, Error_tag);
    Field(param,0) =  VAL_UWT_ERROR_UNKNOWN;
  }
  else {
    value s = caml_copy_string(req->path);
    Begin_roots1(s);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = s;
    End_roots();
  }
  return param;
}

FSFUNC_1(fs_mkdtemp, fs_mkdtemp_cb, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_mkdtemp(loop, req, STRING_VAL(o_path), cb));
  });
})

static value
fs_readlink_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  value param;
  const ssize_t result = req->result;
  if ( result < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(result);
  }
  else {
    /* libuv has added the trailing zero for us */
    value s = s_caml_copy_string(req->ptr);
    Begin_roots1(s);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = s;
    End_roots();
  }
  return param;
}

FSFUNC_1(fs_readlink, fs_readlink_cb, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_readlink(loop, req, STRING_VAL(o_path), cb));
  });
})

static const int
access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

FSFUNC_2(fs_access, runit, o_path, o_list, {
  const int fl = SAFE_CONVERT_FLAG_LIST(o_list, access_permission_table);
  COPY_STR1(o_path,{
    BLOCK(uv_fs_access(loop, req, STRING_VAL(o_path), fl, cb));
  });
})

FSFUNC_2(fs_chmod, runit, o_path, o_mode, {
  const intnat mode = Long_val(o_mode);
  if ( mode < INT_MIN || mode > INT_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  COPY_STR1(o_path,{
    BLOCK(uv_fs_chmod(loop, req, STRING_VAL(o_path), mode, cb));
  });
})

FSFUNC_2(fs_fchmod, runit, o_fd, o_mode, {
  const int fd = FD_VAL(o_fd);
  const intnat mode = Long_val(o_mode);
  if ( mode < INT_MIN || mode > INT_MAX ){
    ret = UV_EINVAL;
    goto eend;
  }
  BLOCK(uv_fs_fchmod(loop, req, fd, mode, cb));
})

DISABLE_WARNING_TYPE_LIMIT();
FSFUNC_3(fs_chown, runit, o_path, o_uid, o_gid, {
  const intnat r_uid = Long_val(o_uid);
  const intnat r_gid = Long_val(o_gid);
  const uv_uid_t uid = r_uid;
  const uv_gid_t gid = r_gid;
  if ( r_uid != uid || gid != r_gid || ((uv_uid_t)-1 >= 0 && r_uid < 0) ||
       ((uv_gid_t)-1 >= 0 && r_gid < 0) ){
    ret = UV_EINVAL;
    goto eend;
  }
  COPY_STR1(o_path,{
    BLOCK(uv_fs_chown(loop, req, STRING_VAL(o_path), uid, gid, cb));
  });
})

FSFUNC_3(fs_fchown, runit, o_fd, o_uid, o_gid, {
  const int fd = FD_VAL(o_fd);
  const intnat r_uid = Long_val(o_uid);
  const intnat r_gid = Long_val(o_gid);
  const uv_uid_t uid = r_uid;
  const uv_gid_t gid = r_gid;
  if ( r_uid != uid || gid != r_gid || ((uv_uid_t)-1 >= 0 && r_uid < 0) ||
       ((uv_gid_t)-1 >= 0 && r_gid < 0) ){
    ret = UV_EINVAL;
    goto eend;
  }
  BLOCK(uv_fs_fchown(loop, req, fd, uid, gid, cb));
})

#if HAVE_DECL_UV_FS_LCHOWN
FSFUNC_3(fs_lchown, runit, o_path, o_uid, o_gid, {
  const intnat r_uid = Long_val(o_uid);
  const intnat r_gid = Long_val(o_gid);
  const uv_uid_t uid = r_uid;
  const uv_gid_t gid = r_gid;
  if ( r_uid != uid || gid != r_gid || ((uv_uid_t)-1 >= 0 && r_uid < 0) ||
       ((uv_gid_t)-1 >= 0 && r_gid < 0) ){
    ret = UV_EINVAL;
    goto eend;
  }
  COPY_STR1(o_path,{
    BLOCK(uv_fs_lchown(loop, req, STRING_VAL(o_path), uid, gid, cb));
  });
})
#else
NO_FS_3(fs_lchown)
#endif
POP_WARNING();

FSFUNC_3(fs_utime, runit, o_p, o_atime, o_mtime, {
  const double atime = Double_val(o_atime);
  const double mtime = Double_val(o_mtime);
  COPY_STR1(o_p,{
    BLOCK(uv_fs_utime(loop, req, STRING_VAL(o_p), atime, mtime, cb));
  });
})

FSFUNC_3(fs_futime, runit, o_fd, o_atime, o_mtime, {
  const double atime = Double_val(o_atime);
  const double mtime = Double_val(o_mtime);
  const int fd = FD_VAL(o_fd);
  BLOCK(uv_fs_futime(loop, req, fd, atime, mtime, cb));
})

FSFUNC_3(fs_symlink, runit, o_opath, o_npath, o_mode, {
  int flag;
  switch( Long_val(o_mode) ){
  case 1: flag = UV_FS_SYMLINK_DIR; break;
  case 2: flag = UV_FS_SYMLINK_JUNCTION; break;
  default: assert(false); /* fall */
  case 0: flag = 0; break;
  }
  COPY_STR2(o_opath, o_npath,{
    BLOCK(uv_fs_symlink(loop, req, STRING_VAL(o_opath), STRING_VAL(o_npath),
                        flag, cb ));
  });
})

static value
fs_stat_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  value param;
  const ssize_t result = req->result;
  if ( result < 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(result);
  }
  else {
    value st = uwt__stat_to_value(&req->statbuf);
    Begin_roots1(st);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = st;
    End_roots();
  }
  return param;
}

FSFUNC_1(fs_stat, fs_stat_cb, o_file, {
  COPY_STR1(o_file,{
    BLOCK(uv_fs_stat(loop, req, STRING_VAL(o_file), cb));
  });
})

FSFUNC_1(fs_lstat, fs_stat_cb, o_file, {
  COPY_STR1(o_file,{
    BLOCK(uv_fs_lstat(loop, req, STRING_VAL(o_file), cb));
  });
})

FSFUNC_1(fs_fstat, fs_stat_cb, o_file, {
  int fd = FD_VAL(o_file);
  BLOCK(uv_fs_fstat(loop, req, fd, cb));
})

FSFUNC_1(fs_realpath, fs_readlink_cb, o_path, {
  COPY_STR1(o_path,{
    BLOCK(uv_fs_realpath(loop, req, STRING_VAL(o_path), cb));
  });
})

#if HAVE_DECL_UV_FS_COPYFILE

/* ugly, but using #ifdef inside a macro is not portable,... */
#ifndef UV_FS_COPYFILE_FICLONE
#define UWT_FS_COPYFILE_FICLONE 0
#else
#define UWT_FS_COPYFILE_FICLONE UV_FS_COPYFILE_FICLONE
#endif

#ifndef UV_FS_COPYFILE_FICLONE_FORCE
#define UWT_FS_COPYFILE_FICLONE_FORCE 0
#else
#define UWT_FS_COPYFILE_FICLONE_FORCE UV_FS_COPYFILE_FICLONE_FORCE
#endif

FSFUNC_3(fs_copyfile, runit, o_old, o_new, o_flags,{
  const int copy_flags = Long_val(o_flags);
  int flags = 0 ;
  if ( copy_flags & 1 ){
    flags |= UV_FS_COPYFILE_EXCL;
  }
  if ( copy_flags & 2 ){
    flags |= UWT_FS_COPYFILE_FICLONE; /* emulate, if libuv version too low */
  }
  if ( copy_flags & 4 ){
    if ( !UWT_FS_COPYFILE_FICLONE_FORCE ){
      ret = UV_ENOSYS;
    }
    flags |= UWT_FS_COPYFILE_FICLONE_FORCE;
  }
  if ( ret != UV_ENOSYS ){
    COPY_STR2(o_old, o_new,{
        BLOCK(uv_fs_copyfile(loop, req, STRING_VAL(o_old), STRING_VAL(o_new),
                             flags, cb));
      });
  }
})
#undef UWT_FS_COPYFILE_FICLONE
#undef UWT_FS_COPYFILE_FICLONE_FORCE
#else
NO_FS_3(fs_copyfile)
#endif /* HAVE_DECL_UV_FS_COPYFILE */

#ifdef DEF_O_NONBLOCK
#undef DEF_O_NONBLOCK
#undef O_NONBLOCK
#endif
#ifdef DEF_O_DSYNC
#undef DEF_O_DSYNC
#undef O_DSYNC
#endif
#ifdef DEF_O_SYNC
#undef DEF_O_SYNC
#undef O_SYNC
#endif
#ifdef DEF_O_RSYNC
#undef DEF_O_RSYNC
#undef O_RSYNC
#endif

#undef ALLOC_WP
#undef BLOCK
#undef CB_LWT
#undef CB_SYNC
#undef CHECK_STRING
#undef COPY_STR1
#undef COPY_STR2
#undef FSFUNC_1
#undef FSFUNC_2
#undef FSFUNC_3
#undef FSFUNC_4
#undef FSFUNC_5
#undef MALLOC_UV_BUF_T
#undef R_WRAP_LWT
#undef R_WRAP_SYNC
#undef STRING_VAL
#undef runit
#undef NO_FS_3
