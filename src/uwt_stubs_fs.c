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

#include "uwt_stubs_fs.h"

#define CHECK_STRING(s)                         \
  do {                                          \
    if ( !uwt_is_safe_string(s) ){              \
      o_ret = VAL_UWT_INT_RESULT_ECHARSET;      \
      goto nomem;                               \
    }                                           \
  } while (0)

#define BLOCK(code)                             \
  do {                                          \
    if ( callback_type == CB_SYNC ){            \
      caml_enter_blocking_section();            \
    }                                           \
    libuv_called = true;                        \
    do code while(0);                           \
    if ( callback_type == CB_SYNC ){            \
      caml_leave_blocking_section();            \
    }                                           \
  } while (0)

#define COPY_STR1(x,code)                       \
  do {                                          \
    char * x##_dup = NULL;                      \
    CHECK_STRING(x);                            \
    if ( callback_type == CB_SYNC ){            \
      x ## _dup = s_strdup(String_val(x));      \
      if ( x ## _dup == NULL ){                 \
        o_ret = VAL_UWT_INT_RESULT_ENOMEM;      \
        goto nomem;                             \
      }                                         \
    }                                           \
    do code while(0);                           \
    if ( callback_type == CB_SYNC ){            \
      free( x##_dup);                           \
    }                                           \
  } while(0)

#define COPY_STR2(x,y,code)                     \
  do {                                          \
    char * x##_dup = NULL;                      \
    char * y##_dup = NULL;                      \
    CHECK_STRING(x);                            \
    CHECK_STRING(y);                            \
    if ( callback_type == CB_SYNC ){            \
      x ## _dup = s_strdup(String_val(x));      \
      if ( x ## _dup == NULL ){                 \
        o_ret = VAL_UWT_INT_RESULT_ENOMEM;      \
        goto nomem;                             \
      }                                         \
      y ## _dup = s_strdup(String_val(y));      \
      if ( y ## _dup == NULL ){                 \
        free(x ## _dup);                        \
        x ## _dup = NULL;                       \
        o_ret = VAL_UWT_INT_RESULT_ENOMEM;      \
        goto nomem;                             \
      }                                         \
    }                                           \
    do code while(0);                           \
    if ( callback_type == CB_SYNC ){            \
      free( x##_dup );                          \
      free( y##_dup );                          \
    }                                           \
  } while(0)

#define STRING_VAL(x)                           \
  (callback_type == CB_SYNC ?                   \
    (x ## _dup) :                               \
   String_val(x))

#define R_WRAP(name,tz,code)                              \
  value o_ret;                                            \
  struct loop * wp_loop = Loop_val(o_loop);               \
  struct req * wp_req = Req_val(o_req);                   \
  uv_fs_t * req;                                          \
  if (unlikely( wp_loop == NULL ||                        \
                wp_req == NULL ||                         \
                wp_loop->init_called == 0 ||              \
                (req = (uv_fs_t*)wp_req->req) == NULL ||  \
                wp_req->in_use == 1 )){                   \
    o_ret = VAL_UWT_INT_RESULT_UWT_EFATAL;                \
  }                                                       \
  else {                                                  \
    uv_loop_t * loop = &wp_loop->loop;                    \
    bool libuv_called = false;                            \
    int ret = UV_UWT_EFATAL;                              \
    const int callback_type = wp_loop->loop_type;         \
    const uv_fs_cb cb =                                   \
      callback_type == CB_SYNC ? NULL :                   \
      ((uv_fs_cb)uwt__req_callback);                      \
    GR_ROOT_ENLARGE();                                    \
    do                                                    \
      code                                                \
        while(0);                                         \
    if ( libuv_called ){                                  \
      wp_req->clean_cb = (clean_cb)uv_fs_req_cleanup;     \
    }                                                     \
    if ( ret >= 0  ){                                     \
      wp_req->c_cb = tz;                                  \
      wp_req->cb_type = callback_type;                    \
      if ( callback_type != CB_SYNC ){                    \
        uwt__gr_register(&wp_req->cb,o_cb);               \
        wp_req->in_use = 1;                               \
        o_ret = Val_long(0);                              \
      }                                                   \
      else {                                              \
        o_ret = Val_long(ret);                            \
      }                                                   \
    }                                                     \
    else {                                                \
      o_ret = Val_uwt_int_result(ret);                    \
  nomem:                                                  \
    ATTR_UNUSED;                                          \
      Field(o_req,1) = 0;                                 \
      uwt__req_free(wp_req);                              \
    }                                                     \
  }                                                       \
  CAMLreturn(o_ret);                                      \
}

#define FSFUNC_4(name,tz,a,b,c,d,code)                                  \
  CAMLprim value                                                        \
  uwt_ ## name ## _byte(value *a, int argn)                             \
  {                                                                     \
    (void)argn;                                                         \
    assert( argn == 7 );                                                \
    return (uwt_ ## name ## _native(a[0],a[1],a[2],a[3],                \
                                    a[4],a[5],a[6]));                   \
  }                                                                     \
  CAMLprim value                                                        \
  uwt_ ## name ## _native (value a, value b, value c,value d,           \
                           value o_loop, value o_req, value o_cb ){     \
    CAMLparam5(a,b,o_loop,o_req,o_cb);                                  \
    CAMLxparam2(c,d);                                                   \
    R_WRAP(name,tz,code)

#define FSFUNC_3(name,tz,a,b,c,code)                                  \
  CAMLprim value                                                      \
  uwt_ ## name ## _byte(value *a, int argn)                           \
  {                                                                   \
    (void)argn;                                                       \
    assert( argn == 6 );                                              \
    return (uwt_ ## name ## _native(a[0],a[1],a[2],a[3],a[4],a[5]));  \
  }                                                                   \
  CAMLprim value                                                      \
  uwt_ ## name ## _native (value a, value b, value c,                 \
                           value o_loop, value o_req, value o_cb ){   \
    CAMLparam5(a,b,o_loop,o_req,o_cb);                                \
    CAMLxparam1(c);                                                   \
    R_WRAP(name,tz,code)

#define FSFUNC_2(name,tz,a,b,code)                        \
  CAMLprim value                                          \
  uwt_ ## name  (value a,value b,                         \
                 value o_loop, value o_req, value o_cb ){ \
  CAMLparam5(a,b,o_loop,o_req,o_cb);                      \
  R_WRAP(name,tz,code)

#define FSFUNC_1(name,tz,a,code)                          \
  CAMLprim value                                          \
  uwt_ ## name  (value a,                                 \
                 value o_loop, value o_req, value o_cb ){ \
  CAMLparam4(a,o_loop,o_req,o_cb);                        \
  R_WRAP(name,tz,code)

#define runit uwt__ret_uv_fs_result_unit

static const int open_flag_table[16] = {
#ifdef _WIN32
  _O_RDONLY, _O_WRONLY, _O_RDWR, 0, _O_CREAT , _O_EXCL , _O_TRUNC, _O_APPEND,
  0, 0, 0, 0,
  _O_TEMPORARY, _O_SHORT_LIVED, _O_SEQUENTIAL, _O_RANDOM
#else
#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
  O_RDONLY, O_WRONLY, O_RDWR, O_NONBLOCK, O_CREAT , O_EXCL , O_TRUNC, O_APPEND,
  O_NOCTTY, O_DSYNC, O_SYNC, O_RSYNC,
  0, 0, 0, 0
#endif
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
      uv_fs_t* creq = malloc(sizeof *creq);
      if ( creq ){
        int cret = uv_fs_close(req->loop, creq, fd, fs_open_clean_cb);
        if ( cret < 0 ){
          free(creq);
        }
      }
      ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_UNKNOWN;
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

FSFUNC_3(fs_open,fs_open_cb,o_name,o_flag_list,o_perm,{
  const int flags = SAFE_CONVERT_FLAG_LIST(o_flag_list,open_flag_table);
  COPY_STR1(o_name,{
      BLOCK({
          ret = uv_fs_open(loop,
                           req,
                           STRING_VAL(o_name),
                           flags,
                           Long_val(o_perm),
                           cb);
        });
    });
  })

static value
fs_read_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  value param;
  const ssize_t result = req->result;
  struct req * wp = r->data;
  if ( result == 0 ){
    param = Val_long(0);
  }
  else if ( result < 0 ){
    param = Val_uwt_int_result(result);
  }
  else if ( (size_t)result > wp->buf.len ||
            wp->buf.base == NULL || wp->sbuf == CB_INVALID ){
    param = VAL_UWT_INT_RESULT_UWT_EFATAL;
  }
  else {
    param = Val_long(result);
    if ( wp->buf_contains_ba == 0 ){
      value o = GET_CB_VAL(wp->sbuf);
      memcpy(String_val(o) + wp->offset,
             wp->buf.base,
             result);
    }
  }
  return param;
}

FSFUNC_4(fs_read,fs_read_cb,o_file,o_buf,o_offset,o_len,{
  const size_t slen = (size_t)Long_val(o_len);
  struct req * wp = wp_req;
  size_t offset = Long_val(o_offset);
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  if ( slen > ULONG_MAX ){
    ret = UV_EINVAL;
  }
  else {
    if ( ba ){
      wp->buf_contains_ba = 1;
      wp->buf.len = slen;
      wp->buf.base = Ba_buf_val(o_buf) + offset;
    }
    else {
      uwt__malloc_uv_buf_t(&wp->buf,slen,wp->cb_type);
    }
    if ( slen && wp->buf.base == NULL ){
      ret = UV_ENOMEM;
    }
    else {
      wp->offset = offset;
      BLOCK({
          ret = uv_fs_read(loop, req, fd, &wp->buf, 1, -1, cb);
        });
      if ( ret >= 0 ){
        uwt__gr_register(&wp->sbuf,o_buf);
      }
      else {
        if ( !ba ){
          uwt__free_uv_buf_t(&wp->buf,wp->cb_type);
        }
        wp->offset = 0;
        wp->buf_contains_ba = 0;
        wp->buf.len = 0;
        wp->buf.base = NULL;
      }
    }
  }
  })

static value
fs_write_cb(uv_req_t * r)
{
  const uv_fs_t* req = (uv_fs_t*)r;
  const ssize_t result = req->result;
  value erg;
  const struct req * wp = req->data;
  if ( result < 0 ){
    erg = Val_uwt_int_result(result);
  }
  else if ( (size_t)result > wp->buf.len ){
    erg = VAL_UWT_INT_RESULT_UWT_EFATAL;
  }
  else {
    erg = Val_long(result);
  }
  return erg;
}

FSFUNC_4(fs_write,
         fs_write_cb,
         o_file,
         o_buf,
         o_pos,
         o_len,{
  const size_t slen = (size_t)Long_val(o_len);
  struct req * wp = wp_req;
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  if ( slen > ULONG_MAX ){
    ret = UV_EINVAL;
  }
  else {
    if ( ba ){
      wp->buf_contains_ba = 1;
      wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
      wp->buf.len = slen;
    }
    else {
      uwt__malloc_uv_buf_t(&wp->buf,slen,wp->cb_type);
    }
    if ( slen && wp->buf.base == NULL ){
      ret = UV_ENOMEM;
    }
    else {
      if ( slen && ba == 0 ){
        memcpy(wp->buf.base,
               String_val(o_buf) + Long_val(o_pos),
               slen);
      }
      BLOCK({
          ret = uv_fs_write(loop, req, fd, &wp->buf, 1, -1, cb);
        });
      if ( ret >= 0 ){
        if ( ba ){
          uwt__gr_register(&wp->sbuf,o_buf);
        }
      }
      else {
        if ( ba == 0 ){
          uwt__free_uv_buf_t(&wp->buf,wp->cb_type);
        }
        wp->buf_contains_ba = 0;
        wp->buf.base = NULL;
        wp->buf.len = 0;
      }
    }
  }
})

FSFUNC_1(fs_close,runit,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({
        ret = uv_fs_close(loop,req,fd,cb);
      });
})

FSFUNC_1(fs_unlink,runit,o_path,{
  COPY_STR1(o_path,{
    BLOCK({
      ret = uv_fs_unlink(loop,req,STRING_VAL(o_path),cb);
    });
  });
})

FSFUNC_2(fs_mkdir,runit,o_path,o_mode,{
  COPY_STR1(o_path,{
      BLOCK({
          ret = uv_fs_mkdir(loop,req,STRING_VAL(o_path),Long_val(o_mode),cb);});
    });
})

FSFUNC_1(fs_rmdir,runit,o_path,{
  COPY_STR1(o_path,{
      BLOCK({ret = uv_fs_rmdir(loop,req,STRING_VAL(o_path),cb);});
    });
})

FSFUNC_2(fs_rename,runit,o_old,o_new,{
  COPY_STR2(o_old,o_new,{
      BLOCK({ret = uv_fs_rename(loop,req,STRING_VAL(o_old),
                                STRING_VAL(o_new),cb);});
    });
})

FSFUNC_2(fs_link,runit,o_old,o_new,{
    COPY_STR2(o_old,o_new,{
        BLOCK({ret = uv_fs_link(loop,req,STRING_VAL(o_old),
                                STRING_VAL(o_new),cb);});
      });
})

FSFUNC_1(fs_fsync,runit,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fsync(loop,req,fd,cb);});
})

FSFUNC_1(fs_fdatasync,runit,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fdatasync(loop,req,fd,cb);});
})

FSFUNC_2(fs_ftruncate,runit,o_fd,o_off,{
  const int fd = FD_VAL(o_fd);
  const int64_t off = Int64_val(o_off);
  BLOCK({ret = uv_fs_ftruncate(loop,req,fd,off,cb);});
})

static value
fs_sendfile_cb(uv_req_t * r)
{
  value param;
  const uv_fs_t* req = (uv_fs_t*)r;
  const ssize_t result = req->result;
  if ( result < 0 ){ /* error */
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

FSFUNC_4(fs_sendfile,fs_sendfile_cb,o_outfd,o_infd,o_offset,o_len,{
  const int outfd = FD_VAL(o_outfd);
  const int infd = FD_VAL(o_infd);
  const int64_t offset = Int64_val(o_offset);
  const int64_t len = Int64_val(o_len);
  BLOCK({ret = uv_fs_sendfile(loop,
                              req,
                              outfd,
                              infd,
                              offset,
                              len,
                              cb);});
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

FSFUNC_1(fs_scandir,fs_scandir_cb,o_path,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_scandir(loop,req,STRING_VAL(o_path),0,cb);});
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
  else if ( req->path == NULL ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) =  VAL_UWT_ERROR_UWT_EFATAL;
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

FSFUNC_1(fs_mkdtemp,fs_mkdtemp_cb,o_path,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_mkdtemp(loop,req,STRING_VAL(o_path),cb);});
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
  else if ( req->ptr == NULL ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = VAL_UWT_ERROR_UWT_EFATAL;
  }
  else {
    /* libuv has added the trailing zero for us */
    value s = caml_copy_string(req->ptr);
    Begin_roots1(s);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = s;
    End_roots();
  }
  return param;
}

FSFUNC_1(fs_readlink,fs_readlink_cb,o_path,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_readlink(loop,req,STRING_VAL(o_path),
                                    cb);});
      });
})

static const int
access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

FSFUNC_2(fs_access,runit,o_path,o_list,{
    const int fl = SAFE_CONVERT_FLAG_LIST(o_list, access_permission_table);
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_access(loop,
                                  req,
                                  STRING_VAL(o_path),
                                  fl,
                                  cb);});
    });
})

FSFUNC_2(fs_chmod,runit,o_path,o_mode,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_chmod(loop,
                                 req,
                                 STRING_VAL(o_path),
                                 Long_val(o_mode),
                                 cb
                                 );});
      });
})

FSFUNC_2(fs_fchmod,runit,o_fd,o_mode,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fchmod(loop,
                              req,
                              fd,
                              Long_val(o_mode),
                              cb);});
})

FSFUNC_3(fs_chown,runit,o_path,o_uid,o_gid,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_chown(loop,
                                 req,
                                 STRING_VAL(o_path),
                                 Long_val(o_uid),
                                 Long_val(o_gid),
                                 cb);});
      });
})

FSFUNC_3(fs_fchown,runit,o_fd,o_uid,o_gid,{
    const int fd = FD_VAL(o_fd);
    BLOCK({
        ret = uv_fs_fchown(loop,
                           req,
                           fd,
                           Long_val(o_uid),
                           Long_val(o_gid),
                           cb);
      });
})

FSFUNC_3(fs_utime,runit,o_p,o_atime,o_mtime,{
  const double atime = Double_val(o_atime);
  const double mtime = Double_val(o_mtime);
  COPY_STR1(o_p,{
      BLOCK({ret = uv_fs_utime(loop,
                               req,
                               STRING_VAL(o_p),
                               atime,
                               mtime,
                               cb);
        });
    });
})

FSFUNC_3(fs_futime,runit,o_fd,o_atime,o_mtime,{
  const double atime = Double_val(o_atime);
  const double mtime = Double_val(o_mtime);
  const int fd = FD_VAL(o_fd);
  BLOCK({ret = uv_fs_futime(loop,
                            req,
                            fd,
                            atime,
                            mtime,
                            cb);});
})

FSFUNC_3(fs_symlink,runit,o_opath,o_npath,o_mode,{
  int flag;
  switch( Long_val(o_mode) ){
  case 1: flag = UV_FS_SYMLINK_DIR; break;
  case 2: flag = UV_FS_SYMLINK_JUNCTION; break;
  default: assert(false); /* fall */
  case 0: flag = 0; break;
  }
  COPY_STR2(o_opath,o_npath,{
      BLOCK({ret = uv_fs_symlink(loop,
                                 req,
                                 STRING_VAL(o_opath),
                                 STRING_VAL(o_npath),
                                 flag,
                                 cb
                                 );});
    });
})

CAMLprim value
uwt_get_fs_result(value o_req)
{
  CAMLparam1(o_req);
  struct req * wp = Req_val(o_req);
  value ret = Val_unit;
  if ( wp == NULL || wp->req == NULL || wp->c_cb == NULL ){
    caml_invalid_argument("uwt_get_fs_result");
  }
  if ( wp->c_cb == uwt__ret_uv_fs_result_unit ){
    ret = VAL_UWT_UNIT_RESULT(((uv_fs_t*)(wp->req))->result);
  }
  else {
    ret = wp->c_cb(wp->req);
  }
  Field(o_req,1) = 0;
  uwt__req_free(wp);
  CAMLreturn(ret);
}


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

FSFUNC_1(fs_stat,fs_stat_cb,o_file,{
    COPY_STR1(o_file,{
        BLOCK({
            ret = uv_fs_stat(loop,req,STRING_VAL(o_file),cb);
              });
      });
})

FSFUNC_1(fs_lstat,fs_stat_cb,o_file,{
    COPY_STR1(o_file,{
        BLOCK({
            ret = uv_fs_lstat(loop,req,STRING_VAL(o_file),cb);});
      });
})

FSFUNC_1(fs_fstat,fs_stat_cb,o_file,{
    int fd = FD_VAL(o_file);
    BLOCK({
        ret = uv_fs_fstat(loop,req,fd,cb);
          });
})

#if HAVE_DECL_UV_FS_REALPATH
FSFUNC_1(fs_realpath,fs_readlink_cb,o_path,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_realpath(loop,req,STRING_VAL(o_path),
                                    cb);});
      });
})
#endif


CAMLprim value
uwt_fs_free(value o_req)
{
  struct req * wp = Req_val(o_req);
  if ( wp != NULL ){
    Field(o_req,1) = 0;
    uwt__req_free(wp);
  }
  return Val_unit;
}
#undef runit
#undef CHECK_STRING
#undef BLOCK
#undef COPY_STR1
#undef COPY_STR2
#undef STRING_VAL
#undef R_WRAP
#undef FSFUNC_4
#undef FSFUNC_3
#undef FSFUNC_2
#undef FSFUNC_1
