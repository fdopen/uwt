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

#include "uwt_stubs_worker.h"

static void
common_after_work_cb(uv_work_t *req, int status)
{
  GET_RUNTIME();
  struct req * r = NULL;
  if (unlikely( !req || (r = req->data) == NULL ||
                r->cb == CB_INVALID || r->c_cb == NULL )){
    DEBUG_PF("fatal, no cb");
    uwt__req_free_most(r);
  }
  else {
    CAMLparam0();
    CAMLlocal1(param);
    r = req->data;
    r->in_cb = 1;
    if ( status != 0 ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = Val_uwt_error(status);
    }
    else {
      param = r->c_cb((uv_req_t *)req);
      if ( r->buf_contains_ba == 1 ){
        value t = caml_alloc_small(1,Ok_tag);
        Field(t,0) = param;
        param = t;
      }
    }
    value exn = CAML_CALLBACK1(r,cb,param);
    if ( Is_exception_result(exn) ){
      uwt__add_exception(r->loop,exn);
    }
    r->in_cb = 0;
    uwt__req_free_most(r);
    CAMLreturn0;
  }
}

static int
uwt_add_worker_common(value o_uwt,
                      cb_cleaner cleaner,
                      cb_worker worker,
                      cb_camlval camlval,
                      void * p1,
                      void * p2,
                      bool wrap)
{
  CAMLparam1(o_uwt);
  struct loop * loop = Loop_val(Field(o_uwt,0));
  struct req * req = Req_val(Field(o_uwt,1));
  int erg;
  if (unlikely( loop == NULL || req == NULL || loop->init_called == 0 ||
                req->req == NULL || req->in_use == 1 )){
    erg = UV_UWT_EFATAL;
    if ( cleaner && req && req->req ){
      cleaner((void*)req->req);
    }
    goto endp;
  }
  GR_ROOT_ENLARGE();

  req->c.p1 = p1;
  req->c.p2 = p2;
  if ( wrap ){
    req->buf_contains_ba = 1;
  }
  erg = uv_queue_work(&loop->loop,
                      (uv_work_t*)req->req,
                      worker,
                      common_after_work_cb);
  if ( erg < 0 ){
    if ( cleaner != NULL ){
      cleaner((void*)req->req);
    }
    value o_req = Field(o_uwt,1);
    Field(o_req,1) = 0;
    req->finalize_called = 1;
    uwt__req_free_most(req);
  }
  else {
    value o_cb = Field(o_uwt,2);
    uwt__gr_register(&req->cb,o_cb);
    req->c_cb = camlval;
    req->clean_cb = cleaner;
    req->in_use = 1;
  }
 endp:
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}

int uwt_add_worker(value a,
                   cb_cleaner b,
                   cb_worker c,
                   cb_camlval d,
                   void * p1,
                   void * p2)
{
  return (uwt_add_worker_common(a,b,c,d,p1,p2,true));
}

int uwt_add_worker_result(value a,
                          cb_cleaner b,
                          cb_worker c,
                          cb_camlval d,
                          void * p1,
                          void * p2)
{
  return (uwt_add_worker_common(a,b,c,d,p1,p2,false));
}

#ifdef ARCH_SIXTYFOUR
static FORCE_INLINE int64_t voids_to_int64_t(const struct worker_params * x)
{
  _Static_assert(sizeof(int64_t) == sizeof(void *),"wrong sizes");
  return ((intptr_t)x->p1);
}
static FORCE_INLINE void int64_t_to_voids(int64_t val, struct worker_params * x)
{
  x->p1 = (void*)val;
}
#else
union dummy_union {
    int64_t i64;
    struct worker_params p;
};
static FORCE_INLINE int64_t voids_to_int64_t(const struct worker_params *x)
{
  union dummy_union y;
  _Static_assert(sizeof(struct worker_params) >= sizeof(int64_t),"wrong sizes");
  y.p = *x;
  return y.i64;
}
static FORCE_INLINE void int64_t_to_voids(int64_t val, struct worker_params * x)
{
  union dummy_union y;
  y.i64 = val;
  *x = y.p;
}
#endif

static void
lseek_work_cb(uv_work_t *req)
{
  struct req * r = req->data;
  const int fd = r->c_param;
  int64_t offset = voids_to_int64_t(&r->c);
#ifdef _WIN32
  const DWORD whence = r->offset;
  HANDLE handle = (HANDLE)(0 + _get_osfhandle(fd));
  if ( handle == INVALID_HANDLE_VALUE ){
    r->offset = UV_EBADF;
    offset = -1;
  }
  else {
    LARGE_INTEGER distance_to_move;
    LARGE_INTEGER new_position;
    distance_to_move.QuadPart = offset;
    if ( SetFilePointerEx(handle,distance_to_move,&new_position,whence) ){
      offset = new_position.QuadPart;
    }
    else {
      DWORD er =  GetLastError();
      r->offset = uwt_translate_sys_error(er);
      offset = -1;
    }
  }
#else
  const int whence = r->offset;
  errno = 0;
  offset = lseek(fd,offset,whence);
  r->offset = -errno;
#endif
  int64_t_to_voids(offset,&r->c);
}

static value
lseek_cb(uv_req_t * req)
{
  const struct req * r = req->data;
  value ret;
  const int64_t offset = voids_to_int64_t(&r->c);
  if ( offset == -1 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(r->offset);
  }
  else {
    value p = caml_copy_int64(offset);
    Begin_roots1(p);
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = p;
    End_roots();
  }
  return ret;
}

#ifdef _WIN32
static const int seek_command_table[] = {
  FILE_BEGIN, FILE_CURRENT, FILE_END
};
#else
static const int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};
#endif

/*
  lseek is used inside Uwt_io. Therefore, I can't be seperated like the other
  unix functions
*/
CAMLprim value
uwt_lseek_native(value o_fd, value o_pos, value o_mode, value o_loop,
                 value o_req, value o_cb)
{
  struct loop * loop = Loop_val(o_loop);
  struct req * req = Req_val(o_req);
  RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req);
  CAMLparam3(o_loop,o_req,o_cb);
  const int fd = FD_VAL(o_fd);
  const int64_t offset = Int64_val(o_pos);
  const int sct_i = Long_val(o_mode);
  if ( sct_i < 0 || (size_t)sct_i >= AR_SIZE(seek_command_table) ){
    assert(false);
    caml_failwith("invalid lseek mode");
  }
  const int whence = seek_command_table[sct_i];

  GR_ROOT_ENLARGE();

  /* be careful: everything the worker thread needs, must be set
     before uv_queue_work is called */
  req->c_param = fd;
  req->offset = whence;
  int64_t_to_voids(offset,&req->c);
  const int erg = uv_queue_work(&loop->loop,
                                (uv_work_t*)req->req,
                                lseek_work_cb,
                                common_after_work_cb);
  if ( erg < 0 ){
    Field(o_req,1) = 0;
    uwt__req_free(req);
  }
  else {
    uwt__gr_register(&req->cb,o_cb);
    req->c_cb = lseek_cb;
    req->in_use = 1;
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}
BYTE_WRAP6(uwt_lseek)
