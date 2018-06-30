/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_worker.h"

static void
uwork_cb(uv_work_t *req, int status)
{
  REQ_CB_INIT(req);
  struct req * r = req->data;
  value param;
  if ( status != 0 ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = Val_uwt_error(status);
  }
  else {
    param = r->c_cb((uv_req_t *)req);
    if ( r->buf_contains_ba == 1 ){
      Begin_roots1(param);
      value t = caml_alloc_small(1,Ok_tag);
      Field(t,0) = param;
      param = t;
      End_roots();
    }
  }
  if ( r->clean_cb != NULL ){
    r->clean_cb(r->req);
  }
  REQ_CB_CALL(param);
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
  if (unlikely( worker == NULL || camlval == NULL )){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  uv_loop_t * loop = Uv_loop_val(Field(o_uwt,0));
  struct req * req = uwt__req_create_null(UV_WORK);
  value o_req = Field(o_uwt,1);
  Field(o_req,0) = (intnat) req;

  if (unlikely(req == NULL)){
    if ( cleaner ){
      struct req dummy;
      uv_work_t w;
      memset(&dummy, 0, sizeof dummy);
      dummy.c.p1 = p1;
      dummy.c.p2 = p2;
      dummy.req = (uv_req_t*)&w;
      memset(&w, 0, sizeof w);
      w.data = &dummy;
      w.type = UV_WORK;
      cleaner((uv_req_t*)&w);
    }
    return VAL_UWT_INT_RESULT_ENOMEM;
  }

  req->c.p1 = p1;
  req->c.p2 = p2;
  req->clean_cb = cleaner;
  req->c_cb = camlval;
  if ( wrap ){
    req->buf_contains_ba = 1;
  }
  const int erg = uv_queue_work(loop, (uv_work_t*)req->req, worker, uwork_cb);
  if ( erg < 0 ){
    if ( cleaner ){
      cleaner(req->req);
    }
    uwt__req_free(req);
  }
  else {
    value o_cb = Field(o_uwt,2);
    uwt__gr_register__(&req->cb, o_cb);
  }
  return VAL_UWT_UNIT_RESULT(erg);
}

CAMLprim value
uwt_workreq_create(value o_unit)
{
  (void) o_unit;
  GR_ROOT_ENLARGE();
  value r =caml_alloc_small(1,Abstract_tag);
  Field(r,0) = 0;
  return r;
}

CAMLprim value
uwt_workreq_cancel_na(value o_req)
{
  struct req * wp = (void*)Field(o_req,0);
  if ( wp && uv_cancel(wp->req) == 0 ){
    return Val_long(1);
  }
  return Val_long(0);
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
  const int fd = (int)r->buf.len;
  int64_t offset = voids_to_int64_t(&r->c);
#ifdef _WIN32
  const DWORD whence = r->offset;
  HANDLE handle = (HANDLE)(0 + _get_osfhandle(fd));
  if ( handle == INVALID_HANDLE_VALUE ){
    r->offset = UV_EBADF;
    offset = -1;
  }
  else if ( GetFileType(handle) != FILE_TYPE_DISK ){
    /*
      from: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365542(v=vs.85).aspx

      "You cannot use the SetFilePointerEx function with a handle to a
      nonseeking device such as a pipe or a communications device. To
      determine the file type for hFile, use the GetFileType function."

      But it doesn't return an error for whatever reason. So I check
      it manually
    */
    r->offset = UV_ESPIPE;
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
uwt_lseek(value o_fd, value o_pos, value o_mode, value o_loop, value o_cb)
{
  CAMLparam1(o_cb);
  int erg;
  uv_loop_t * loop = Uv_loop_val(o_loop);
  const int fd = FD_VAL(o_fd);
  const int64_t offset = Int64_val(o_pos);
  const int whence = seek_command_table[ Long_val(o_mode) ];

  GR_ROOT_ENLARGE();
  value o_ret;
  struct req * req = uwt__req_create_res(UV_WORK, &o_ret);
  req->buf.len = (size_t)fd;
  req->offset = whence;
  req->c_cb = lseek_cb;
  int64_t_to_voids(offset,&req->c);
  erg = uv_queue_work(loop, (uv_work_t*)req->req, lseek_work_cb, uwork_cb);
  if ( erg >= 0 ){
    uwt__gr_register(&req->cb,o_cb);
  }
  else {
    uwt__req_free(req);
    Field(o_ret,0) = Val_uwt_error(erg);
    Tag_val(o_ret) = Error_tag;
  }
  CAMLreturn(o_ret);
}
