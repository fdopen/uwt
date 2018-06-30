/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_base.h"
#if defined(_MSC_VER)
#include <intrin.h>
#endif

/*
  basic uwt functionality:
  - type declarations

  - memory management (c and ocaml heap)

  - ocaml runtime lock

  - wrappers for uv_req_t and uv_loop_t.

  - core of uv_handle_t wrappers: memory allocation/deallocation
  (exposed to the garbage collector, therefore handled here and not
  inside uwt_stubs_handle.c)
*/


/* Memory in the OCaml heap,
   protected from removal by GC and
   easily accessible from C */

UWT_LOCAL value uwt__global_caml_root = Val_unit;
UWT_LOCAL cb_t uwt__global_caml_root_size = 0;
UWT_LOCAL cb_t uwt__global_caml_root_n = 0;
UWT_LOCAL cb_t * uwt__global_caml_root_free_pos = NULL;

#define SET_CB_VAL(cb,x)                          \
  do {                                            \
    const cb_t cb_ = cb;                          \
    const cb_t i_ = GET_CB_VAL__I(cb_);           \
    const cb_t j_ = GET_CB_VAL__J(cb_);           \
    value ar_ = Field(uwt__global_caml_root,i_);  \
    Store_field(ar_,j_,x);                        \
  } while (0)

#define GR_ROOT_INIT_SIZE (1u << GR_ROOT_INIT_SIZE_P2)

UWT_LOCAL void
uwt__gr_enlarge__(void)
{
  CAMLparam0();
  CAMLlocal1(nroot);
  cb_t i;
  cb_t * t;
  if ( uwt__global_caml_root == Val_unit ){
    enum { AR_INIT_SIZE = 2};
    nroot = caml_alloc(GR_ROOT_INIT_SIZE,0);
    for ( i = 0 ; i < GR_ROOT_INIT_SIZE ; ++i ){
      Field(nroot,i) = Val_unit;
    }
    uwt__global_caml_root = caml_alloc_small(AR_INIT_SIZE,0);
    Field(uwt__global_caml_root,0) = nroot;
    for ( i = 1 ; i < AR_INIT_SIZE ; ++i ){
      Field(uwt__global_caml_root,i) = Val_unit;
    }
    t = malloc(AR_INIT_SIZE * GR_ROOT_INIT_SIZE * sizeof(*t));
    if ( t == NULL ){
      caml_raise_out_of_memory();
    }
    for ( i = 0; i < GR_ROOT_INIT_SIZE; ++i ){
      t[i] = i;
    }
    uwt__global_caml_root_free_pos = t;
    uwt__global_caml_root_size = GR_ROOT_INIT_SIZE;
    caml_register_generational_global_root(&uwt__global_caml_root);
  }
  else {
    const cb_t ri = (uwt__global_caml_root_size + (GR_ROOT_INIT_SIZE - 1))
      / GR_ROOT_INIT_SIZE;
    const size_t ar_size = Wosize_val(uwt__global_caml_root);
    const cb_t nroot_size =
      uwt__global_caml_root_size + GR_ROOT_INIT_SIZE;
    if ( uwt__global_caml_root_size > nroot_size ){
      caml_failwith("too many lwt threads waiting for i/o");
    }
    if ( ri >= ar_size ){
      uint64_t cn_size = ar_size * (uint64_t)(2 * GR_ROOT_INIT_SIZE);
      if ( cn_size > UINT_MAX ){
        cn_size = UINT_MAX;
      }
      nroot = caml_alloc(ar_size*2,0);
      for ( i = 0 ; i < ar_size ; ++i ){
        Store_field(nroot,i,Field(uwt__global_caml_root,i));
      }
      for ( i = ar_size ; i < ar_size * 2 ; ++i ){
        Field(nroot,i) = Val_unit;
      }
      t = realloc(uwt__global_caml_root_free_pos,cn_size * sizeof(*t));
      if ( t == NULL ){
        caml_raise_out_of_memory();
      }
      caml_modify_generational_global_root(&uwt__global_caml_root,nroot);
      uwt__global_caml_root_free_pos = t;
    }
    nroot = caml_alloc(GR_ROOT_INIT_SIZE,0);
    cb_t j;
    for ( i = 0, j = uwt__global_caml_root_size ;
          i < GR_ROOT_INIT_SIZE ;
          ++i, ++j ){
      Field(nroot,i) = Val_unit;
      uwt__global_caml_root_free_pos[j] = j;
    }
    Store_field(uwt__global_caml_root,ri,nroot);
    uwt__global_caml_root_size = nroot_size;
  }
  CAMLreturn0;
}

UWT_LOCAL void
uwt__gr_unregister(cb_t *a)
{
  const cb_t n = *a;
  if ( n != CB_INVALID ){
    SET_CB_VAL(n,Val_unit);
    --uwt__global_caml_root_n;
    uwt__global_caml_root_free_pos[uwt__global_caml_root_n] = n;
    *a = CB_INVALID;
  }
}

UWT_LOCAL void
uwt__gr_register__(cb_t *a,value x)
{
  if (unlikely( uwt__global_caml_root_n >= uwt__global_caml_root_size )){
    Begin_roots1(x);
    uwt__gr_enlarge__();
    End_roots();
  }
  const cb_t pos = uwt__global_caml_root_free_pos[uwt__global_caml_root_n];
  uwt__global_caml_root_n++;
  SET_CB_VAL(pos,x);
  *a = pos;
}

/* Stacks of pre-allocated memory buffers */
struct stack {
    void ** s;
    unsigned int pos; /* position in s */
    unsigned int size; /* how many can I save until I have to realloc */
    unsigned int malloc_size; /* how large the elements are */

    unsigned int created;     /* how many elements were created at all */
    unsigned int pos_min;    /* statistic. Unneeded elements are deleted */
    unsigned int gc_n;      /* from time to time */
};

#define STACK_START_SIZE 256

static bool
uwt__stack_resize_add(struct stack * s,void *p,bool do_free)
{
  void **ns;
  const unsigned int nsize = s->size == 0 ? STACK_START_SIZE : (s->size*2);
  if (unlikely( nsize < s->size )){
    if (do_free){
      --s->created;
      free(p);
    }
    return false;
  }
  ns = realloc(s->s,nsize * (sizeof(void*)));
  if (unlikely( !ns )){
    if (do_free){
      --s->created;
      free(p);
    }
    return false;
  }
  else {
    s->s = ns;
    s->size = nsize;
    s->s[s->pos] = p;
    s->pos = s->pos+1;
    return true;
  }
}

static struct stack stack_struct_req =
  { NULL, 0, 0, sizeof(struct req), 0,0,0};

static struct stack stack_struct_handle =
  { NULL, 0, 0, sizeof(struct handle),0,0,0};

static struct stack stacks_req_t[UV_REQ_TYPE_MAX];
static struct stack stacks_handle_t[UV_HANDLE_TYPE_MAX];

/*
  Cleanup must be defered to later (main thread), if unreferenced uv_handle_t
  are garbage collected.
*/
static struct stack stack_struct_handles_to_close =
  { NULL, 0, 0, sizeof (struct handle),0,0,0};

#define MIN_BUCKET_SIZE_LOG2 8u
#define MAX_BUCKET_SIZE_LOG2 16u

#define STACKS_MEM_BUF_SIZE                           \
  (MAX_BUCKET_SIZE_LOG2 - MIN_BUCKET_SIZE_LOG2 + 1u)
static struct stack stacks_mem_buf[STACKS_MEM_BUF_SIZE];

static inline void
mem_stack_free(struct stack * s, void *p)
{
  if (likely( s->pos < s->size )){
    s->s[s->pos] = p;
    ++s->pos;
  }
  else {
    uwt__stack_resize_add(s,p,true);
  }
}

static inline void *
mem_stack_pop(struct stack * x)
{
  if (unlikely( x->pos == 0 )){
    x->pos_min = 0;
    void * p = malloc(x->malloc_size);
    if ( p ){
      ++x->created;
    }
    return p;
  }
  else {
    --x->pos;
    x->pos_min = UMIN(x->pos,x->pos_min);
    return (x->s[x->pos]);
  }
}

#define INVALID_BUF UINT_MAX

/* log2_help: integer log2,  fractional part discarded:
   255 -> 7
   256 -> 8
   511 -> 8
   512 -> 9
 */

#if !defined(HAVE_BUILTIN_CLZ) && defined(_MSC_VER)
static __inline uint32_t __builtin_clz(uint32_t value) {
  uint32_t leading_zero = 0;
  _BitScanReverse(&leading_zero, value);
  return (31 - leading_zero);
}
#endif
#if defined(HAVE_BUILTIN_CLZ) || defined(_MSC_VER)
#define log2_help(x)                                                    \
  ((unsigned int)((8u * sizeof (unsigned int)) - __builtin_clz((x)) - 1u))
#else
static unsigned int log2_help(unsigned int v)
{
  const unsigned int b[] = {0x2, 0xC, 0xF0, 0xFF00, 0xFFFF0000};
  const unsigned int S[] = {1, 2, 4, 8, 16};
  _Static_assert(sizeof(unsigned int) == 4 ,"unsupported size of unsigned int");
  int i;
  unsigned int r = 0;
  for ( i = 4; i >= 0; i-- ){
    if ( v & b[i] ){
      v >>= S[i];
      r |= S[i];
    }
  }
  return r;
}
#endif

static inline unsigned int
which_buf(size_t len)
{
  unsigned int ret;
  if ( len <= (1u << MIN_BUCKET_SIZE_LOG2) ){
    ret = 0;
  }
  else if ( len > (1u << MAX_BUCKET_SIZE_LOG2) ){
    ret = INVALID_BUF;
  }
  else {
    ret = log2_help(len - 1u) - (MIN_BUCKET_SIZE_LOG2 - 1u);
  }
  return ret;
}

UWT_LOCAL void
uwt__malloc_uv_buf_t(uv_buf_t * buf, size_t len)
{
  if ( len == 0 ){
    buf->base = NULL;
    buf->len = 0;
  }
  else {
    const unsigned int buck = which_buf(len);
    if ( buck == INVALID_BUF ){
      buf->base = malloc(len);
    }
    else {
      buf->base = mem_stack_pop(&stacks_mem_buf[buck]);
    }
    buf->len = buf->base ? len : 0;
  }
}

UWT_LOCAL void
uwt__free_uv_buf_t_const(const uv_buf_t * buf)
{
  if ( buf->base != NULL && buf->len != 0 ){
    const unsigned int buck = which_buf(buf->len);
    if ( buck == INVALID_BUF ){
      free(buf->base);
    }
    else {
      mem_stack_free(&stacks_mem_buf[buck],buf->base);
    }
  }
}

UWT_LOCAL void
uwt__free_uv_buf_t(uv_buf_t * buf)
{
  uwt__free_uv_buf_t_const(buf);
  buf->len = 0;
  buf->base = NULL;
}

/* basics for uv_loop wrapper: macros, globals, and everything that
   can be called from the GC */
static struct loop uwt_lwt_loop;

/* Runtime Lock:
   the OCaml runtime is released by an uv_prepare handle
   (prepare handles are called "right before polling for i/o").
   The runtime is acquired again either inside a callback or after uv_run
*/
UWT_LOCAL bool uwt_global_runtime_released = false;

static void clean_caches(void);

static void
my_enter_blocking_section(uv_prepare_t *x)
{
  static uint64_t last_time;
  static int cnt;
  assert(uwt_global_runtime_released == false);
  ++cnt;
  if ( cnt > 10 || x->loop->time - last_time > 1000 ){
    last_time = x->loop->time;
    cnt = 0;
    clean_caches();
  }
  struct loop * l = x->loop->data;
  if ( l->loop_will_wait ){
    if ( l->stop_loop_earlier ){
      uv_stop(x->loop);
    }
    else {
      uwt_global_runtime_released = true;
      caml_enter_blocking_section();
    }
  }
}

/* uv_loop_t */
UWT_LOCAL value *uwt__global_wakeup = NULL;
UWT_LOCAL value *uwt_global_exception_fun = NULL;

#define UWT_WAKEUP_STRING "uwt.wakeup"
#define UWT_ADD_EXCEPTION_STRING "uwt.add_exception"

static void
close_garbage_collected_handles(void)
{
  unsigned int i;
  for ( i = 0 ; i < stack_struct_handles_to_close.pos ; ++i ){
    struct handle * s = stack_struct_handles_to_close.s[i];
    stack_struct_handles_to_close.s[i] = NULL;
    uwt__handle_finalize_close(s);
  }
  stack_struct_handles_to_close.pos = 0;
  if ( stack_struct_handles_to_close.size > STACK_START_SIZE * 8 ){
    void * ns = malloc(STACK_START_SIZE * sizeof(void *));
    if (ns){
      free(stack_struct_handles_to_close.s);
      stack_struct_handles_to_close.s = ns;
      stack_struct_handles_to_close.size = STACK_START_SIZE;
    }
  }
}

CAMLprim value
uwt_run_loop(value o_loop,value o_mode)
{
  struct loop * wp = Loop_val(o_loop);
  if (unlikely( wp->in_use != 0 )){
    return VAL_UWT_INT_RESULT_EBUSY;
  }
  wp->in_use = 1;
  if (stack_struct_handles_to_close.pos){
    close_garbage_collected_handles();
  }
  uv_run_mode m;
  if ( Long_val(o_mode) == 0 ){
    m = UV_RUN_ONCE;
    wp->loop_will_wait = 1;
  }
  else {
    m = UV_RUN_NOWAIT;
    wp->loop_will_wait = 0;
  }
  wp->stop_loop_earlier = 0;
  assert( uwt_global_runtime_released == false );
  const int erg = uv_run(&wp->loop, m);
  GET_RUNTIME();
  if (stack_struct_handles_to_close.pos){
    close_garbage_collected_handles();
  }
  wp->in_use = 0;
  return (VAL_UWT_INT_RESULT(erg));
}

CAMLprim value
uwt_default_loop(value o_unit)
{
  CAMLparam0();
  CAMLlocal1(p);
  (void) o_unit;

  if ( uwt__global_wakeup == NULL ){
    uwt__global_wakeup = caml_named_value(UWT_WAKEUP_STRING);
    if ( uwt__global_wakeup == NULL ){
      caml_failwith("uwt lwt callback not defined");
    }
  }
  if ( uwt_global_exception_fun == NULL ){
    uwt_global_exception_fun = caml_named_value(UWT_ADD_EXCEPTION_STRING);
    if ( uwt_global_exception_fun == NULL ){
      caml_failwith("uwt exception callback not found");
    }
  }
  value ret = TAG_POINTER(&uwt_lwt_loop);
  if ( uwt_lwt_loop.init_called == 0 ){
    int erg = uv_loop_init(&uwt_lwt_loop.loop);
    if ( erg < 0 ){
      fprintf(stderr,
              "fatal: uv_loop_init failed:%s (%s)\n",
              uv_err_name(erg),
              uv_strerror(erg));
      exit(2);
    }
    erg = uv_prepare_init(&uwt_lwt_loop.loop,&uwt_lwt_loop.prep);
    if ( erg == 0 ){
      erg = uv_prepare_start(&uwt_lwt_loop.prep,my_enter_blocking_section);
      if ( erg == 0 ){
        uv_unref((uv_handle_t*)&uwt_lwt_loop.prep);
      }
    }
    if ( erg != 0 ){
      fprintf(stderr,
              "fatal: uv_prepare_init/uv_prepare_start failed:%s (%s)\n",
              uv_err_name(erg),
              uv_strerror(erg));
      exit(2);
    }
    uwt_lwt_loop.loop.data = &uwt_lwt_loop;
    uwt_lwt_loop.init_called = 1;
    uwt_lwt_loop.in_use = 0;
    uwt_lwt_loop.stop_loop_earlier = 0;
    uwt_lwt_loop.loop_will_wait = 0;
  }
  CAMLreturn(ret);
}

/* basics for uv_req_t wrapper: macros, globals,
   allocation/deallocation functions and everything that can be called
   from the GC */

static uv_req_t *
malloc_uv_req_t(int type)
{
  struct stack * x;
  x = &stacks_req_t[type];
  return (mem_stack_pop(x));
}

UWT_LOCAL void
uwt__req_free(struct req *r)
{
  uv_req_t * req = r->req;
  mem_stack_free(&stacks_req_t[req->type], req);
  mem_stack_free(&stack_struct_req, r);
}

UWT_LOCAL struct req *
uwt__req_create_null(uv_req_type typ)
{
  struct req * wp;
  wp = mem_stack_pop(&stack_struct_req);
  if ( wp == NULL ){
    return NULL;
  }
  wp->req = malloc_uv_req_t(typ);
  if ( wp->req == NULL ){
    mem_stack_free(&stack_struct_req, wp);
    return NULL;
  }

  wp->c.p1 = NULL;
  wp->c.p2 = NULL;
  wp->c_cb = NULL;
  wp->clean_cb = NULL;
  wp->cb = CB_INVALID;
  wp->sbuf = CB_INVALID;
  wp->buf.base = NULL;
  wp->buf.len = 0;
  wp->offset = 0;
  wp->buf_contains_ba = 0;
  wp->req->data = wp;
  wp->req->type = typ;
  return wp;
}

UWT_LOCAL struct req *
uwt__req_create(uv_req_type t)
{
  struct req * r = uwt__req_create_null(t);
  if (r == NULL){
    caml_raise_out_of_memory();
  }
  return r;
}

UWT_LOCAL struct req *
uwt__req_create_res(uv_req_type t, value *vp)
{
  value v = caml_alloc_small(1, Ok_tag);
  struct req * r = uwt__req_create_null(t);
  Field(v,0) = TAG_POINTER(r);
  if ( r == NULL ){
    caml_raise_out_of_memory();
  }
  *vp = v;
  return r;
}

CAMLprim value
uwt_req_cancel_na(value res)
{
  struct req * wp = UNTAG_POINTER(res);
  /* only cancelable requests are exposed to ocaml. */
  if ( uv_cancel(wp->req) == 0 ){
    return Val_long(1);
  }
  return Val_long(0);
}

/* basics for uv_handle_t wrapper: macros, globals,
   allocation/deallocation functions and everything that can be called
   from the GC */
static int
handle_cmp(value a, value b)
{
  uintnat p1 = Field(a,2);
  uintnat p2 = Field(b,2);
  int x = (p1 > p2) - (p1 < p2);
  if (x) {
    return x;
  }
  p1 = Field(a,3);
  p2 = Field(b,3);
  return ((p1 > p2) - (p1 < p2));
}

static intnat
handle_hash(value a){
  return (Field(a,2));
}

UWT_LOCAL void
uwt__handle_unreg_camlval(struct handle *s)
{
  if ( s->cb_listen != CB_INVALID ){
    uwt__gr_unregister(&s->cb_listen);
  }
  if ( s->cb_read != CB_INVALID ){
    uwt__gr_unregister(&s->cb_read);
  }
  if ( s->cb_close != CB_INVALID ){
    uwt__gr_unregister(&s->cb_close);
  }
}

static uv_handle_t *
malloc_uv_handle_t(int type)
{
  struct stack * x;
  x = &stacks_handle_t[type];
  return (mem_stack_pop(x));
}

UWT_LOCAL void
uwt__free_handle(struct handle *h)
{
  uv_handle_t * handle = h->handle;
  mem_stack_free(&stacks_handle_t[handle->type], handle);
  mem_stack_free(&stack_struct_handle, h);
}

UWT_LOCAL void
uwt__handle_finalize_close_cb(uv_handle_t *h)
{
  struct handle * s = h->data;
  if ( s->cb_listen != CB_INVALID ||
       s->cb_read != CB_INVALID ||
       s->cb_close != CB_INVALID ){
    GET_RUNTIME();
    uwt__handle_unreg_camlval(s);
  }
  mem_stack_free(&stacks_handle_t[h->type], h);
  mem_stack_free(&stack_struct_handle, s);
}

UWT_LOCAL void
uwt__cancel_reader(struct handle *h)
{
  value param;
  h->read_waiting = 0;
  assert ( h->cb_read != CB_INVALID );
  if ( h->handle->type == UV_UDP ){
    param = caml_alloc_small(1,Error_tag);
    Field(param,0) = VAL_UWT_ERROR_ECANCELED;
  }
  else {
    param = VAL_UWT_INT_RESULT_ECANCELED;
  }
  value cb = Field(GET_CB_VAL(h->cb_read),1);
  uwt__gr_unregister(&h->cb_read);
  param = caml_callback2_exn(*uwt__global_wakeup,cb,param);
  if (unlikely( Is_exception_result(param) )){
    caml_callback_exn(*uwt_global_exception_fun, Extract_exception(param));
  }
  /* it was increased in either uwt_read_own or uwt_udp_recv_own.*/
  --h->in_use_cnt;
}

UWT_LOCAL void
uwt__handle_finalize_close(struct handle * s)
{
  uv_handle_t * h = s->handle;
  s->close_called = 1;
  s->finalize_called = 1;
  if ( s->read_waiting ){
    uwt__cancel_reader(s);
  }
  uv_close(h,uwt__handle_finalize_close_cb);
}

static void
handle_finalize(value p)
{
  struct handle * s = Handle_val(p);
  if ( !s ){
    return;
  }
  Field(p,1) = 0; /* Mantis: #7279 */
  s->finalize_called = 1;
  if ( s->close_called ){
    return;
  }
  if ( s->in_use_cnt == 0 ){
    if (unlikely( s->cb_listen != CB_INVALID ||
                  s->cb_read != CB_INVALID ||
                  s->cb_close != CB_INVALID )){
      DEBUG_PF("fatal: reference count mechanism is distorted");
    }
    /* we might be in the wrong thread, defer the close call to later */
    if ( stack_struct_handles_to_close.pos <
         stack_struct_handles_to_close.size ){
      stack_struct_handles_to_close.s[stack_struct_handles_to_close.pos] = s;
      ++stack_struct_handles_to_close.pos;
    }
    else {
      bool added = uwt__stack_resize_add(&stack_struct_handles_to_close,s,false);
      if ( !added ){
        /* memory leak, perhaps fd leak */
        DEBUG_PF("out of memory, handle can't be closed");
      }
    }
  }
}

DISABLE_WARNING_CAST_QUAL()

#define OPS_NAME ((char*)"ops_name")
static char * const custom_op_name = OPS_NAME;

static struct custom_operations ops_uwt_handle = {
  OPS_NAME,
  handle_finalize,
  handle_cmp,
  handle_hash,
  custom_serialize_default,
  custom_deserialize_default,
#if defined(custom_compare_ext_default)
  custom_compare_ext_default
#endif
};

static struct custom_operations ops_uwt_handle_nf = {
  OPS_NAME,
  custom_finalize_default,
  handle_cmp,
  handle_hash,
  custom_serialize_default,
  custom_deserialize_default,
#if defined(custom_compare_ext_default)
  custom_compare_ext_default
#endif
};

POP_WARNING()

#ifdef _WIN32
CAMLprim value
uwt_init_sync_na(value unit)
{
  (void) unit;
  /* workaround libuv bug. see #1488 */
  uv_hrtime();
  return Val_unit;
}
#endif

CAMLprim value
uwt_init_na(value unit)
{
  unsigned int i,j;

  /* they must point to the same location. Otherwise the
     polymorphic comparison function won't behave as intended */
  ops_uwt_handle.identifier = custom_op_name;
  ops_uwt_handle_nf.identifier = custom_op_name;

  _Static_assert(UV_UNKNOWN_REQ == 0 , "macros changed");
  _Static_assert(UV_REQ_TYPE_MAX < 256, "macros changed");

  _Static_assert(UV_UNKNOWN_HANDLE == 0, "macros changed");
  _Static_assert(UV_HANDLE_TYPE_MAX < 256, "macros changed");
  (void) unit;
  memset(&stacks_req_t,0,sizeof(stacks_req_t));
  memset(&stacks_handle_t,0,sizeof(stacks_handle_t));

#define XX(t,d)                                 \
  stacks_req_t[t].s = NULL;                     \
  stacks_req_t[t].pos = 0;                      \
  stacks_req_t[t].size = 0;                     \
  stacks_req_t[t].created = 0;                  \
  stacks_req_t[t].pos_min = 0;                  \
  stacks_req_t[t].gc_n = 0;                     \
  stacks_req_t[t].malloc_size = sizeof(d)

  XX(UV_CONNECT,uv_connect_t);
  XX(UV_WRITE,uv_write_t);
  XX(UV_UDP_SEND,uv_udp_send_t);
  XX(UV_SHUTDOWN,uv_shutdown_t);
  XX(UV_FS,uv_fs_t);
  XX(UV_GETADDRINFO,uv_getaddrinfo_t);
  XX(UV_GETNAMEINFO,uv_getnameinfo_t);
  XX(UV_WORK,uv_work_t);
#undef XX
#define XX(t,d)                                 \
  stacks_handle_t[t].s = NULL;                  \
  stacks_handle_t[t].pos = 0;                   \
  stacks_handle_t[t].size = 0;                  \
  stacks_handle_t[t].created = 0;               \
  stacks_handle_t[t].pos_min = 0;               \
  stacks_handle_t[t].gc_n = 0;                  \
  stacks_handle_t[t].malloc_size = sizeof(d)

  XX(UV_TIMER,uv_timer_t);
  XX(UV_TCP,uv_tcp_t);
  XX(UV_NAMED_PIPE,uv_pipe_t);
  XX(UV_UDP,uv_udp_t);
  XX(UV_TTY,uv_tty_t);
  XX(UV_PROCESS,uv_process_t);
  XX(UV_SIGNAL,uv_signal_t);
  XX(UV_POLL,uv_poll_t);
  XX(UV_FS_EVENT,uv_fs_event_t);
  XX(UV_FS_POLL,uv_fs_poll_t);
  XX(UV_ASYNC,uv_async_t);
#undef XX

  j = MIN_BUCKET_SIZE_LOG2;
  for ( i = 0; i < STACKS_MEM_BUF_SIZE; ++i ){
    stacks_mem_buf[i].s = NULL;
    stacks_mem_buf[i].pos = 0;
    stacks_mem_buf[i].size = 0;
    stacks_mem_buf[i].malloc_size = 1u << j;
    stacks_mem_buf[i].created = 0;
    stacks_mem_buf[i].pos_min = 0;
    stacks_mem_buf[i].gc_n = 0;
    ++j;
  }

  /* prealloc memory. If malloc fails during a GC call, it could lead to
     resource leakage */
  stack_struct_handles_to_close.s = malloc(STACK_START_SIZE * (sizeof(void*)));
  if ( stack_struct_handles_to_close.s == NULL ){
    caml_raise_out_of_memory();
  }
  stack_struct_handles_to_close.size = STACK_START_SIZE;

  return Val_unit;
}

UWT_LOCAL value
uwt__handle_res_create(uv_handle_type handle_type, bool finalizable)
{
  value res;
  value v;
  struct handle * wp;
  static uintnat hcnt;
  res = caml_alloc_custom(finalizable ? &ops_uwt_handle : &ops_uwt_handle_nf,
                          sizeof(intnat)*3, 0, 1);
  Field(res,1) = 0;

  Begin_roots1(res);
  v = caml_alloc_small(1,Ok_tag);
  Field(v,0) = res;
  End_roots();

  wp = mem_stack_pop(&stack_struct_handle);
  if ( !wp ){
    caml_raise_out_of_memory();
  }
  wp->handle = malloc_uv_handle_t(handle_type);
  if ( !wp->handle ){
    mem_stack_free(&stack_struct_handle, wp);
    caml_raise_out_of_memory();
  }
  wp->cb_listen = CB_INVALID;
  wp->cb_read = CB_INVALID;
  wp->cb_close = CB_INVALID;
  wp->handle->data = wp;
  wp->handle->type = handle_type;
#ifdef _WIN32
  wp->orig_fd = -1;
#endif

  wp->initialized = 0;
  wp->in_use_cnt = 0;
  wp->c_read_size = 0;
  wp->x.obuf_offset = 0;
  wp->finalize_called = 0;
  wp->close_called = 0;
  wp->can_reuse_cb_read = 0;
  wp->use_read_ba = 0;
  wp->read_waiting = 0;

  Field(res,1) = (intnat)wp;
  Field(res,2) = hcnt++;
  /* To get similar behaviour as with file descriptors:
     the hash value and equality won't change after close. */
  Field(res,3) = (intnat)wp;
  return v;
}

static void
close_pipe_handle(uv_handle_t *h)
{
  mem_stack_free(&stacks_handle_t[UV_NAMED_PIPE], h);
}

/*
  special case:
  if uv_spawn fails, and UV_CREATE_PIPE was specified, pipes could have
  been created.
  The library user might not expect this. So I close them to avoid descriptor
  leakage. But I also create new uv_pipe_t handles (without file descriptors),
  that can be used or closed like the original handles.
  Hash value and the behaviour of the compare and identity function won't change
*/
UWT_LOCAL int
uwt__pipe_handle_reinit(struct handle * wp)
{
  uv_handle_t * h = wp->handle;
  uv_pipe_t * p = (uv_pipe_t*) h;
  uv_loop_t * l = p->loop;
  int ipc = p->ipc;
  uv_handle_t * nh;
  nh = malloc_uv_handle_t(UV_NAMED_PIPE);
  if ( nh == NULL ){
    return UV_ENOMEM;
  }
  int erg = uv_pipe_init(l,(uv_pipe_t*)nh, ipc);
  if (erg < 0){
    mem_stack_free(&stacks_handle_t[UV_NAMED_PIPE], nh);
    return erg;
  }
  uv_close(h,close_pipe_handle);
  wp->handle = nh;
  wp->handle->data = wp;
  wp->initialized = 0;
  return 0;
}

/* Memory debugging and TODO */

static void
stack_clean (struct stack *s){
  if ( s->s && s->size > 0 ){
    unsigned int i;
    for ( i = 0; i < s->pos; ++i ){
      free(s->s[i]);
    }
    free(s->s);
    s->s = NULL;
    s->pos = 0;
    s->size = 0;
    s->created = 0;
    s->pos_min = 0;
    s->gc_n = 0;
  }
}

/* just for debugging. make valgrind happy */
CAMLprim value
uwt_free_all_memory(value unit)
{
  (void) unit;
  unsigned int i;
  (void) unit;
  if ( uwt__global_caml_root != Val_unit ){
    unsigned int found = 0;
    unsigned int ar_size = Wosize_val(uwt__global_caml_root);
    for ( i = 0 ; i < ar_size ; ++i ){
      value aro = Field(uwt__global_caml_root,i);
      if ( aro == Val_unit ){
        continue;
      }
      unsigned int ar_size2 = Wosize_val(aro);
      unsigned int j;
      for ( j = 0 ; j < ar_size2 ; ++j ){
        if ( Field(aro,j) != Val_unit ){
          ++found;
        }
      }
    }
    if ( !found ){
      uwt__global_caml_root_size = 0;
      uwt__global_caml_root_n = 0;
      caml_remove_generational_global_root(&uwt__global_caml_root);
      uwt__global_caml_root = Val_unit;
      free(uwt__global_caml_root_free_pos);
      uwt__global_caml_root_free_pos = NULL;
    }
    else {
      DEBUG_PF("uwt__global_caml_root still in use, found %u elements\n",found);
    }
  }

  stack_clean(&stack_struct_req);
  stack_clean(&stack_struct_handle);
  for ( i = 0; i < UV_REQ_TYPE_MAX; ++i ){
    stack_clean(&stacks_req_t[i]);
  }
  for ( i = 0; i < UV_HANDLE_TYPE_MAX; ++i ){
    stack_clean(&stacks_handle_t[i]);
  }
  for ( i = 0; i < STACKS_MEM_BUF_SIZE; ++i ){
    stack_clean(&stacks_mem_buf[i]);
  }

  if ( uwt_lwt_loop.init_called == 1 ){
    assert( uwt_lwt_loop.in_use == 0 );
    uwt_lwt_loop.init_called = 0;
      /* this doesn't work, it will probably report busy.
         But I intentionally don't cancel everything */
    uv_loop_close(&uwt_lwt_loop.loop);
    uwt_lwt_loop.in_use = 0;
  }

  if ( stack_struct_handles_to_close.pos != 0 ){
    DEBUG_PF("there are still %u handles that must be closed\n",
             stack_struct_handles_to_close.pos);
  }
  else {
    stack_struct_handles_to_close.pos = 0;
    free(stack_struct_handles_to_close.s);
    stack_struct_handles_to_close.s = NULL;
    stack_struct_handles_to_close.size = 0;
  }

  return Val_unit;
}

/*
 TODO:
  - find a real algorithm when to release memory
  - document it
  - make it configurable for the user
*/
static void
clean_cache(struct stack * s)
{
  if ( s->pos < 256 || s->pos_min < ( s->created / 2 ) ){
    s->gc_n = 0;
    s->pos_min = s->pos;
  }
  else if ( s->gc_n < 10 ){
    ++s->gc_n;
  }
  else {
    unsigned int i;
    for ( i = 0; i < ( s->created / 3 ); ++i ){
     --s->pos;
      free(s->s[s->pos]);
    }
    s->created = s->created - i;
    s->gc_n = 0;
    s->pos_min = s->pos;
  }
}

static void
clean_caches(void)
{
  unsigned int i;
  clean_cache(&stack_struct_req);
  clean_cache(&stack_struct_handle);
  for ( i = 0; i < UV_REQ_TYPE_MAX; ++i ){
    clean_cache(&stacks_req_t[i]);
  }
  for ( i = 0; i < UV_HANDLE_TYPE_MAX; ++i ){
    clean_cache(&stacks_handle_t[i]);
  }
  for ( i = 0; i < STACKS_MEM_BUF_SIZE; ++i ){
    clean_cache(&stacks_mem_buf[i]);
  }
}

static void
help_cleanup(struct stack * s)
{
  unsigned int i = s->pos;
  while ( i ){
    --i;
    free(s->s[i]);
  }
  s->created -= s->pos;
  s->pos = 0;
  s->gc_n = 0;
  s->pos_min = 0;
}

CAMLprim value
uwt_cleanup_na(value o_unit)
{
  (void)o_unit;
  unsigned int i;
  help_cleanup(&stack_struct_req);
  help_cleanup(&stack_struct_handle);
  for ( i = 0; i < UV_REQ_TYPE_MAX; ++i ){
    help_cleanup(&stacks_req_t[i]);
  }
  for ( i = 0; i < UV_HANDLE_TYPE_MAX; ++i ){
    help_cleanup(&stacks_handle_t[i]);
  }
  for ( i = 0; i < STACKS_MEM_BUF_SIZE; ++i ){
    help_cleanup(&stacks_mem_buf[i]);
  }
  return Val_unit;
}

UWT_LOCAL int
uwt__build_iovecs(value o_ios, struct req * wp)
{
  const size_t ar_size = Wosize_val(o_ios);
  const size_t bufs_len = ar_size * (sizeof(uv_buf_t));
  size_t whole_len = bufs_len;
  size_t i;
  for ( i = 0; i < ar_size; ++i ){
    value cur = Field(o_ios,i);
    if ( Tag_val(cur) != 0 ){
      whole_len += Long_val(Field(cur,2));
    }
  }
  uwt__malloc_uv_buf_t(&wp->buf, whole_len);
  if ( wp->buf.base == NULL ){
    return UV_ENOMEM;
  }
  wp->buf_contains_ba = 0;
  uv_buf_t *bufs = (uv_buf_t *)wp->buf.base;
  char * p = wp->buf.base + bufs_len;
  for ( i = 0; i < ar_size; ++i ){
    value cur = Field(o_ios,i);
    const size_t len = Long_val(Field(cur,2));
#if defined(_WIN32) && defined(ARCH_SIXTYFOUR)
    if (unlikely( len > ULONG_MAX )){
      uwt__free_uv_buf_t(&wp->buf);
      return UV_EINVAL;
    }
#endif
    bufs[i].len = len;
    if ( Tag_val(cur) == 0 ){
      bufs[i].base = Ba_buf_val(Field(cur,0)) + Long_val(Field(cur,1));
    }
    else {
      bufs[i].base = p;
      const char *src = String_val(Field(cur,0)) + Long_val(Field(cur,1));
      memcpy(p, src, len);
      p+= len;
    }
  }
  return 0;
}

#undef SET_CB_VAL
#undef GR_ROOT_INIT_SIZE
#undef STACK_START_SIZE
#undef MIN_BUCKET_SIZE_LOG2
#undef MAX_BUCKET_SIZE_LOG2
#undef STACKS_MEM_BUF_SIZE
#undef INVALID_BUF
#undef UWT_WAKEUP_STRING
#undef UWT_ADD_EXCEPTION_STRING
