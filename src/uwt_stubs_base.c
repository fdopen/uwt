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

#include "uwt_stubs_base.h"

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
  if ( uwt__global_caml_root_n >= uwt__global_caml_root_size ){
    uwt__gr_enlarge__();
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

UWT_LOCAL bool
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
  Cleanup must be defered to later (main thread),
  if unreferenced uv_handle_t / uv_req_t are garbage collected.
*/
static struct stack stack_struct_handles_to_close =
  { NULL, 0, 0, sizeof (struct handle),0,0,0};

static struct stack stack_struct_req_to_free =
  { NULL, 0, 0, sizeof (struct req),0,0,0};

#define MIN_BUCKET_SIZE_LOG2 8u
#define MAX_BUCKET_SIZE_LOG2 16u

#define STACKS_MEM_BUF_SIZE                           \
  (MAX_BUCKET_SIZE_LOG2 - MIN_BUCKET_SIZE_LOG2 + 1u)
static struct stack stacks_mem_buf[STACKS_MEM_BUF_SIZE];

CAMLprim value
uwt_init_stacks_na(value unit)
{
  unsigned int i,j;
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

  stack_struct_req_to_free.s = malloc(STACK_START_SIZE * (sizeof(void*)));
  if ( stack_struct_req_to_free.s == NULL ){
    caml_raise_out_of_memory();
  }
  stack_struct_req_to_free.size = STACK_START_SIZE;

  return Val_unit;
}

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
#ifdef HAVE_BUILTIN_CTZ
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
uwt__malloc_uv_buf_t(uv_buf_t * buf, size_t len, enum cb_type cb_type)
{
  if ( len == 0 ){
    buf->base = NULL;
    buf->len = 0;
  }
  else {
    unsigned int buck;
    if ( cb_type != CB_LWT ||
         (buck = which_buf(len)) == INVALID_BUF ){
      buf->base = malloc(len);
    }
    else {
      buf->base = mem_stack_pop(&stacks_mem_buf[buck]);
    }
    buf->len = buf->base ? len : 0;
  }
}

UWT_LOCAL void
uwt__free_uv_buf_t_const(const uv_buf_t * buf, enum cb_type cb_type)
{
  if ( buf->base != NULL && buf->len != 0 ){
    unsigned int buck;
    if ( cb_type != CB_LWT ||
         (buck = which_buf(buf->len)) == INVALID_BUF ){
      free(buf->base);
    }
    else {
      mem_stack_free(&stacks_mem_buf[buck],buf->base);
    }
  }
}

UWT_LOCAL void
uwt__free_uv_buf_t(uv_buf_t * buf, enum cb_type cb_type)
{
  uwt__free_uv_buf_t_const(buf,cb_type);
  buf->len = 0;
  buf->base = NULL;
}

/* basics for uv_loop wrapper: macros, globals, and everything that
   can be called from the GC */

static struct loop uwt_global_def_loop[CB_MAX];

static int
in_default_loops(struct loop * l)
{
  int i;
  for ( i = 0; i < CB_MAX; ++i ){
    if ( l == &uwt_global_def_loop[i] ){
      return i;
    }
  }
  return -1;
}

static void
loop_finalize(value v)
{
  struct loop * x = Loop_val(v);
  if ( x &&  in_default_loops(x) < 0 ){
    Field(v,1) = 0; /* Mantis: #7279 */
    if ( x->in_use == 1 ){
      x->do_clean = 1; /* TODO: handle this case */
    }
    else {
      /* remember to change when exposed in the interface */
      free(x);
    }
  }
}


static int
pointer_cmp(value a, value b)
{
  intnat p1 = Field(a,1);
  intnat p2 = Field(b,1);
  return ((p1 > p2) - (p1 < p2));
}

static intnat
pointer_hash(value a){
  size_t n = Field(a,1);
#ifdef ARCH_SIXTYFOUR
  return (n >> 5);
#else
  return (n >> 3);
#endif
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcast-qual"
static struct custom_operations ops_uwt_loop = {
  (char*)"uwt.loop",
  loop_finalize,
  pointer_cmp,
  pointer_hash,
  custom_serialize_default,
  custom_deserialize_default,
#if defined(custom_compare_ext_default)
  custom_compare_ext_default
#endif
};
#pragma GCC diagnostic pop


/* Runtime Lock:
   the OCaml runtime is released by an uv_prepare handle
   (prepare handles are called "right before polling for i/o").
   The runtime is acquired again either inside a callback or after uv_run
*/
UWT_LOCAL bool uwt_global_runtime_released = false;

static void
my_enter_blocking_section(uv_prepare_t *x)
{
  assert(uwt_global_runtime_released == false);
  uwt_global_runtime_released = true;
  caml_enter_blocking_section();
  struct loop * l = x->loop->data;
  if ( l->exn_caught == 1 && l->loop_type == CB_LWT ){
    /* This way, the loop won't block for I/O, so we can handle exceptions
       sooner. */
    uv_stop(x->loop);
  }
}

static void
runtime_acquire_prepare_init(struct loop * l)
{
  bool do_abort = true;
  if ( uv_prepare_init(&l->loop,&l->prep) == 0 ){
    if ( uv_prepare_start(&l->prep,my_enter_blocking_section) == 0 ){
      uv_unref((uv_handle_t*)&l->prep);
      do_abort = false;
    }
  }
  if ( do_abort ){
    fputs("fatal error in uwt, can't register prepare handle\n",stderr);
    exit(2);
  }
}

/* uv_loop_t */
UWT_LOCAL value *uwt__global_wakeup = NULL;
static value *uwt_global_exception_fun = NULL;

#define UWT_WAKEUP_STRING "uwt.wakeup"
#define UWT_ADD_EXCEPTION_STRING "uwt.add_exception"

static void
gc_close_free_common(struct stack *s)
{
  s->pos = 0;
  if ( s->size > STACK_START_SIZE * 8 ){
    void * ns = malloc(STACK_START_SIZE * sizeof(void *));
    if (ns){
      free(s->s);
      s->s = ns;
      s->size = STACK_START_SIZE;
    }
  }
}

static void
close_garbage_collected_handles(void)
{
  unsigned int i;
  for ( i = 0 ; i < stack_struct_handles_to_close.pos ; ++i ){
    struct handle * s = stack_struct_handles_to_close.s[i];
    stack_struct_handles_to_close.s[i] = NULL;
    if ( s->handle != NULL ){
      uwt__handle_finalize_close(s);
    }
    else {
      uwt__handle_free_common(s);
      uwt__free_struct_handle(s);
    }
  }
  gc_close_free_common(&stack_struct_handles_to_close);
}

static void
free_garbage_collected_reqs(void)
{
  unsigned int i;
  for ( i = 0 ; i < stack_struct_req_to_free.pos ; ++i ){
    uwt__req_free(stack_struct_req_to_free.s[i]);
  }
  gc_close_free_common(&stack_struct_req_to_free);
}

UWT_LOCAL void
uwt__add_exception(struct loop *l, value e)
{
  assert( Is_exception_result(e) );
  if ( uwt_global_exception_fun && l && l->loop_type == CB_LWT ){
    l->exn_caught = 1;
    caml_callback_exn(*uwt_global_exception_fun,Extract_exception(e));
  }
}

CAMLprim value
uwt_run_loop(value o_loop,value o_mode)
{
  struct loop * wp;
  wp = Loop_val(o_loop);
  value ret;
  if (unlikely( !wp )){
    ret = VAL_UWT_INT_RESULT_EBADF;
  }
  else if (unlikely( wp->in_use != 0 )){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    uv_loop_t * loop = &wp->loop;
    uv_run_mode m;
    int erg;
    wp->in_use = 1;
    if (stack_struct_handles_to_close.pos){
      close_garbage_collected_handles();
    }
    if (stack_struct_req_to_free.pos){
      free_garbage_collected_reqs();
    }
    switch ( Long_val(o_mode) ){
    case 0: m = UV_RUN_ONCE; break;
    case 1: m = UV_RUN_NOWAIT; break;
    default: assert(false);
    case 2: /*fall*/
      m = UV_RUN_DEFAULT;
    }
    assert( uwt_global_runtime_released == false );
    wp->exn_caught = 0;
    erg = uv_run(loop, m);
    if ( uwt_global_runtime_released == true ){
      uwt_global_runtime_released = false;
      caml_leave_blocking_section();
    }
    if (stack_struct_handles_to_close.pos){
      close_garbage_collected_handles();
    }
    if (stack_struct_req_to_free.pos){
      free_garbage_collected_reqs();
    }
    wp->in_use = 0;
    ret = VAL_UWT_INT_RESULT(erg);
    /* TODO: handle this case
    if ( unlikely(wp->do_clean == 1 ) ){

    } */
  }
  return ret;
}

#if 0
/* not yet exposed */
CAMLprim value
uwt_loop_close(value o_loop)
{
  value ret;
  struct loop * wp;
  wp = Loop_val(o_loop);
  if ( !wp || !wp->loop ){
    ret = VAL_UWT_INT_RESULT_EBADF;
  }
  else if ( wp->in_use ){
    ret = VAL_UWT_INT_RESULT_EBUSY;
  }
  else {
    uv_loop_t * loop = wp->loop;
    int erg;
    erg = uv_loop_close(loop);
    ret = VAL_UWT_UNIT_RESULT(erg);
    if ( erg >= 0 ){
      int x;
      Field(o_loop,1) = 0;
      x = in_default_loops(wp);
      if ( x >= 0 ){
        uwt_def_loop_init_called[x] = false;
      }
      else {
        /* remember to change when exposed in the interface */
        free(wp);
      }
    }
  }
  return ret;
}
#endif

static void
cache_cleaner_init(uv_loop_t * l);

CAMLprim value
uwt_default_loop(value o_mode)
{
  CAMLparam0();
  CAMLlocal2(p,ret);
  const int mode = Long_val(o_mode);
  if ( mode < 0 || mode >= CB_MAX ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_UWT_EFATAL;
  }
  else {
    if ( mode == CB_LWT ){
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
    }
    p = caml_alloc_custom(&ops_uwt_loop,sizeof(intnat), 0, 1);
    Field(p,1) = 0;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = p;
    Field(p,1) = (intnat)&uwt_global_def_loop[mode];
    if ( uwt_global_def_loop[mode].init_called == 0 ){
      const int erg = uv_loop_init(&uwt_global_def_loop[mode].loop);
      if ( erg < 0 ){
        Field(p,1) = 0;
        Tag_val(ret) = Error_tag;
        Field(ret,0) = Val_uwt_error(erg);
      }
      else {
        uwt_global_def_loop[mode].loop.data = &uwt_global_def_loop[mode];
        uwt_global_def_loop[mode].init_called = 1;
        uwt_global_def_loop[mode].in_use = 0;
        uwt_global_def_loop[mode].exn_caught = 0;
        uwt_global_def_loop[mode].loop_type = mode;
        if ( mode == CB_LWT ){ /* TODO CB_CB not supported */
          cache_cleaner_init(&uwt_global_def_loop[mode].loop);
          runtime_acquire_prepare_init(&uwt_global_def_loop[mode]);
        }
      }
    }
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
  assert(type > UV_UNKNOWN_REQ);
  assert(type < UV_REQ_TYPE_MAX);
  x = &stacks_req_t[type];
  assert(x->malloc_size);
  return (mem_stack_pop(x));
}

UWT_LOCAL void
uwt__free_mem_uv_req_t(struct req * wp)
{
  uv_req_t * req;
  if ( wp && ( req = wp->req ) != NULL ){
    if ( wp->cb_type != CB_LWT ){
      free(req);
    }
    else if ( req->type <= UV_UNKNOWN_REQ ||
              req->type >= UV_REQ_TYPE_MAX ){
      DEBUG_PF("unknown type");
      free(req);
    }
    else {
      mem_stack_free(&stacks_req_t[req->type],req);
    }
    wp->req = NULL;
  }
}

static void
req_free_common(struct req * wp)
{
  if ( wp->cb != CB_INVALID ){
    uwt__gr_unregister(&wp->cb);
  }
  if ( wp->sbuf != CB_INVALID ){
    uwt__gr_unregister(&wp->sbuf);
  }
  if ( wp->buf.base != NULL && wp->buf_contains_ba == 0 ){
    uwt__free_uv_buf_t(&wp->buf,wp->cb_type);
  }
  wp->buf.base = NULL;
  wp->buf.len = 0;
  if ( wp->req ){
    if ( wp->clean_cb != NULL ){
      wp->clean_cb(wp->req);
      wp->clean_cb = NULL;
    }
    uwt__free_mem_uv_req_t(wp);
  }
}

UWT_LOCAL void
uwt__free_struct_req(struct req *r)
{
  if ( r->cb_type == CB_LWT ){
    mem_stack_free(&stack_struct_req,r);
  }
  else {
    free(r);
  }
}

UWT_LOCAL void
uwt__req_free(struct req * wp){
  if ( wp ){
    req_free_common(wp);
    uwt__free_struct_req(wp);
  }
}

UWT_LOCAL void
uwt__req_free_most(struct req * wp)
{
  if ( wp ){
    req_free_common(wp);
    wp->in_use = 0;
    if ( wp->finalize_called ){
      uwt__free_struct_req(wp);
    }
  }
}

static void req_finalize(value v)
{
  struct req * wp = Req_val(v);
  if ( wp != NULL ){
    Field(v,1) = 0; /* Mantis: #7279 */
    wp->finalize_called = 1;
    if ( wp->in_use == 0 && wp->in_cb == 0 ){
      if ( wp->cb == CB_INVALID &&
           wp->sbuf == CB_INVALID &&
           (wp->buf.base == NULL || wp->buf_contains_ba == 1) &&
           wp->clean_cb == NULL ) {
        uwt__free_mem_uv_req_t(wp);
        uwt__free_struct_req(wp);
      }
      else {
        /* Calling caml_modify during GC is fragile.
           And it might be easier to write code for work threads, if the
           cleanup is done in the main thread and not during garbage
           collection. */
        if ( stack_struct_req_to_free.pos <
             stack_struct_req_to_free.size ){
          stack_struct_req_to_free.s[stack_struct_req_to_free.pos] = wp;
          ++stack_struct_req_to_free.pos;
        }
        else {
          if ( !uwt__stack_resize_add(&stack_struct_req_to_free,wp,false) ){
            DEBUG_PF("out of memory, req can't be cleaned");
            uwt__free_mem_uv_req_t(wp);
            uwt__free_struct_req(wp);
          }
        }
      }
    }
  }
}

#pragma GCC diagnostic ignored "-Wcast-qual"
static struct custom_operations ops_uwt_req = {
  (char*)"uwt.req",
  req_finalize,
  pointer_cmp,
  pointer_hash,
  custom_serialize_default,
  custom_deserialize_default,
#if defined(custom_compare_ext_default)
  custom_compare_ext_default
#endif
};
#pragma GCC diagnostic pop

static size_t get_req_t_size(uv_req_type typ)
{
  switch ( typ ){
  case UV_CONNECT: return (sizeof(uv_connect_t));
  case UV_WRITE: return (sizeof(uv_write_t));
  case UV_UDP_SEND: return (sizeof(uv_udp_send_t));
  case UV_SHUTDOWN: return (sizeof(uv_shutdown_t));
  case UV_FS: return (sizeof(uv_fs_t));
  case UV_GETADDRINFO: return (sizeof(uv_getaddrinfo_t));
  case UV_GETNAMEINFO: return (sizeof(uv_getnameinfo_t));
  case UV_WORK: return (sizeof(uv_work_t));
  default:
    caml_failwith("fatal: unsupported uv_req_t type");
  }
}

UWT_LOCAL struct req *
uwt__req_create(uv_req_type typ, struct loop *l)
{
  struct req * wp;
  const enum cb_type cb_type = l->loop_type;
  if ( cb_type == CB_LWT ){
    wp = mem_stack_pop(&stack_struct_req);
  }
  else {
    wp = malloc(sizeof *wp);
  }
  if ( wp == NULL ){
    caml_raise_out_of_memory();
  }
  wp->cb_type = cb_type;
  if ( cb_type == CB_LWT ){
    wp->req = malloc_uv_req_t(typ);
  }
  else {
    wp->req = malloc(get_req_t_size(typ));
  }
  if ( wp->req == NULL ){
    uwt__free_struct_req(wp);
    caml_raise_out_of_memory();
  }

  wp->c.p1 = NULL;
  wp->c.p2 = NULL;
  wp->loop = l;

  wp->c_cb = NULL;
  wp->clean_cb = NULL;
  wp->cb = CB_INVALID;
  wp->sbuf = CB_INVALID;
  wp->buf.base = NULL;
  wp->buf.len = 0;
  wp->offset = 0;
  wp->c_param = 0;
  wp->in_use = 0;
  wp->finalize_called = 0;
  wp->buf_contains_ba = 0;
  wp->in_cb = 0;
  wp->req->data = wp;
  wp->req->type = typ;
  return wp;
}

UWT_LOCAL value
uwt__ret_uv_fs_result_unit(uv_req_t * r)
{
  uv_fs_t* req = (uv_fs_t*)r;
  return (VAL_UWT_UNIT_RESULT(req->result));
}

UWT_LOCAL value
uwt__ret_unit_cparam(uv_req_t * r)
{
  struct req * wp = r->data;
  return (VAL_UWT_UNIT_RESULT(wp->c_param));
}

UWT_LOCAL void
uwt__req_callback(uv_req_t * req)
{
  GET_RUNTIME();
  struct req * wp_req = req->data;
  if (unlikely( !wp_req || wp_req->cb == CB_INVALID || wp_req->c_cb == NULL )){
    DEBUG_PF("no data in callback!");
  }
  else {
    assert( wp_req->in_use == 1 );
    value exn;
    wp_req->in_cb = 1;
    if ( wp_req->c_cb == uwt__ret_unit_cparam ){
      exn = VAL_UWT_UNIT_RESULT(wp_req->c_param);
    }
    else if ( wp_req->c_cb == uwt__ret_uv_fs_result_unit ){
      exn = VAL_UWT_UNIT_RESULT(((uv_fs_t*)req)->result);
    }
    else {
      exn = wp_req->c_cb(req);
    }
    exn = CAML_CALLBACK1(wp_req,cb,exn);
    if (unlikely( Is_exception_result(exn) )){
      uwt__add_exception(wp_req->loop,exn);
    }
    wp_req->in_cb = 0;
  }
  uwt__req_free_most(wp_req);
}

CAMLprim value
uwt_req_create(value o_loop, value o_type)
{
  CAMLparam1(o_loop);
  struct loop * l = Loop_val(o_loop);
  if ( l == NULL || l->init_called == 0 ){
    /* not yet exposed in the interface, therefore not possible */
    caml_failwith("invalid loop in req_create");
  }
  int type;
  switch ( Long_val(o_type) ){
  case 0: type = UV_FS; break;
  case 1: type = UV_GETADDRINFO; break;
  case 2: type = UV_GETNAMEINFO; break;
  case 3: type = UV_WORK; break;
  default:
    assert(false);
    caml_failwith("invalid request typ");
  }
  value res =  caml_alloc_custom(&ops_uwt_req, sizeof(intnat), 0, 1);
  Field(res,1) = 0;
  struct req * req = uwt__req_create(type,l);
  Field(res,1) = (intptr_t)req;
  CAMLreturn(res);
}

CAMLprim value
uwt_req_cancel_noerr(value res)
{
  struct req * wp = Req_val(res);
  if ( wp != NULL &&
       wp->req != NULL &&
       wp->in_use == 1 &&
       wp->in_cb == 0 ){
    Field(res,1) = 0;
    wp->finalize_called = 1;
    /* At the moment only cancelable requests are exposed
       to ocaml. */
    uv_cancel(wp->req);
  }
  return Val_unit;
}

CAMLprim value
uwt_req_finalize_na(value res)
{
  struct req * wp = Req_val(res);
  if ( wp && wp->in_cb ){
    /* control flow will go back to uwt__req_callback / common_after_work_cb
       They can free wp  */
    wp->finalize_called = 1;
    Field(res,1) = 0;
  }
  return Val_unit;
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
uwt__handle_free_common(struct handle *s)
{
  if ( !s ) {
    return;
  }
  if ( s->cb_listen != CB_INVALID ){
    uwt__gr_unregister(&s->cb_listen);
  }
  if ( s->cb_listen_server != CB_INVALID ){
    uwt__gr_unregister(&s->cb_listen_server);
  }
  if ( s->cb_read != CB_INVALID ){
    uwt__gr_unregister(&s->cb_read);
  }
  if ( s->cb_close != CB_INVALID ){
    uwt__gr_unregister(&s->cb_close);
  }
  if ( s->obuf != CB_INVALID ){
    uwt__gr_unregister(&s->obuf);
  }
  s->in_use_cnt = 0;
}

static uv_handle_t *
malloc_uv_handle_t(int type)
{
  struct stack * x;
  assert(type > UV_UNKNOWN_HANDLE);
  assert(type < UV_HANDLE_TYPE_MAX);
  x = &stacks_handle_t[type];
  assert(x->malloc_size);
  return (mem_stack_pop(x));
}

UWT_LOCAL void
uwt__free_struct_handle(struct handle * h){
  mem_stack_free(&stack_struct_handle,h);
}

UWT_LOCAL void
uwt__free_mem_uv_handle_t(struct handle * h)
{
  uv_handle_t * handle;
  if ( h && (handle = h->handle ) != NULL ){
    if ( h->cb_type != CB_LWT ){
      free(handle);
    }
    else if ( handle->type <= UV_UNKNOWN_HANDLE ||
              handle->type >= UV_HANDLE_TYPE_MAX ){
      DEBUG_PF("unknown type");
      free(handle);
    }
    else {
      mem_stack_free(&stacks_handle_t[handle->type],handle);
    }
    h->handle = NULL;
  }
}

static void
handle_finalize_close_cb(uv_handle_t *h)
{
  struct handle * s = h->data;
  if ( s ){
    uwt__free_mem_uv_handle_t(s);
    if ( s->cb_listen != CB_INVALID ||
         s->cb_listen_server != CB_INVALID ||
         s->cb_read != CB_INVALID ||
         s->cb_close != CB_INVALID ||
         s->obuf != CB_INVALID ){
      GET_RUNTIME();
      uwt__handle_free_common(s);
    }
    uwt__free_struct_handle(s);
  }
}

UWT_LOCAL void
uwt__cancel_reader(struct handle *h)
{
  if ( h->read_waiting == 1 &&
       h->cb_read != CB_INVALID &&
       h->obuf != CB_INVALID ){
    value exn;
    value param;
    h->read_waiting = 0;
    if ( h->handle->type == UV_UDP ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = VAL_UWT_ERROR_ECANCELED;
    }
    else {
      param = VAL_UWT_INT_RESULT_ECANCELED;
    }
    value cb = GET_CB_VAL(h->cb_read);
    uwt__gr_unregister(&h->cb_read);
    uwt__gr_unregister(&h->obuf);
    ++h->in_callback_cnt;
    ++h->in_use_cnt;
    assert( h->close_called == 1 );
    exn = caml_callback2_exn(*uwt__global_wakeup,cb,param);
    if (unlikely( Is_exception_result(exn) )){
      uwt__add_exception(h->loop,exn);
    }
    --h->in_callback_cnt;
    --h->in_use_cnt;
    if ( h->in_use_cnt ){
      /* yes twice, it was increased in either
         uwt_read_own or uwt_udp_recv_own. It doesn't
         matter currently, because close was already called.
         But I should add debug code to the close callback, if the
         reference count mechanism works as intended. */
      --h->in_use_cnt;
    }
  }
  h->read_waiting = 0;
}

UWT_LOCAL void
uwt__handle_finalize_close(struct handle * s)
{
  uv_handle_t * h = s->handle;
  if ( !h ){
    uwt__handle_free_common(s);
    uwt__free_struct_handle(s);
  }
  else {
    s->close_called = 1;
    if ( s->read_waiting ){
      uwt__cancel_reader(s);
    }
    uv_close(h,handle_finalize_close_cb);
  }
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
  if ( s->close_executed ){
    if ( s->handle ){
      uwt__free_mem_uv_handle_t(s);
    }
    uwt__free_struct_handle(s);
    return;
  }
  if ( s->close_called ){
    return;
  }
  if ( s->in_use_cnt == 0 && s->in_callback_cnt == 0 ){
    if (unlikely( s->cb_listen != CB_INVALID ||
                  s->cb_listen_server != CB_INVALID ||
                  s->cb_read != CB_INVALID ||
                  s->cb_close != CB_INVALID ||
                  s->obuf != CB_INVALID )){
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

#pragma GCC diagnostic push
static struct custom_operations ops_uwt_handle = {
  (char*)"uwt.handle",
  handle_finalize,
  handle_cmp,
  handle_hash,
  custom_serialize_default,
  custom_deserialize_default,
#if defined(custom_compare_ext_default)
  custom_compare_ext_default
#endif
};
#pragma GCC diagnostic pop

UWT_LOCAL value
uwt__handle_create(uv_handle_type handle_type, struct loop *l)
{
  value res;
  struct handle * wp;
  static uintnat hcnt;
  const enum cb_type cb_type = l->loop_type;
  res = caml_alloc_custom(&ops_uwt_handle, sizeof(intnat)*3, 0, 1);
  Field(res,1) = 0;
  wp = mem_stack_pop(&stack_struct_handle);
  if ( !wp ){
    caml_raise_out_of_memory();
  }
  wp->cb_type = cb_type;
  if ( cb_type == CB_LWT ){
    wp->handle = malloc_uv_handle_t(handle_type);
  }
  else {
    wp->handle = malloc(sizeof *wp->handle);
  }
  if ( !wp->handle ){
    uwt__free_struct_handle(wp);
    caml_raise_out_of_memory();
  }
  wp->loop = l;
  wp->cb_listen = CB_INVALID;
  wp->cb_listen_server = CB_INVALID;
  wp->cb_read = CB_INVALID;
  wp->cb_close = CB_INVALID;
  wp->obuf = CB_INVALID;
  wp->ba_read = NULL;
  wp->handle->data = wp;
  wp->handle->type = handle_type;
#ifdef _WIN32
  wp->orig_fd = -1;
#endif

  wp->initialized = 0;
  wp->in_use_cnt = 0;
  wp->in_callback_cnt = 0;
  wp->c_read_size = 0;
  wp->obuf_offset = 0;
  wp->finalize_called = 0;
  wp->close_called = 0;
  wp->close_executed = 0;
  wp->can_reuse_cb_read = 0;
  wp->use_read_ba = 0;
  wp->read_waiting = 0;
  Field(res,1) = (intnat)wp;
  Field(res,2) = hcnt++;
  /* to get the similar behaviour as with file descriptors:
     the hash value and equality won't change after close.
  */
  Field(res,3) = (intnat)wp;
  return res;
}

/* Memory debugging and TODO */

/*
  uv_req_t/struct req are currently not exposed to the end user.
  uwt.ml code captures most exceptions and deallocates
  all resources.
  Only rare exceptions like Out_of_memory will trigger
  conditions under which uv_req_t must be deallocated
  by the OCaml garbage collector.
  uwt_test_req_leak will be used by the test suite to
  create such a condition manually.
*/

static void
free_test_req_leak(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    caml_stat_free(w->p1);
    w->p1 = NULL;
  }
}

CAMLprim value
uwt_test_req_leak(value o_req, value o_ref)
{
  CAMLparam1(o_req);
  struct req * wp = Req_val(o_req);
  if ( wp == NULL || wp->in_use == 1 || wp->req == NULL ){
    caml_failwith("uwt_test_req_leak");
  }
  wp->c.p1 = caml_stat_alloc(sizeof(value));
  wp->clean_cb = free_test_req_leak;
  GR_ROOT_ENLARGE();
  uwt__gr_register(&wp->sbuf,o_ref);
  CAMLreturn(Val_unit);
}

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

  for ( i = 0; i < CB_MAX; ++i ){
    if ( uwt_global_def_loop[i].init_called == 1 ){
      assert( uwt_global_def_loop[i].in_use == 0 );
      uwt_global_def_loop[i].init_called = 0;
      /* this doesn't work, it will probably report busy.
         But I intentionally don't cancel everything */
      uv_loop_close(&uwt_global_def_loop[i].loop);
      uwt_global_def_loop[i].in_use = 0;
    }
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

  if ( stack_struct_req_to_free.pos != 0 ){
    DEBUG_PF("there are still %u reqs that must be freed\n",
             stack_struct_req_to_free.pos);
  }
  else {
    stack_struct_req_to_free.pos = 0;
    free(stack_struct_req_to_free.s);
    stack_struct_req_to_free.s = NULL;
    stack_struct_req_to_free.size = 0;
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
clean_caches(uv_timer_t* handle)
{
  (void)handle;
  /* timers are called before polling for I/O  and before
     prepare handles. */
  GET_RUNTIME();
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

static uv_timer_t timer_cache_cleaner;
static void
cache_cleaner_init(uv_loop_t * l)
{
  bool do_abort = true;
  if ( uv_timer_init(l,&timer_cache_cleaner) == 0 ){
    if ( uv_timer_start(&timer_cache_cleaner,clean_caches,90000,90000) == 0 ){
      uv_unref((uv_handle_t*)&timer_cache_cleaner);
      do_abort = false;
    }
  }
  if ( do_abort ){
    fputs("fatal error in uwt, can't register cache cleaner\n",stderr);
    exit(2);
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

#undef SET_CB_VAL
#undef GR_ROOT_INIT_SIZE
#undef STACK_START_SIZE
#undef MIN_BUCKET_SIZE_LOG2
#undef MAX_BUCKET_SIZE_LOG2
#undef STACKS_MEM_BUF_SIZE
#undef INVALID_BUF
#undef UWT_WAKEUP_STRING
#undef UWT_ADD_EXCEPTION_STRING
