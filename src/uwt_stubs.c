/* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
 * Module Uwt
 * Copyright (C) 2015 Andreas Hauptmann
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * * Neither the name of the author nor the names of its contributors
 *   may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"
#include <uv.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#define CAML_NAME_SPACE 1
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#ifdef _WIN32
#include <io.h>
#endif

#include "uwt-worker.h"
#include "uwt_stubs.h"
#include "macros.h"
#include "uwt-error.h"
#include "map_error.h"

#define DEF_ALLOC_SIZE 65536

#define Some_tag 0

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

static int
safe_convert_flag_list(value list, const int flags[],size_t flags_size)
{
  int res;
  res = 0;
  while ( list != Val_int(0) ){
    const intnat pos = Long_val(Field(list, 0));
    list = Field(list, 1);
    if ( pos < 0 || (size_t)pos >= flags_size ){
      DEBUG_PF("invalid position in flag_table");
    }
    else {
      res |= flags[pos];
    }
  }
  return res;
}
#define SAFE_CONVERT_FLAG_LIST(a,b)             \
  safe_convert_flag_list(a,b,AR_SIZE(b))

static value
safe_rev_convert_flag_list(int res, const int flags[],size_t flags_size)
{
  CAMLparam0();
  CAMLlocal2(l,tmp);
  size_t i;
  l = Val_unit;
  for ( i = flags_size; i != 0 ; ){
    --i;
    if ( res & flags[i] ){
      tmp = caml_alloc_small(2,0);
      Field(tmp,0) = Val_long(i);
      Field(tmp,1) = l;
      l = tmp;
    }
  }
  CAMLreturn(l);
}
#define SAFE_REV_CONVERT_FLAG_LIST(a,b)         \
  safe_rev_convert_flag_list(a,b,AR_SIZE(b))

enum cb_type {
  CB_SYNC = 0,
  CB_LWT = 1,
  CB_CB = 2,
  CB_MAX = 3
};

struct loop {
    uv_loop_t loop;
    uv_prepare_t prep;
    unsigned int init_called:1;
    unsigned int exn_caught:1;
    unsigned int in_use :1;
    unsigned int do_clean: 1;
    unsigned int loop_type: 2;
};

#define Loop_val(v)                             \
  ( (struct loop *)( Field((v),1)) )

static value *uwt_global_wakeup = NULL;
static struct loop uwt_global_def_loop[CB_MAX];
static value *uwt_global_exception_fun = NULL;
static bool uwt_global_runtime_released = false;

static value uwt_global_caml_root = Val_unit;
static unsigned int uwt_global_caml_root_size = 0;
static unsigned int uwt_global_caml_root_n = 0;
static unsigned int * uwt_global_caml_root_free_pos = NULL;

#define GR_ROOT_INIT_SIZE_P2 12u
#define GR_ROOT_INIT_SIZE (1u << GR_ROOT_INIT_SIZE_P2)
#define UWT_WAKEUP_STRING "uwt.wakeup"
#define UWT_ADD_EXCEPTION_STRING "uwt.add_exception"

#define GET_RUNTIME()                             \
  do {                                            \
    if ( uwt_global_runtime_released == true ){   \
      uwt_global_runtime_released = false;        \
      caml_leave_blocking_section();              \
    }                                             \
  } while (0)

typedef unsigned int cb_t;
#define CB_INVALID UINT_MAX

#define GET_CB_VAL__I(x)                              \
  ((unsigned int)(x) / (1u << GR_ROOT_INIT_SIZE_P2))
#define GET_CB_VAL__J(x)                                      \
  ((unsigned int)(x) & ((1u << GR_ROOT_INIT_SIZE_P2) - 1u))

#define GET_CB_VAL(cb)                                                  \
  (Field(Field(uwt_global_caml_root,GET_CB_VAL__I(cb)),GET_CB_VAL__J(cb)))
#define SET_CB_VAL(cb,x)                        \
  do {                                          \
    const unsigned cb_ = cb;                    \
    const unsigned i_ = GET_CB_VAL__I(cb_);     \
    const unsigned j_ = GET_CB_VAL__J(cb_);     \
    value ar_ = Field(uwt_global_caml_root,i_); \
    Store_field(ar_,j_,x);                      \
  } while (0)

#define CAML_CALLBACK1(_wp,_ct,_val)            \
  caml_callback2_exn(*uwt_global_wakeup,        \
                     GET_CB_VAL((_wp)->_ct),    \
                     (_val))

static void
gr_root_enlarge__(void)
{
  CAMLparam0();
  CAMLlocal1(nroot);
  unsigned int i;
  unsigned int * t;
  if ( uwt_global_caml_root == Val_unit ){
    enum { AR_INIT_SIZE = 2};
    nroot = caml_alloc(GR_ROOT_INIT_SIZE,0);
    for ( i = 0 ; i < GR_ROOT_INIT_SIZE ; ++i ){
      Field(nroot,i) = Val_unit;
    }
    uwt_global_caml_root = caml_alloc_small(AR_INIT_SIZE,0);
    Field(uwt_global_caml_root,0) = nroot;
    for ( i = 1 ; i < AR_INIT_SIZE ; ++i ){
      Field(uwt_global_caml_root,i) = Val_unit;
    }
    t = malloc(AR_INIT_SIZE * GR_ROOT_INIT_SIZE * sizeof(*t));
    if ( t == NULL ){
      caml_raise_out_of_memory();
    }
    for ( i = 0; i < GR_ROOT_INIT_SIZE; ++i ){
      t[i] = i;
    }
    uwt_global_caml_root_free_pos = t;
    uwt_global_caml_root_size = GR_ROOT_INIT_SIZE;
    caml_register_generational_global_root(&uwt_global_caml_root);
  }
  else {
    const unsigned int ri = (uwt_global_caml_root_size + (GR_ROOT_INIT_SIZE - 1))
                            / GR_ROOT_INIT_SIZE;
    const size_t ar_size = Wosize_val(uwt_global_caml_root);
    const unsigned int nroot_size =
      uwt_global_caml_root_size + GR_ROOT_INIT_SIZE;
    if ( uwt_global_caml_root_size > nroot_size ){
      caml_failwith("too many lwt threads waiting for i/o");
    }
    if ( ri >= ar_size ){
      uint64_t cn_size = ar_size * (uint64_t)(2 * GR_ROOT_INIT_SIZE);
      if ( cn_size > UINT_MAX ){
        cn_size = UINT_MAX;
      }
      nroot = caml_alloc(ar_size*2,0);
      for ( i = 0 ; i < ar_size ; ++i ){
        Store_field(nroot,i,Field(uwt_global_caml_root,i));
      }
      for ( i = ar_size ; i < ar_size * 2 ; ++i ){
        Field(nroot,i) = Val_unit;
      }
      t = realloc(uwt_global_caml_root_free_pos,cn_size * sizeof(*t));
      if ( t == NULL ){
        caml_raise_out_of_memory();
      }
      caml_modify_generational_global_root(&uwt_global_caml_root,nroot);
      uwt_global_caml_root_free_pos = t;
    }
    nroot = caml_alloc(GR_ROOT_INIT_SIZE,0);
    unsigned int j;
    for ( i = 0, j = uwt_global_caml_root_size ;
          i < GR_ROOT_INIT_SIZE ;
          ++i, ++j ){
      Field(nroot,i) = Val_unit;
      uwt_global_caml_root_free_pos[j] = j;
    }
    Store_field(uwt_global_caml_root,ri,nroot);
    uwt_global_caml_root_size = nroot_size;
  }
  CAMLreturn0;
}

static void
gr_root_unregister(unsigned int *a)
{
  const unsigned int n = *a;
  if ( n != CB_INVALID ){
    SET_CB_VAL(n,Val_unit);
    --uwt_global_caml_root_n;
    uwt_global_caml_root_free_pos[uwt_global_caml_root_n] = n;
    *a = CB_INVALID;
  }
}

static void
gr_root_register__(unsigned int *a,value x)
{
  if ( uwt_global_caml_root_n >= uwt_global_caml_root_size ){
    gr_root_enlarge__();
  }
  const unsigned pos = uwt_global_caml_root_free_pos[uwt_global_caml_root_n];
  uwt_global_caml_root_n++;
  SET_CB_VAL(pos,x);
  *a = pos;
}

#define GR_ROOT_ENLARGE()                                               \
  ATTR_UNUSED                                                           \
  void (* const gr_root_register)(unsigned int *a,value x) =            \
    gr_root_register__;                                                 \
  do {                                                                  \
    if (unlikely( uwt_global_caml_root_n + 4 >=                         \
                  uwt_global_caml_root_size )){                         \
      gr_root_enlarge__();                                              \
    }                                                                   \
  } while(0)

#define STACK_START_SIZE 256
struct stack {
    void ** s;
    unsigned int pos; /* position in s */
    unsigned int size; /* how many can I save until I have to realloc */
    unsigned int malloc_size; /* how large the elements are */

    unsigned int created;     /* how many elements were created at all */
    unsigned int pos_min;    /* statistic. Unneeded elements are deleted */
    unsigned int gc_n;      /* from time to time */
};

static bool
stack_resize_add(struct stack * s,void *p,bool do_free)
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

static inline void
mem_stack_free(struct stack * s, void *p)
{
  if (likely( s->pos < s->size )){
    s->s[s->pos] = p;
    ++s->pos;
  }
  else {
    stack_resize_add(s,p,true);
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

/* TODO: better representation */
union all_sockaddr {
    struct sockaddr addr;
    struct sockaddr_in in;
    struct sockaddr_in6 in6;
    struct sockaddr_storage stor;
#ifndef _WIN32
    struct sockaddr_un un;
#endif
};

static value
uwt_alloc_sockaddr(union all_sockaddr *addr)
{
  value res;
  if ( addr == NULL ){
    return Val_unit;
  }
  switch (addr->addr.sa_family) {
#ifndef _WIN32
  case AF_UNIX:
    {
      const size_t max_len =
        sizeof(union all_sockaddr) - sizeof(sa_family_t) - 1;
      _Static_assert(sizeof(union all_sockaddr) - sizeof(sa_family_t) - 1 >=
                     sizeof addr->un.sun_path,
                     "sun path length");
      const size_t len = strnlen(addr->un.sun_path,max_len);
      value str = caml_alloc_string(len);
      memcpy(String_val(str),addr->un.sun_path,len);
      Begin_roots1(str);
      res = caml_alloc_small(1,0);
      Field(res,0) = str;
      End_roots();
      break;
    }
#endif
  case AF_INET:
    {
      value a = caml_alloc_string(4);
      memcpy(String_val(a), &addr->in.sin_addr, 4);
      Begin_root (a);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(addr->in.sin_port));
      End_roots();
      break;
    }
  case AF_INET6:
    {
      value a = caml_alloc_string(16);
      /* we can't use alloc_inet_addr, because OCaml and libuv
         sometimes have different opinions about ipv6 support.
         alloc_inet6_addr is not always available */
      memcpy(String_val(a), &addr->in6.sin6_addr, 16);
      Begin_root (a);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(addr->in6.sin6_port));
      End_roots();
      break;
    }
  default:
    {
      res = Val_unit;
    }
  }
  return res;
}

#ifndef GET_INET6_ADDR
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))
#endif

static bool
uwt_get_sockaddr(value o_addr,union all_sockaddr *addr)
{
  switch(Tag_val(o_addr)) {
#ifndef _WIN32
  case 0:
    {
      value path;
      mlsize_t len;
      path = Field(o_addr, 0);
      len = caml_string_length(path);
      if (len >= sizeof(addr->un.sun_path)) {
        return false;
      }
      memset(&addr->un, 0, sizeof(struct sockaddr_un));
      addr->un.sun_family = AF_UNIX;
      memmove (addr->un.sun_path, String_val(path), len + 1);
      return true;
    }
#endif
  case 1:                       /* ADDR_INET */
    if ( caml_string_length(Field(o_addr, 0)) == 4 ) {
      memset(&addr->in, 0, sizeof(struct sockaddr_in));
      addr->in.sin_family = AF_INET;
      addr->in.sin_addr = GET_INET_ADDR(Field(o_addr, 0));
      addr->in.sin_port = htons(Int_val(Field(o_addr, 1)));
#ifdef SIN6_LEN
      addr->in.sin_len = sizeof(struct sockaddr_in);
#endif
      return true;
    }
    else {
      memset(&addr->in6, 0, sizeof(struct sockaddr_in6));
      addr->in6.sin6_family = AF_INET6;
      addr->in6.sin6_addr = GET_INET6_ADDR(Field(o_addr, 0));
      addr->in6.sin6_port = htons(Int_val(Field(o_addr, 1)));
#ifdef SIN6_LEN
      addr->in6.sin6_len = sizeof(struct sockaddr_in6);
#endif
      return true;
    }
  default:
    return false;
  }
}

#define Ba_buf_val(x)  ((char*)Caml_ba_data_val(x))

struct req;
typedef value (*req_c_cb)(uv_req_t*);
typedef void (*clean_cb)(uv_req_t*);

struct ATTR_PACKED req {
    struct worker_params c;
    uv_req_t * req;
    struct loop * loop;
    size_t offset;
    req_c_cb c_cb;
    clean_cb clean_cb;
    uv_buf_t buf;
    cb_t cb;
    cb_t sbuf;
    int c_param;
    unsigned int in_use : 1;
    unsigned int finalize_called: 1; /* message from ocaml garbage collector,
                                        that it can be freed */
    unsigned int cb_type : 2; /* 0: sync, 1: lwt, 2: normal callback */
    unsigned int buf_contains_ba: 1; /* used for other purpose, if not used */
    unsigned int in_cb: 1;
};

#define Req_val(v)                              \
  ( (struct req *)( Field((v),1)) )

struct ATTR_PACKED handle {
    uv_handle_t * handle;
    struct loop * loop;
    size_t obuf_offset; /* for read_own */
    size_t c_read_size; /* passed to the alloc function */
    void * ba_read; /* pointer to bigarray for reading */
    cb_t cb_listen;
    cb_t cb_listen_server;
    cb_t cb_read;
    cb_t cb_close;
    cb_t obuf;
#ifdef _WIN32
    int orig_fd; /* when converting to and back from Unix.file_descr, I have to
                    save the original crt fd in order to avoid descriptor
                    leaking. See mantis #5258 */
#endif
    uint16_t in_use_cnt;
    uint16_t in_callback_cnt;

    /* initialized doesn't mean _init() was called.
       Some handles contain only garbage after init was called
       (at least uv_pipe_init).
       If you pass such a handle to e.g. uv_write or uv_read, you program will
       segfault (best case).
       Most errors are already captured by libuv, but not all.
       Therefore, I try to keep track with this flag, if a handle
       is valid or not.
    */
    unsigned int initialized: 1;
    unsigned int finalize_called: 1;
    unsigned int close_called: 1;
    unsigned int close_executed: 1;
    unsigned int cb_type: 2;
    unsigned int cb_read_removed_by_cb: 1;
    unsigned int use_read_ba: 1;
    unsigned int can_reuse_cb_read:1;
    unsigned int read_waiting: 1;
};

#ifdef Handle_val
#undef Handle_val
#if 0 /* not used yet */
#define OCAML_Handle_val(v)                               \
  (((struct filedescr *) Data_custom_val(v))->fd.handle)
#endif
#endif

#define Handle_val(x)                           \
  ((struct handle*)(Field((x),1)))

static void req_free(struct req * wp);
static void req_finalize(value v)
{
  struct req * wp = Req_val(v);
  if ( wp != NULL ){
    Field(v,1) = 0; /* Mantis: #7279 */
    if ( wp->in_use != 0 || wp->in_cb != 0 ){
      wp->finalize_called = 1;
    }
    else {
      if ( wp->cb != CB_INVALID ||
           wp->sbuf != CB_INVALID ){
        DEBUG_PF("fatal: request handle still in use,"
                 " even though marked otherwise");
        wp->cb = CB_INVALID;
        wp->sbuf = CB_INVALID;
      }
      req_free(wp);
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
pointer_hash(value a){
  size_t n = Field(a,1);
#ifdef ARCH_SIXTYFOUR
  return (n >> 5);
#else
  return (n >> 3);
#endif
}

static intnat
handle_hash(value a){
  return (Field(a,2));
}

/* workaround for the old style type declartion of the ocaml runtime */
#pragma GCC diagnostic push
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

static void handle_finalize(value);
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

static void loop_finalize(value);
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

static struct stack stack_struct_req =
  { NULL, 0, 0, sizeof(struct req), 0,0,0};
static struct stack stack_struct_handle =
  { NULL, 0, 0, sizeof(struct handle),0,0,0};
static struct stack stacks_req_t[UV_REQ_TYPE_MAX];
static struct stack stacks_handle_t[UV_HANDLE_TYPE_MAX];

static struct stack stack_struct_handles_to_close =
  { NULL, 0, 0, sizeof (struct handle),0,0,0};

#define MIN_BUCKET_SIZE_LOG2 8u
#define MAX_BUCKET_SIZE_LOG2 17u

#define STACKS_MEM_BUF_SIZE \
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

  return Val_unit;
}

#define INVALID_BUF UINT_MAX
static inline unsigned int
which_buf(unsigned int len)
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

static void
malloc_uv_buf_t(uv_buf_t * buf, size_t len, enum cb_type cb_type)
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

static void
free_uv_buf_t_const(const uv_buf_t * buf, enum cb_type cb_type)
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

static void
free_uv_buf_t(uv_buf_t * buf, enum cb_type cb_type)
{
  free_uv_buf_t_const(buf,cb_type);
  buf->len = 0;
  buf->base = NULL;
}

static void
free_struct_req(struct req *r)
{
  if ( r->cb_type == CB_LWT ){
    mem_stack_free(&stack_struct_req,r);
  }
  else {
    free(r);
  }
}

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

static void
free_mem_uv_req_t(struct req * wp)
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

int uwt_is_safe_string (value str)
{
  return ( caml_string_length(str) == strlen(String_val(str)) );
}

#define malloc_struct_handle()                  \
  mem_stack_pop(&stack_struct_handle)

#define free_struct_handle(x)                   \
  mem_stack_free(&stack_struct_handle,x)

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

static void
free_mem_uv_handle_t(struct handle * h)
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
req_free_common(struct req * wp)
{
  if ( wp->cb != CB_INVALID ){
    gr_root_unregister(&wp->cb);
  }
  if ( wp->sbuf != CB_INVALID ){
    gr_root_unregister(&wp->sbuf);
  }
  if ( wp->buf.base != NULL && wp->buf_contains_ba == 0 ){
    free_uv_buf_t(&wp->buf,wp->cb_type);
  }
  wp->buf.base = NULL;
  wp->buf.len = 0;
  if ( wp->req ){
    if ( wp->clean_cb != NULL ){
      wp->clean_cb(wp->req);
      wp->clean_cb = NULL;
    }
    free_mem_uv_req_t(wp);
  }
}

static void
req_free(struct req * wp){
  if ( wp ){
    req_free_common(wp);
    free_struct_req(wp);
  }
}

/*
  like above, but don't clean wp itself.
  There may still exist a reference to it in the ocaml heap.
  The finalizer will clean up the rest
*/
static void
req_free_most(struct req * wp)
{
  if ( wp ){
    req_free_common(wp);
    wp->in_use = 0;
    if ( wp->finalize_called ){
      free_struct_req(wp);
    }
  }
}

static value
handle_create(int handle_type, struct loop *l)
{
  value res;
  struct handle * wp;
  static uintnat hcnt;
  const enum cb_type cb_type = l->loop_type;
  res = caml_alloc_custom(&ops_uwt_handle, sizeof(intnat)*3, 0, 1);
  Field(res,1) = 0;
  wp = malloc_struct_handle();
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
    free_struct_handle(wp);
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

static void
handle_free_common(struct handle *s)
{
  if ( !s ) {
    return;
  }
  if ( s->cb_listen != CB_INVALID ){
    gr_root_unregister(&s->cb_listen);
  }
  if ( s->cb_listen_server != CB_INVALID ){
    gr_root_unregister(&s->cb_listen_server);
  }
  if ( s->cb_read != CB_INVALID ){
    gr_root_unregister(&s->cb_read);
  }
  if ( s->cb_close != CB_INVALID ){
    gr_root_unregister(&s->cb_close);
  }
  if ( s->obuf != CB_INVALID ){
    gr_root_unregister(&s->obuf);
  }
  s->in_use_cnt = 0;
}

static void
handle_finalize_close_cb(uv_handle_t *h)
{
  struct handle * s = h->data;
  if ( s ){
    free_mem_uv_handle_t(s);
    if ( s->cb_listen != CB_INVALID ||
         s->cb_listen_server != CB_INVALID ||
         s->cb_read != CB_INVALID ||
         s->cb_close != CB_INVALID ||
         s->obuf != CB_INVALID ){
      GET_RUNTIME();
      handle_free_common(s);
    }
    free_struct_handle(s);
  }
}

static void
handle_finalize_close(struct handle * s)
{
  uv_handle_t * h = s->handle;
  if ( !h ){
    handle_free_common(s);
    free_struct_handle(s);
  }
  else {
    s->close_called = 1;
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
      free_mem_uv_handle_t(s);
    }
    free_struct_handle(s);
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
      if ( s->handle == NULL ){
        s->cb_listen = CB_INVALID;
        s->cb_listen_server = CB_INVALID;
        s->cb_read = CB_INVALID;
        s->cb_close = CB_INVALID;
        s->obuf = CB_INVALID;
      }
    }
    if ( s->handle == NULL ){
      handle_free_common(s);
      free_struct_handle(s);
    }
    else {
      /* we might be in the wrong thread, defer the close call to later */
      if ( stack_struct_handles_to_close.pos <
           stack_struct_handles_to_close.size ){
        stack_struct_handles_to_close.s[stack_struct_handles_to_close.pos] = s;
        ++stack_struct_handles_to_close.pos;
      }
      else {
        bool added = stack_resize_add(&stack_struct_handles_to_close,s,false);
        if ( !added ){
          handle_free_common(s);
          DEBUG_PF("out of memory, handle can't be closed");
          /* handle_finalize_close(s);*/
        }
      }
    }
  }
}

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

static struct req *
req_create(uv_req_type typ, struct loop *l)
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
    free_struct_req(wp);
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

#define VAL_UWT_UNIT_RESULT(x)                   \
  ( (x) < 0 ? Val_uwt_int_result(x) : Val_long(0) )

#define VAL_UWT_INT_RESULT(x)                   \
  ( (x) < 0 ? Val_uwt_int_result(x) : Val_long(x))

ATTR_UNUSED static value
result_eunavail(void)
{
  value ret = caml_alloc_small(1,Error_tag);
  Field(ret,0) = VAL_UWT_ERROR_UWT_EUNAVAIL;
  return ret;
}

static void
close_garbage_collected_handles(void)
{
  unsigned int i;
  for ( i = 0 ; i < stack_struct_handles_to_close.pos ; ++i ){
    handle_finalize_close(stack_struct_handles_to_close.s[i]);
    stack_struct_handles_to_close.s[i] = NULL;
  }
  stack_struct_handles_to_close.pos = 0;
  if ( stack_struct_handles_to_close.size > 128 ){
    free(stack_struct_handles_to_close.s);
    stack_struct_handles_to_close.s = NULL;
    stack_struct_handles_to_close.size = 0;
  }
}

CAMLprim value
uwt_run_loop(value o_loop,value o_mode)
{
  struct loop * wp;
  wp = Loop_val(o_loop);
  value ret;
  if (unlikely( !wp )){
    ret = VAL_UWT_INT_RESULT_UWT_EBADF;
  }
  else if (unlikely( wp->in_use != 0 )){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
  }
  else {
    uv_loop_t * loop = &wp->loop;
    uv_run_mode m;
    int erg;
    wp->in_use = 1;
    if (stack_struct_handles_to_close.pos){
      close_garbage_collected_handles();
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
    wp->in_use = 0;
    ret = VAL_UWT_INT_RESULT(erg);
    /* TODO: handle this case
    if ( unlikely(wp->do_clean == 1 ) ){

    } */
  }
  return ret;
}

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

#if 0
/* not yet exposed */
CAMLprim value
uwt_loop_close(value o_loop)
{
  value ret;
  struct loop * wp;
  wp = Loop_val(o_loop);
  if ( !wp || !wp->loop ){
    ret = VAL_UWT_INT_RESULT_UWT_EBADF;
  }
  else if ( wp->in_use ){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
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

static void
cache_cleaner_init(uv_loop_t * l);

static void
runtime_acquire_prepare_init(struct loop * l);

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
      if ( uwt_global_wakeup == NULL ){
        uwt_global_wakeup = caml_named_value(UWT_WAKEUP_STRING);
        if ( uwt_global_wakeup == NULL ){
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

static void
add_exception(struct loop *l, value e)
{
  assert( Is_exception_result(e) );
  if ( uwt_global_exception_fun && l && l->loop_type == CB_LWT ){
    l->exn_caught = 1;
    caml_callback_exn(*uwt_global_exception_fun,Extract_exception(e));
  }
}

static value ret_uv_fs_result_unit(uv_req_t * r);
static value ret_unit_cparam(uv_req_t * r);

static void
universal_callback(uv_req_t * req)
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
    if ( wp_req->c_cb == ret_unit_cparam ){
      exn = VAL_UWT_UNIT_RESULT(wp_req->c_param);
    }
    else if ( wp_req->c_cb == ret_uv_fs_result_unit ){
      exn = VAL_UWT_UNIT_RESULT(((uv_fs_t*)req)->result);
    }
    else {
      exn = wp_req->c_cb(req);
    }
    exn = CAML_CALLBACK1(wp_req,cb,exn);
    if (unlikely( Is_exception_result(exn) )){
      add_exception(wp_req->loop,exn);
    }
    wp_req->in_cb = 0;
  }
  req_free_most(wp_req);
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
  struct req * req = req_create(type,l);
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
    /* control flow will go back to universal_callback / common_after_work_cb
       They can free wp  */
    wp->finalize_called = 1;
    Field(res,1) = 0;
  }
  return Val_unit;
}

/* {{{ Fs start */
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
      ((uv_fs_cb)universal_callback);                     \
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
        gr_root_register(&wp_req->cb,o_cb);               \
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
      req_free(wp_req);                                   \
    }                                                     \
  }                                                       \
  CAMLreturn(o_ret);                                      \
}

#define RSTART_5(name,tz,a,b,c,d,code)                                  \
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

#define RSTART_4(name,tz,a,b,c,code)                                  \
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

#define RSTART_3(name,tz,a,b,code)                        \
  CAMLprim value                                          \
  uwt_ ## name  (value a,value b,                         \
                 value o_loop, value o_req, value o_cb ){ \
  CAMLparam5(a,b,o_loop,o_req,o_cb);                      \
  R_WRAP(name,tz,code)

#define RSTART_2(name,tz,a,code)                          \
  CAMLprim value                                          \
  uwt_ ## name  (value a,                                 \
                 value o_loop, value o_req, value o_cb ){ \
  CAMLparam4(a,o_loop,o_req,o_cb);                        \
  R_WRAP(name,tz,code)

#define RSTART_xxx(a,n,z,...)                   \
  RSTART_ ## a (n,z,__VA_ARGS__)

#define RSTART_xx(a,n,z,...)                    \
  RSTART_xxx(a,n,z,__VA_ARGS__)

#define RSTART_x(n,z,...)                         \
  RSTART_xx(PP_NARG(__VA_ARGS__),n,z,__VA_ARGS__)

#define RSTART_u(n,...)                         \
  RSTART_x(n,ret_uv_fs_result_unit,__VA_ARGS__)

#define RSTART_c(n,...)                         \
  RSTART_x(n,n ## _cb,__VA_ARGS__)

#define RSTART_(i,n,...)                        \
  RSTART_ ## i (n,__VA_ARGS__)

#define RSTART(i,n,...)                         \
  RSTART_(i,n,__VA_ARGS__)

#define FSSTART(...)                            \
  RSTART(c,__VA_ARGS__)

#define UFSSTART(...)                           \
  RSTART(u,__VA_ARGS__)

static value
ret_uv_fs_result_unit(uv_req_t * r)
{
  uv_fs_t* req = (uv_fs_t*)r;
  return (VAL_UWT_UNIT_RESULT(req->result));
}

static value
ret_unit_cparam(uv_req_t * r)
{
  struct req * wp = r->data;
  return (VAL_UWT_UNIT_RESULT(wp->c_param));
}

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
      Field(ret,0) = VAL_UWT_ERROR_UWT_UNKNOWN;
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

FSSTART(fs_open,o_name,o_flag_list,o_perm,{
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
  if ( result == UV_EOF || result == 0 ){
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

#ifndef _WIN32
#define FD_VAL(x) (Long_val(x))
#else
#define FD_VAL(x) (CRT_fd_val(x))
#endif

FSSTART(fs_read,o_file,o_buf,o_offset,o_len,{
  const size_t slen = (size_t)Long_val(o_len);
  struct req * wp = wp_req;
  size_t offset = Long_val(o_offset);
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  if ( slen > ULONG_MAX ){
    ret = UV_UWT_EINVAL;
  }
  else {
    if ( ba ){
      wp->buf.len = slen;
      wp->buf.base = Ba_buf_val(o_buf) + offset;
    }
    else {
      malloc_uv_buf_t(&wp->buf,slen,wp->cb_type);
    }
    if ( slen && wp->buf.base == NULL ){
      ret = UV_ENOMEM;
    }
    else {
      wp->offset = offset;
      wp->buf_contains_ba = ba;
      BLOCK({
          ret = uv_fs_read(loop, req, fd, &wp->buf, 1, -1, cb);
        });
      if ( ret >= 0 ){
        gr_root_register(&wp->sbuf,o_buf);
      }
      else {
        if ( !ba ){
          free_uv_buf_t(&wp->buf,wp->cb_type);
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

FSSTART(fs_write,
        o_file,
        o_buf,
        o_pos,
        o_len,{
  const size_t slen = (size_t)Long_val(o_len);
  struct req * wp = wp_req;
  const int ba = slen && (Tag_val(o_buf) != String_tag);
  const int fd = FD_VAL(o_file);
  if ( slen > ULONG_MAX ){
    ret = UV_UWT_EINVAL;
  }
  else {
    if ( ba ){
      wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
      wp->buf.len = slen;
    }
    else {
      malloc_uv_buf_t(&wp->buf,slen,wp->cb_type);
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
      wp->buf_contains_ba = ba;
      BLOCK({
          ret = uv_fs_write(loop, req, fd, &wp->buf, 1, -1, cb);
        });
      if ( ret >= 0 ){
        if ( ba ){
          gr_root_register(&wp->sbuf,o_buf);
        }
      }
      else {
        if ( ba == 0 ){
          free_uv_buf_t(&wp->buf,wp->cb_type);
        }
        wp->buf_contains_ba = 0;
        wp->buf.base = NULL;
        wp->buf.len = 0;
      }
    }
  }
})

UFSSTART(fs_close,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({
        ret = uv_fs_close(loop,req,fd,cb);
      });
})

UFSSTART(fs_unlink,o_path,{
  COPY_STR1(o_path,{
    BLOCK({
      ret = uv_fs_unlink(loop,req,STRING_VAL(o_path),cb);
    });
  });
})

UFSSTART(fs_mkdir,o_path,o_mode,{
  COPY_STR1(o_path,{
      BLOCK({
          ret = uv_fs_mkdir(loop,req,STRING_VAL(o_path),Long_val(o_mode),cb);});
    });
})

UFSSTART(fs_rmdir,o_path,{
  COPY_STR1(o_path,{
      BLOCK({ret = uv_fs_rmdir(loop,req,STRING_VAL(o_path),cb);});
    });
})

UFSSTART(fs_rename,o_old,o_new,{
  COPY_STR2(o_old,o_new,{
      BLOCK({ret = uv_fs_rename(loop,req,STRING_VAL(o_old),
                                STRING_VAL(o_new),cb);});
    });
})

UFSSTART(fs_link,o_old,o_new,{
    COPY_STR2(o_old,o_new,{
        BLOCK({ret = uv_fs_link(loop,req,STRING_VAL(o_old),
                                STRING_VAL(o_new),cb);});
      });
})

UFSSTART(fs_fsync,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fsync(loop,req,fd,cb);});
})

UFSSTART(fs_fdatasync,o_fd,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fdatasync(loop,req,fd,cb);});
})

UFSSTART(fs_ftruncate,o_fd,o_off,{
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

FSSTART(fs_sendfile,o_outfd,o_infd,o_offset,o_len,{
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

FSSTART(fs_scandir,o_path,{
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
    value s = s_caml_copy_string(req->path);
    Begin_roots1(s);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = s;
    End_roots();
  }
  return param;
}

FSSTART(fs_mkdtemp,o_path,{
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
    value s = s_caml_copy_string(req->ptr);
    Begin_roots1(s);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = s;
    End_roots();
  }
  return param;
}

FSSTART(fs_readlink,o_path,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_readlink(loop,req,STRING_VAL(o_path),
                                    cb);});
      });
})

static const int
access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

UFSSTART(fs_access,o_path,o_list,{
    const int fl = SAFE_CONVERT_FLAG_LIST(o_list, access_permission_table);
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_access(loop,
                                  req,
                                  STRING_VAL(o_path),
                                  fl,
                                  cb);});
    });
})

UFSSTART(fs_chmod,o_path,o_mode,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_chmod(loop,
                                 req,
                                 STRING_VAL(o_path),
                                 Long_val(o_mode),
                                 cb
                                 );});
      });
})

UFSSTART(fs_fchmod,o_fd,o_mode,{
    const int fd = FD_VAL(o_fd);
    BLOCK({ret = uv_fs_fchmod(loop,
                              req,
                              fd,
                              Long_val(o_mode),
                              cb);});
})

UFSSTART(fs_chown,o_path,o_uid,o_gid,{
    COPY_STR1(o_path,{
        BLOCK({ret = uv_fs_chown(loop,
                                 req,
                                 STRING_VAL(o_path),
                                 Long_val(o_uid),
                                 Long_val(o_gid),
                                 cb);});
      });
})

UFSSTART(fs_fchown,o_fd,o_uid,o_gid,{
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

UFSSTART(fs_utime,o_p,o_atime,o_mtime,{
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

UFSSTART(fs_futime,o_fd,o_atime,o_mtime,{
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

UFSSTART(fs_symlink,o_opath,o_npath,o_mode,{
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
  if ( wp->c_cb == ret_uv_fs_result_unit ){
    ret = VAL_UWT_UNIT_RESULT(((uv_fs_t*)(wp->req))->result);
  }
  else {
    ret = wp->c_cb(wp->req);
  }
  Field(o_req,1) = 0;
  req_free(wp);
  CAMLreturn(ret);
}

static value
uv_stat_to_value(const uv_stat_t * sb)
{
  CAMLparam0();
  CAMLlocal5(atime,omtime,octime,btime,size);
  value v;
  value s;

  size = caml_copy_int64(sb->st_size);
  atime = caml_copy_int64(sb->st_atim.tv_sec);
  omtime = caml_copy_int64(sb->st_mtim.tv_sec);
  octime = caml_copy_int64(sb->st_ctim.tv_sec);
  btime = caml_copy_int64(sb->st_birthtim.tv_sec);

  switch ( sb->st_mode & S_IFMT ){
  case S_IFREG: v = Val_long(0); break;
  case S_IFDIR: v = Val_long(1); break;
  case S_IFCHR: v = Val_long(2); break;
  case S_IFBLK: v = Val_long(3); break;
  case S_IFLNK: v = Val_long(4); break;
  case S_IFIFO: v = Val_long(5); break;
  case S_IFSOCK: v = Val_long(6); break;
  default: v = Val_long(7);
  }
  s = caml_alloc_small(21,0);
  Field(s,1) = v;
  Field(s,2) = Val_long(sb->st_mode & 07777);

#define SET_INT(n,f)                            \
  Field(s,n) = Val_long(sb->f)
  SET_INT(0,st_dev);
  SET_INT(3,st_nlink);
  SET_INT(4,st_uid);
  SET_INT(5,st_gid);
  SET_INT(6,st_rdev);
  SET_INT(7,st_ino);
  SET_INT(9,st_blksize);
  SET_INT(10,st_blocks);
  SET_INT(11,st_flags);
  SET_INT(12,st_gen);

  SET_INT(14,st_atim.tv_nsec);
  SET_INT(16,st_mtim.tv_nsec);
  SET_INT(18,st_ctim.tv_nsec);
  SET_INT(20,st_birthtim.tv_nsec);
#undef SET_INT

  Field(s,8) = size;
  Field(s,13) = atime;
  Field(s,15) = omtime;
  Field(s,17) = octime;
  Field(s,19) = btime;

  CAMLreturn(s);
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
    value st = uv_stat_to_value(&req->statbuf);
    Begin_roots1(st);
    param = caml_alloc_small(1,Ok_tag);
    Field(param,0) = st;
    End_roots();
  }
  return param;
}

FSSTART(fs_stat,o_file,{
    COPY_STR1(o_file,{
        BLOCK({
            ret = uv_fs_stat(loop,req,STRING_VAL(o_file),cb);
              });
      });
})

#define fs_lstat_cb fs_stat_cb
FSSTART(fs_lstat,o_file,{
    COPY_STR1(o_file,{
        BLOCK({
            ret = uv_fs_lstat(loop,req,STRING_VAL(o_file),cb);});
      });
})

#define fs_fstat_cb fs_stat_cb
FSSTART(fs_fstat,o_file,{
    int fd = FD_VAL(o_file);
    BLOCK({
        ret = uv_fs_fstat(loop,req,fd,cb);
          });
})

#if HAVE_DECL_UV_FS_REALPATH
#define fs_realpath_cb fs_readlink_cb
FSSTART(fs_realpath,o_path,{
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
    req_free(wp);
  }
  return Val_unit;
}
#undef FSSTART
#undef UFSSTART
/* }}} Fs end */

/* {{{ Handle start */
#define HANDLE_CB_INIT(x)                             \
  struct handle *h_ = NULL;                           \
  do {                                                \
    uv_handle_t *x_ = (uv_handle_t*)(x);              \
    if (unlikely( !x_ || (h_ = x_->data) == NULL )){  \
      DEBUG_PF("data lost");                          \
      return;                                         \
    }                                                 \
    if (unlikely( h_->close_called )){                \
      DEBUG_PF("callback called after close!");       \
      return;                                         \
    }                                                 \
    ++h_->in_callback_cnt;                            \
    GET_RUNTIME();                                    \
  } while (0)

#define HANDLE_CB_INIT_WITH_CLEAN(x)                  \
  struct handle *h_ = NULL;                           \
  do {                                                \
    uv_handle_t *x_ = (uv_handle_t*)(x);              \
    if (unlikely( !x_ || (h_ = x_->data) == NULL )){  \
      DEBUG_PF("data lost");                          \
      return;                                         \
    }                                                 \
    ++h_->in_callback_cnt;                            \
    GET_RUNTIME();                                    \
  } while (0)


#define MAYBE_CLOSE_HANDLE(s)                   \
  do {                                          \
    struct handle * h__  = s;                   \
    if (unlikely( h__->in_use_cnt == 0 &&       \
                  h__->in_callback_cnt == 0 &&  \
                  h__->finalize_called == 1 &&  \
                  h__->close_called == 0 )){    \
      handle_finalize_close(h__);               \
    }                                           \
  } while (0)

#define MAYBE_SAVE_EXN(s)                       \
  do {                                          \
    value v__ = (s);                            \
    if (unlikely( Is_exception_result(v__) )){  \
      add_exception(h_->loop,v__);              \
    }                                           \
  } while(0)

#define HANDLE_CB_RET(val)                      \
  do {                                          \
    value v_ = (val);                           \
    MAYBE_SAVE_EXN(v_);                         \
    --h_->in_callback_cnt;                      \
    MAYBE_CLOSE_HANDLE(h_);                     \
  } while (0)

#define HANDLE_IS_INVALID(_xs)                            \
  (unlikely( !_xs || !_xs->handle || _xs->close_called ))

#define HANDLE_IS_INVALID_UNINIT(_xs)                     \
  (unlikely( !_xs || !_xs->handle || _xs->close_called || \
             _xs->initialized == 0 ))

#define HANDLE_NINIT_END GR_ROOT_ENLARGE

#define HANDLE_NCHECK(_xs)                      \
  do {                                          \
    if ( HANDLE_IS_INVALID(_xs) ){              \
      return VAL_UWT_INT_RESULT_UWT_EBADF;      \
    }                                           \
  } while (0)

#define HANDLE_NO_UNINIT_CLOSED_INT_RESULT(xs)            \
  do {                                                    \
    struct handle * p_ = Handle_val(xs);                  \
    if ( HANDLE_IS_INVALID_UNINIT(p_) ){                  \
      return VAL_UWT_INT_RESULT_UWT_EBADF;                \
    }                                                     \
  } while (0)

#define HANDLE_NO_UNINIT_CLOSED_WRAP(xs)                  \
  do {                                                    \
    struct handle * p_ = Handle_val(xs);                  \
    if ( HANDLE_IS_INVALID_UNINIT(p_) ){                  \
      value ret = caml_alloc_small(1,Error_tag);          \
      Field(ret,0) = VAL_UWT_ERROR_UWT_EBADF;             \
      return ret;                                         \
    }                                                     \
  } while (0)

#define HANDLE_NO_UNINIT_NA(_xs)                \
  do {                                          \
    if (unlikely( _xs->initialized == 0 )){     \
      return VAL_UWT_INT_RESULT_UWT_EBADF;      \
    }                                           \
  } while (0)


#define HANDLE_NINIT_XN(n,s,o_s,...)                  \
  struct handle * s = Handle_val(o_s);                \
  HANDLE_NCHECK(s);                                   \
  CAMLparam ## n (o_s,__VA_ARGS__);                   \
  HANDLE_NINIT_END()

#define HANDLE_NINIT2(s1,o_s1,s2,o_s2,x,y)      \
  struct handle * s1 = Handle_val(o_s1);        \
  struct handle * s2 = Handle_val(o_s2);        \
  HANDLE_NCHECK(s1);                            \
  HANDLE_NCHECK(s2);                            \
  CAMLparam4(o_s1,o_s2,x,y);                    \
  HANDLE_NINIT_END()

#define HANDLE_NINIT_X1(s,o_s)                  \
  struct handle * s = Handle_val(o_s);          \
  HANDLE_NCHECK(s);                             \
  CAMLparam1(o_s);                              \
  HANDLE_NINIT_END()

#define HANDLE_NINIT_X2(...)                    \
  HANDLE_NINIT_XN(2,__VA_ARGS__)

#define HANDLE_NINIT_X3(...)                    \
  HANDLE_NINIT_XN(3,__VA_ARGS__)

#define HANDLE_NINIT_X4(...)                    \
  HANDLE_NINIT_XN(4,__VA_ARGS__)

#define HANDLE_NINIT_H2(x,n,...)                \
  HANDLE_NINIT_X ## n (x, __VA_ARGS__ )

#define HANDLE_NINIT_H1(x,n,...)                \
  HANDLE_NINIT_H2(x,n,__VA_ARGS__)

#define HANDLE_NINIT(x,...)                           \
  HANDLE_NINIT_H1(x,PP_NARG(__VA_ARGS__),__VA_ARGS__)

#define HANDLE_NINIT_NA(s,o_s)                  \
  struct handle * s = Handle_val(o_s);          \
  HANDLE_NCHECK(s)

static void
close_cb(uv_handle_t* handle)
{
  struct handle *s = handle->data;
  if (unlikely( !s || s->cb_close == CB_INVALID )){
    DEBUG_PF("data lost");
  }
  else {
    value exn = Val_unit;
    GET_RUNTIME();
    ++s->in_callback_cnt;
    exn = CAML_CALLBACK1(s,cb_close,Val_unit);
    --s->in_callback_cnt;
    handle_free_common(s);
    s->close_executed = 1;
    if (likely( s->in_callback_cnt == 0 )){
      free_mem_uv_handle_t(s);
      if ( s->finalize_called ){
        free_struct_handle(s);
      }
    }
    else {
      DEBUG_PF("close_cb not the last callback?");
    }
    if (unlikely( Is_exception_result(exn) )){
      add_exception(s->loop,exn);
    }
  }
}

static void cancel_reader(struct handle *h);
CAMLprim value
uwt_close_wait(value o_stream,value o_cb)
{
  struct handle * s = Handle_val(o_stream);
  if ( HANDLE_IS_INVALID(s) ){
    return VAL_UWT_INT_RESULT_UWT_EBADF;
  }
  if (unlikely( s->cb_close != CB_INVALID )){
    return VAL_UWT_INT_RESULT_UWT_EBUSY;
  }
  CAMLparam2(o_stream,o_cb);
  GR_ROOT_ENLARGE();
  ++s->in_use_cnt;
  s->close_called = 1;
  /* This way, we can't wrap uv_is_closing.
     But otherwise we "leak" memory until
     the ocaml grabage collector finalizes
     the handle  */
  Field(o_stream,1) = 0;
  if ( s->read_waiting ){
    cancel_reader(s);
  }
  gr_root_register(&s->cb_close,o_cb);
  uv_close(s->handle,close_cb);
  s->finalize_called = 1;
  CAMLreturn(Val_long(0));
}

CAMLprim value
uwt_close_nowait(value o_stream)
{
  struct handle * s = Handle_val(o_stream);
  value ret = VAL_UWT_INT_RESULT_UWT_EBADF;
  if ( s && s->handle && s->close_called == 0 ){
    s->close_called = 1;
    Field(o_stream,1) = 0;
    if ( s->read_waiting ){
      cancel_reader(s);
    }
    s->finalize_called = 1;
    handle_finalize_close(s);
    ret = Val_unit;
  }
  return ret;
}

#define UV_HANDLE_BOOL(type,fun,uninit_ok)                  \
  CAMLprim value                                            \
  uwt_ ## fun ## _na(value o_stream)                        \
  {                                                         \
    value ret = Val_long(0);                                \
    struct handle * s = Handle_val(o_stream);               \
    if ( s && s->handle && (uninit_ok || s->initialized )){ \
      type* stream = (type*)s->handle;                      \
      if ( uv_ ## fun(stream) ){                            \
        ret = Val_long(1);                                  \
      }                                                     \
    }                                                       \
    return ret;                                             \
  }
UV_HANDLE_BOOL(uv_handle_t,is_active,true)
UV_HANDLE_BOOL(uv_handle_t,has_ref,true)


#define UV_HANDLE_VOID(name)                        \
  CAMLprim value                                    \
  uwt_ ## name ## _na(value o_stream)               \
  {                                                 \
    struct handle * s = Handle_val(o_stream);       \
    if ( s && s->handle && s->close_called == 0 ){  \
      uv_ ## name (s->handle);                      \
    }                                               \
    return Val_unit;                                \
  }

UV_HANDLE_VOID(ref)
UV_HANDLE_VOID(unref)
/* }}} Handle end */

/* {{{ Handle ext start */
CAMLprim value
uwt_get_buffer_size_common_na(value o_stream, value o)
{
  HANDLE_NINIT_NA(s,o_stream);
  HANDLE_NO_UNINIT_NA(s);
  int ret;
  int x = 0;
  if ( Long_val(o) == 0 ){
    ret = uv_send_buffer_size(s->handle,&x);
  }
  else {
    ret = uv_recv_buffer_size(s->handle,&x);
  }
  return (VAL_UWT_INT_RESULT(ret));
}

CAMLprim value
uwt_set_buffer_size_common_na(value o_stream, value o_len, value o)
{
  HANDLE_NINIT_NA(s,o_stream);
  HANDLE_NO_UNINIT_NA(s);
  int ret;
  int x = Long_val(o_len);
  if ( Long_val(o) == 0 ){
    ret = uv_send_buffer_size(s->handle,&x);
  }
  else {
    ret = uv_recv_buffer_size(s->handle,&x);
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}
/* }}} Handle ext end */

/* {{{ Timer start */
static void
timer_repeating_cb(uv_timer_t * handle)
{
  HANDLE_CB_INIT(handle);
  value exn = Val_unit;
  struct handle * wp = handle->data;
  if (unlikely( wp->cb_read == CB_INVALID ||
                wp->cb_listen == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value t = GET_CB_VAL(wp->cb_listen);
    exn = GET_CB_VAL(wp->cb_read);
    exn = caml_callback_exn(exn,t);
  }
  HANDLE_CB_RET(exn);
}

static void
timer_once_cb(uv_timer_t * handle)
{
  HANDLE_CB_INIT(handle);
  value exn = Val_unit;
  struct handle * wp = handle->data;
  if (unlikely( wp->cb_read == CB_INVALID || wp->cb_listen == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value timer = GET_CB_VAL(wp->cb_listen);
    exn = GET_CB_VAL(wp->cb_read);
    gr_root_unregister(&wp->cb_read);
    if ( wp->in_use_cnt ){
      wp->in_use_cnt--;
    }
    exn = caml_callback_exn(exn,timer);
    if ( wp->close_called == 0 ){
      if ( wp->cb_listen != CB_INVALID ){
        timer = GET_CB_VAL(wp->cb_listen); /* might have changed */
        Field(timer,1) = 0;
      }
      wp->finalize_called = 1;
      handle_finalize_close(wp);
    }
    gr_root_unregister(&wp->cb_listen);
  }
  HANDLE_CB_RET(exn);
}

#define INIT_LOOP_WRAP(x,y)                       \
  struct loop * x = Loop_val(y);                  \
  do {                                            \
    if (unlikely( !x || x->init_called == 0 ) ){  \
      value ret = caml_alloc_small(1,Error_tag);  \
      Field(ret,0) = VAL_UWT_ERROR_UWT_EFATAL;    \
      return ret;                                 \
    }                                             \
  } while(0)

#define INIT_LOOP_INT_RESULT(x,y)                 \
  struct loop * x = Loop_val(y);                  \
  do {                                            \
    if (unlikely( !x || x->init_called == 0 ) ){  \
      return VAL_UWT_INT_RESULT_UWT_EFATAL;       \
    }                                             \
  } while(0)

CAMLprim value
uwt_timer_start(value o_loop, value o_cb,
                value o_timeout, value o_repeat)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  const intnat l_timeout = Long_val(o_timeout);
  const intnat l_repeat = Long_val(o_repeat);

  GR_ROOT_ENLARGE();
  v = handle_create(UV_TIMER,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  h->close_executed = 0;
  Field(ret,0) = v;
  int erg = uv_timer_init(&l->loop,(uv_timer_t *)h->handle);
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
  }
  else {
    erg = uv_timer_start((uv_timer_t*)h->handle,
                         l_repeat != 0 ? timer_repeating_cb : timer_once_cb,
                         l_timeout,
                         l_repeat);
    if ( erg >= 0 ){
      h->in_use_cnt++;
      h->initialized = 1;
      gr_root_register(&h->cb_read,o_cb);
      gr_root_register(&h->cb_listen,v);
    }
    else {
      h->finalize_called = 1;
      handle_finalize_close(h);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}
/* }}} Timer end */

/* {{{ Stream start */
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
    universal_callback((void*)req);
    --s->in_callback_cnt;
    MAYBE_CLOSE_HANDLE(s);
  }
}

CAMLprim value
uwt_shutdown(value o_stream,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_NINIT(s,o_stream,o_cb);
  uv_stream_t* stream = (uv_stream_t*)s->handle;
  struct req * wp = req_create(UV_SHUTDOWN,s->loop);
  uv_shutdown_t * req = (uv_shutdown_t*)wp->req;
  const int erg = uv_shutdown(req,stream,shutdown_cb);
  value ret;
  if ( erg < 0 ){
    free_mem_uv_req_t(wp);
    free_struct_req(wp);
    ret = Val_uwt_int_result(erg);
  }
  else {
    wp->c_cb = ret_unit_cparam;
    gr_root_register(&wp->cb,o_cb);
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
  HANDLE_NINIT(s,o_stream,o_cb);
  int ret;
  if ( s->cb_listen_server != CB_INVALID ||
       s->cb_listen != CB_INVALID ){
    ret = UV_UWT_EBUSY;
  }
  else {
    uv_stream_t* stream = (uv_stream_t*)s->handle;
    ret = uv_listen(stream,Long_val(o_backlog),listen_cb);
    if ( ret >= 0 ){
      ++s->in_use_cnt;
      gr_root_register(&s->cb_listen,o_cb);
      gr_root_register(&s->cb_listen_server,o_stream);
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
    return VAL_UWT_INT_RESULT_UWT_EBADF;
  }
  int ret = uv_accept((uv_stream_t*)serv->handle,
                      (uv_stream_t*)client->handle);
  if ( ret >= 0 ){
    client->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

static void
read_start_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf)
{
  if ( !handle || !handle->data ){
    DEBUG_PF("no data");
    buf->base = NULL;
    buf->len = 0;
  }
  else {
    const struct handle * h = handle->data;
    const size_t len = UMIN(suggested_size,h->c_read_size);
    malloc_uv_buf_t(buf,len,h->cb_type);
  }
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
      free_uv_buf_t_const(buf,h->cb_type);
      Begin_roots1(ret);
      o = caml_alloc_small(1,tag);
      Field(o,0) = ret;
      cb = GET_CB_VAL(h->cb_read);
      if ( finished == 1 ){
        h->cb_read_removed_by_cb = 1;
        gr_root_unregister(&h->cb_read);
        if ( h->in_use_cnt ){
          h->in_use_cnt--;
        }
      }
      End_roots();
      ret = caml_callback_exn(cb,o);
    }
  }
  if ( buf_not_cleaned && buf->base ){
    free_uv_buf_t_const(buf,h->cb_type);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_read_start(value o_stream,
               value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_NINIT(s,o_stream,o_cb);
  value ret;
  if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
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
      erg = uv_read_start(stream,read_start_alloc_cb,read_start_cb);
      if ( erg >= 0 ){
        s->c_read_size = DEF_ALLOC_SIZE;
        s->cb_read_removed_by_cb = 0;
        gr_root_register(&s->cb_read,o_cb);
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
  HANDLE_NINIT(s,o_stream);
  value ret;
  if ( (s->read_waiting == 1 && Long_val(o_abort) == 0 ) ||
       (s->cb_read == CB_INVALID && s->cb_read_removed_by_cb != 1) ){
    ret = VAL_UWT_INT_RESULT_UWT_ENOTACTIVE;
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
        gr_root_unregister(&s->cb_read);
      }
      s->cb_read_removed_by_cb = 0;
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

static void
cancel_reader(struct handle *h)
{
  if ( h->read_waiting == 1 &&
       h->cb_read != CB_INVALID &&
       h->obuf != CB_INVALID ){
    value exn;
    value param;
    if ( h->handle->type == UV_UDP ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = VAL_UWT_ERROR_ECANCELED;
    }
    else {
      param = VAL_UWT_INT_RESULT_ECANCELED;
    }
    value cb = GET_CB_VAL(h->cb_read);
    gr_root_unregister(&h->cb_read);
    gr_root_unregister(&h->obuf);
    ++h->in_callback_cnt;
    ++h->in_use_cnt;
    assert( h->close_called == 1 );
    exn = caml_callback2_exn(*uwt_global_wakeup,cb,param);
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
    if (unlikely( Is_exception_result(exn) )){
      add_exception(h->loop,exn);
    }
  }
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
        if ( nread == UV_EOF ){
          o = Val_long(0);
        }
        else {
          o = Val_uwt_int_result(nread);
        }
        finished = true;
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
        free_uv_buf_t_const(buf,h->cb_type);
      }
      cb = GET_CB_VAL(h->cb_read);
      gr_root_unregister(&h->cb_read);
      gr_root_unregister(&h->obuf);
      h->can_reuse_cb_read = finished == false;
      if ( h->in_use_cnt ){
        h->in_use_cnt--;
      }
      ret = caml_callback2_exn(*uwt_global_wakeup,cb,o);
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
    free_uv_buf_t_const(buf,h->cb_type);
  }
  HANDLE_CB_RET(ret);
}

static void
alloc_read_own(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf)
{
  struct handle * h;
  (void) suggested_size;
  if (unlikely( !handle || (h = handle->data) == NULL )){
    DEBUG_PF("no data");
    buf->len = 0;
    buf->base = NULL;
  }
  else if ( h->use_read_ba ){
    buf->base = h->ba_read;
    buf->len = h->c_read_size;
  }
  else {
    size_t len;
    len = UMIN(suggested_size,h->c_read_size);
    malloc_uv_buf_t(buf,len,h->cb_type);
  }
}

CAMLprim value
uwt_read_own(value o_s,value o_buf,value o_offset,value o_len,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_s);
  HANDLE_NINIT(s,o_s,o_buf,o_cb);
  const int ba = Tag_val(o_buf) != String_tag;
  value ret;
  assert( s->cb_type == CB_LWT );
  if ( s->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
  }
  else {
    int erg;
    if ( s->can_reuse_cb_read == 1 ){
      s->can_reuse_cb_read = 0;
      erg = 0;
    }
    else {
      uv_stream_t* stream = (uv_stream_t*)s->handle;
      erg = uv_read_start(stream,alloc_read_own,read_own_cb);
    }
    if ( erg >= 0 ){
      size_t len = Long_val(o_len);
      size_t offset = Long_val(o_offset);
      gr_root_register(&s->cb_read,o_cb);
      gr_root_register(&s->obuf,o_buf);
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
      universal_callback((void*)req);                                   \
      --s->in_callback_cnt;                                             \
      MAYBE_CLOSE_HANDLE(s);                                            \
    }                                                                   \
  }

/* TODO: check the alignment, if we can cast or not */
XX(write_send_cb,uv_write_t)
XX(udp_send_cb,uv_udp_send_t)
#undef XX

CAMLprim value
uwt_udp_send_native(value o_stream,value o_buf,value o_pos,value o_len,
                    value o_sock,value o_cb)
{
  const size_t len = Long_val(o_len);
  if ( len > ULONG_MAX ){
    return VAL_UWT_INT_RESULT_UWT_EINVAL;
  }
  union all_sockaddr addr;
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  else {
    if ( !uwt_get_sockaddr(o_sock,&addr) ){
      return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
    }
  }
  HANDLE_NINIT(s,o_stream,o_buf,o_sock,o_cb);
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  struct req * wp;
  wp = req_create( o_sock == Val_unit ? UV_WRITE : UV_UDP_SEND,
                   s->loop );
  value ret = Val_unit;
  if ( ba ){
    wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
    wp->buf.len = len;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    malloc_uv_buf_t(&wp->buf,len,wp->cb_type);
    if ( wp->buf.base != NULL  ){
      memcpy(wp->buf.base,
             String_val(o_buf) + Long_val(o_pos),
             len);
    }
    else {
      ret = VAL_UWT_INT_RESULT_ENOMEM;
      free_mem_uv_req_t(wp);
      free_struct_req(wp);
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
      erg = uv_udp_send(req,handle,&wp->buf,1,&addr.addr,udp_send_cb);
    }
    if ( erg < 0 ){
      if ( ba == 0 ){
        free_uv_buf_t(&wp->buf,wp->cb_type);
      }
      free_mem_uv_req_t(wp);
      free_struct_req(wp);
    }
    else {
      wp->c_cb = ret_unit_cparam;
      wp->cb_type = s->cb_type;
      wp->in_use = 1;
      s->initialized = 1;
      gr_root_register(&wp->cb,o_cb);
      wp->finalize_called = 1;
      ++s->in_use_cnt;
      wp->buf_contains_ba = ba;
      if ( ba ){
        gr_root_register(&wp->sbuf,o_buf);
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}
BYTE_WRAP6(uwt_udp_send)

CAMLprim value
uwt_write(value a,value b,value c,value d,value e){
  return (uwt_udp_send_native(a,b,c,d,Val_unit,e));
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
    universal_callback((void*)req);

    --s1->in_callback_cnt;
    --s2->in_callback_cnt;
    MAYBE_CLOSE_HANDLE(s1);
    MAYBE_CLOSE_HANDLE(s2);
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
    return VAL_UWT_INT_RESULT_UWT_EINVAL;
  }
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream_send);
  HANDLE_NINIT2(s1,o_stream,s2,o_stream_send,o_cb,o_buf);
  value ret = Val_unit;
  struct req * wp = req_create(UV_WRITE,s1->loop);
  uv_write_t* req = (uv_write_t*)wp->req;
  const int ba = len > 0 && Tag_val(o_buf) != String_tag;
  if ( ba ){
    wp->buf.base = Ba_buf_val(o_buf) + Long_val(o_pos);
    wp->buf.len = len;
  }
  else if ( len == 0 ){
    wp->buf.base = NULL;
    wp->buf.len = 0;
  }
  else {
    malloc_uv_buf_t(&wp->buf,len,wp->cb_type);
    if ( wp->buf.base != NULL ){
      memcpy(wp->buf.base,
             String_val(o_buf) + Long_val(o_pos),
             len);
    }
    else {
      ret = VAL_UWT_INT_RESULT_ENOMEM;
      free_mem_uv_req_t(wp);
      free_struct_req(wp);
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
        free_uv_buf_t(&wp->buf,wp->cb_type);
      }
      free_mem_uv_req_t(wp);
      free_struct_req(wp);
    }
    else {
      wp->c_cb = ret_unit_cparam;
      wp->in_use = 1;
      gr_root_register(&wp->cb,o_cb);
      wp->finalize_called = 1;
      ++s1->in_use_cnt;
      ++s2->in_use_cnt;
      wp->buf_contains_ba = ba;
      if ( ba ){
        gr_root_register(&wp->sbuf,o_buf);
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
  if ( o_sock == Val_unit ){
    HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_stream);
  }
  HANDLE_NINIT_NA(s,o_stream);
  uv_buf_t buf;
  int ret;
  buf.len = Long_val(o_len);
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
    union all_sockaddr addr;
    if ( ! uwt_get_sockaddr(o_sock,&addr) ) {
      return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
    }
    else {
      ret = uv_udp_try_send((uv_udp_t*)s->handle,&buf,1,&addr.addr);
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
/* }}} Stream end */

/* {{{ Tty start */
CAMLprim value
uwt_tty_init(value o_loop,value o_fd, value o_readable)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam1(o_loop);
  CAMLlocal1(dc);
  value ret;
  const int fd = FD_VAL(o_fd);
  dc = handle_create(UV_TTY,l);
  struct handle * h =  Handle_val(dc);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  h->close_executed = 0;
  h->initialized = 1;
  Field(ret,0) = dc;

  const int erg = uv_tty_init(&l->loop,
                              (uv_tty_t*)h->handle,
                              fd,
                              Long_val(o_readable) == 1 );
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
    Field(dc,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_tty_set_mode_na(value o_tty,value o_mode)
{
  HANDLE_NINIT_NA(s,o_tty);
  HANDLE_NO_UNINIT_NA(s);
#if HAVE_DECL_UV_TTY_MODE_IO && HAVE_DECL_UV_TTY_MODE_NORMAL && HAVE_DECL_UV_TTY_MODE_RAW
  int mode ;
  switch ( Long_val(o_mode) ){
  case 2: mode = UV_TTY_MODE_IO; break;
  case 1: mode = UV_TTY_MODE_RAW; break;
  default: assert(false); /* fall */
  case 0: mode = UV_TTY_MODE_NORMAL; break;
  }
#else
  int mode = Long_val(o_mode) == 0 ? 0 : 1;
#endif
  int ret = uv_tty_set_mode((uv_tty_t*)s->handle,mode);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tty_reset_mode_na(value unit)
{
  (void) unit;
  int x = uv_tty_reset_mode();
  return (VAL_UWT_UNIT_RESULT(x));
}

CAMLprim value
uwt_tty_get_winsize(value o_tty)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(o_tty);
  CAMLparam1(o_tty);
  CAMLlocal1(tup);
  struct handle * s = Handle_val(o_tty);
  value ret;
  int width;
  int height;
  const int erg = uv_tty_get_winsize((uv_tty_t*)s->handle,&width,&height);
  if ( erg < 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(erg);
  }
  else {
    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(width);
    Field(tup,1) = Val_long(height);
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = tup;
  }
  CAMLreturn(ret);
}
/* }}} Tty end */

#ifdef _WIN32
static bool
set_crt_fd(value o_fd)
{
  bool ret = true;
  if ( CRT_fd_val(o_fd) == NO_CRT_FD ){
    const int fd = _open_osfhandle((intptr_t)Handle_val(o_fd), _O_BINARY);
    if ( fd == -1 ){
      ret = false;
    }
    else {
      CRT_fd_val(o_fd) = fd;
    }
  }
  return ret;
}
#endif

/* {{{ Pipe start */
#define FD_INVALID 2
CAMLprim value
uwt_pipe_open(value o_loop, value o_fd,value o_ipc)
{
  INIT_LOOP_WRAP(l,o_loop);
#ifdef _WIN32
  if ( o_fd != FD_INVALID && set_crt_fd(o_fd) == false ){
    value ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_UWT_EBADF;
    return ret;
  }
#endif
  CAMLparam1(o_loop);
  CAMLlocal1(dc);
  value ret;
  int erg;
  int fd = -1;
  if ( o_fd != FD_INVALID ){
    fd = FD_VAL(o_fd);
  }
  dc = handle_create(UV_NAMED_PIPE,l);
  struct handle * h = Handle_val(dc);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = dc;
  h->close_executed = 0;
  uv_pipe_t * p = (uv_pipe_t*)h->handle;
  erg = uv_pipe_init(&l->loop,
                     p,
                     Long_val(o_ipc) == 1);
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
  }
  else {
    if ( o_fd != FD_INVALID ){
#ifdef _WIN32
      h->orig_fd = fd;
#endif
      h->initialized = 1;
      erg = uv_pipe_open(p,fd);
      if ( erg < 0 ){
        h->finalize_called = 1;
        handle_finalize_close(h);
      }
    }
  }
  if ( erg < 0 ){
    Field(dc,1) = 0;
    Tag_val(ret) = Error_tag;
    Field(ret,0) = Val_uwt_error(erg);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_pipe_init(value o_loop, value o_ipc)
{
  _Static_assert( Is_long(FD_INVALID) == 0 , "int representation has changed");
  return (uwt_pipe_open(o_loop,FD_INVALID,o_ipc));
}
#undef FD_INVALID

CAMLprim value
uwt_pipe_bind_na(value o_pipe, value o_name)
{
  HANDLE_NINIT_NA(p,o_pipe);
  if ( !uwt_is_safe_string(o_name) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  /* string duplicated by libuv */
  int ret = uv_pipe_bind((uv_pipe_t*)p->handle,String_val(o_name));
  if ( ret >= 0 ){
    p->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

typedef int(*pipe_name)(const uv_pipe_t*, char*, size_t*);
static value
uwt_pipe_name(value o_pipe,pipe_name pfunc)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(o_pipe);
  CAMLparam1(o_pipe);
  CAMLlocal1(o_str);
  struct handle * op = Handle_val(o_pipe);
  size_t s = 1024;
  char name[s];
  char * lname = NULL;
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  int erg = pfunc(p,name,&s);
  int etag;
  if ( erg == UV_ENOBUFS ){
    ++s;
    lname = malloc(s);
    if ( lname == NULL ){
      erg = UV_ENOMEM;
    }
    else {
      erg = pfunc(p,lname,&s);
    }
  }
  if ( erg < 0 ){
    o_str = Val_uwt_error(erg);
    etag = Error_tag;
  }
  else {
    char * ms = lname ? lname : name;
#if UV_VERSION_MAJOR < 1
#error "libuv too old"
#endif
#if (UV_VERSION_MAJOR == 1) && (UV_VERSION_MINOR < 3)
    --s;
    assert(ms[s] == '\0');
#endif
    o_str =  caml_alloc_string(s);
    memcpy(String_val(o_str),ms,s);
    etag = Ok_tag;
  }
  if ( lname ){
    free(lname);
  }
  value ret = caml_alloc_small(1,etag);
  Field(ret,0) = o_str;
  CAMLreturn(ret);
}

CAMLprim value
uwt_pipe_getsockname(value o_pipe)
{
  return(uwt_pipe_name(o_pipe,uv_pipe_getsockname));
}

CAMLprim value
uwt_pipe_getpeername(value o_pipe)
{
#if HAVE_DECL_UV_PIPE_GETPEERNAME
  return (uwt_pipe_name(o_pipe,uv_pipe_getpeername));
#else
  (void) o_pipe;
  return (result_eunavail());
#endif
}

CAMLprim value
uwt_pipe_pending_instances_na(value o_pipe, value o_count)
{
  HANDLE_NINIT_NA(op,o_pipe);
  HANDLE_NO_UNINIT_NA(op);
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  uv_pipe_pending_instances(p,Long_val(o_count));
  return (Val_long(0));
}

CAMLprim value
uwt_pipe_pending_count_na(value o_pipe)
{
  HANDLE_NINIT_NA(op,o_pipe);
  HANDLE_NO_UNINIT_NA(op);
  uv_pipe_t* p = (uv_pipe_t*)op->handle;
  int ret = uv_pipe_pending_count(p);
  return (VAL_UWT_INT_RESULT(ret));
}

CAMLprim value
uwt_pipe_pending_type_na(value o_pipe)
{
  struct handle * h = Handle_val(o_pipe);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    return (Val_long(0));
  }
  uv_handle_type x = uv_pipe_pending_type((uv_pipe_t*)h->handle);
  switch( x ){
  case UV_UNKNOWN_HANDLE: /*fall */
  default: return (Val_long(0));
  case UV_TCP: return (Val_long(1));
  case UV_UDP: return (Val_long(2));
  case UV_NAMED_PIPE: return (Val_long(3));
  }
}

static void
pipe_tcp_connect_cb(uv_connect_t* req, int status)
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
    if ( status >= 0 ){
      s->initialized = 1;
    }
    universal_callback((void*)req);
    --s->in_callback_cnt;
    MAYBE_CLOSE_HANDLE(s);
  }
}

CAMLprim value
uwt_pipe_connect(value o_pipe,value o_path,value o_cb)
{
  if ( !uwt_is_safe_string(o_path) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  HANDLE_NINIT(s,o_pipe,o_cb,o_path);
  struct req * wp = req_create(UV_CONNECT,s->loop);
  uv_pipe_t* tpipe = (uv_pipe_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  /* pipe_connect is void in order to mimic the windows api,
     the string won't be used internally, we don't need to create a copy */
  uv_pipe_connect(req,tpipe,String_val(o_path),pipe_tcp_connect_cb);
  wp->c_cb = ret_unit_cparam;
  gr_root_register(&wp->cb,o_cb);
  wp->in_use = 1;
  wp->finalize_called = 1;
  ++s->in_use_cnt;
  CAMLreturn(Val_long(0));
}
/* }}} Pipe end */

/* {{{ Tcp start */
#define UDP_TCP_INIT(name,TYPE)                                         \
  CAMLprim value                                                        \
    uwt_ ## name ## _init(value o_loop)                                 \
  {                                                                     \
    INIT_LOOP_WRAP(l,o_loop);                                           \
    CAMLparam1(o_loop);                                                 \
    CAMLlocal1(v);                                                      \
    v = handle_create(TYPE,l);                                          \
    struct handle * h = Handle_val(v);                                  \
    h->close_executed = 1;                                              \
    value ret = caml_alloc_small(1,Ok_tag);                             \
    Field(ret,0) = v;                                                   \
    h->close_executed = 0;                                              \
    const int erg = uv_ ## name ## _init(&l->loop,                      \
                                         (uv_ ## name ## _t*)h->handle); \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      free_mem_uv_handle_t(h);                                          \
      free_struct_handle(h);                                            \
    }                                                                   \
    CAMLreturn(ret);                                                    \
  }

UDP_TCP_INIT(tcp,UV_TCP)
UDP_TCP_INIT(udp,UV_UDP)
#undef UDP_TCP_INIT

#if HAVE_DECL_UV_UDP_INIT_EX || HAVE_DECL_UV_TCP_INIT_EX
#define UDP_TCP_INIT_EX(name,TYPE)                                      \
  CAMLprim value                                                        \
    uwt_ ## name ## _init_ex(value o_loop, value o_mode)                \
  {                                                                     \
    INIT_LOOP_WRAP(l,o_loop);                                           \
    CAMLparam1(o_loop);                                                 \
    CAMLlocal1(v);                                                      \
    v = handle_create(TYPE,l);                                          \
    struct handle * h = Handle_val(v);                                  \
    h->close_executed = 1;                                              \
    value ret = caml_alloc_small(1,Ok_tag);                             \
    Field(ret,0) = v;                                                   \
    h->close_executed = 0;                                              \
    h->initialized = 1;                                                 \
    const int mode = Long_val(o_mode) == 0 ? AF_INET : AF_INET6 ;       \
    const int erg = uv_ ## name ## _init_ex(&l->loop,                   \
                                            (uv_ ## name ## _t*)h->handle, \
                                            mode );                     \
    if ( erg < 0 ){                                                     \
      Field(v,1) = 0;                                                   \
      Field(ret,0) = Val_uwt_error(erg);                                \
      Tag_val(ret) = Error_tag;                                         \
      free_mem_uv_handle_t(h);                                          \
      free_struct_handle(h);                                            \
    }                                                                   \
    CAMLreturn(ret);                                                    \
  }
#endif

#if !HAVE_DECL_UV_UDP_INIT_EX || !HAVE_DECL_UV_TCP_INIT_EX
#define UDP_TCP_INIT_EX_NO(name)                \
  CAMLprim value                                \
    uwt_ ## name ## _init_ex(value a, value b)  \
  {                                             \
    (void)a;                                    \
    (void)b;                                    \
    return (result_eunavail());                 \
  }
#endif

#if HAVE_DECL_UV_TCP_INIT_EX
UDP_TCP_INIT_EX(tcp,UV_TCP)
#else
UDP_TCP_INIT_EX_NO(tcp)
#endif

#if HAVE_DECL_UV_UDP_INIT_EX
UDP_TCP_INIT_EX(udp,UV_UDP)
#else
UDP_TCP_INIT_EX_NO(udp)
#endif

#ifdef UDP_TCP_INIT_EX
#undef UDP_TCP_INIT_EX
#endif

#ifdef UDP_TCP_INIT_EX_NO
#undef UDP_TCP_INIT_EX_NO
#endif

static value
uwt_tcp_udp_open(value o_tcp, value o_fd, bool tcp)
{
  HANDLE_NINIT_NA(t,o_tcp);
  int ret;
#ifndef _WIN32
  uv_os_sock_t s = Long_val(o_fd);
#else
  uv_os_sock_t s;
  if ( Descr_kind_val(o_fd) != KIND_SOCKET ){
    ret = UV_UWT_EBADF;
    goto endp;
  }
  s = Socket_val(o_fd);
  t->orig_fd = CRT_fd_val(o_fd);
#endif
  if ( tcp ){
    ret = uv_tcp_open((uv_tcp_t*)t->handle,s);
  }
  else {
    ret = uv_udp_open((uv_udp_t*)t->handle,s);
  }
  if ( ret >= 0 ){
    t->initialized = 1;
  }
#ifdef _WIN32
 endp:
#endif
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a,b,true));
}

CAMLprim value
uwt_udp_open_na(value a, value b){
  return (uwt_tcp_udp_open(a,b,false));
}

CAMLprim value
uwt_tcp_bind_na(value o_tcp, value o_sock, value o_flags)
{
  HANDLE_NINIT_NA(t,o_tcp);
  union all_sockaddr addr;
  if ( !uwt_get_sockaddr(o_sock,&addr) ){
    return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
  }
  unsigned int flags = o_flags == Val_unit ? 0 : UV_TCP_IPV6ONLY;
  int ret = uv_tcp_bind((uv_tcp_t *)t->handle,&addr.addr,flags);
  if ( ret >= 0 ){
    t->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_nodelay_na(value o_tcp,value o_enable)
{
  HANDLE_NINIT_NA(th,o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_nodelay(t,Long_val(o_enable));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_keepalive_na(value o_tcp,value o_enable, value o_delay)
{
  HANDLE_NINIT_NA(th,o_tcp);
  /* uninit allowed */
  uv_tcp_t * t = (uv_tcp_t *)th->handle;
  int ret = uv_tcp_keepalive(t,Long_val(o_enable),Long_val(o_delay));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_tcp_simultaneous_accepts_na(value o_tcp,value o_enable)
{
  HANDLE_NINIT_NA(th,o_tcp);
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
  union all_sockaddr addr;
  r = func(t,&addr.addr,&s);
  if ( r < 0 ) {
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(r);
  }
  else {
    sock = uwt_alloc_sockaddr(&addr);
    if ( sock == Val_unit ){
      ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_UWT_UNKNOWN;
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
  union all_sockaddr addr;
  if ( ! uwt_get_sockaddr(o_sock,&addr) ){
    return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
  }
  HANDLE_NINIT(s,o_tcp,o_sock,o_cb);
  struct req * wp = req_create(UV_CONNECT,s->loop);
  uv_tcp_t* tcp = (uv_tcp_t*)s->handle;
  uv_connect_t * req = (uv_connect_t*)wp->req;
  const int ret = uv_tcp_connect(req, tcp, &addr.addr, pipe_tcp_connect_cb);
  if ( ret >= 0 ){
    wp->c_cb = ret_unit_cparam;
    gr_root_register(&wp->cb,o_cb);
    wp->in_use = 1;
    wp->finalize_called = 1;
    ++s->in_use_cnt;
  }
  else {
    free_mem_uv_req_t(wp);
    free_struct_req(wp);
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(ret));
}
/* }}} Tcp end */

/* {{{ Udp start */
/* some functions are defined together with tcp or stream! */
static const int udp_bin_flag_table[2] = {
  UV_UDP_IPV6ONLY, UV_UDP_REUSEADDR
};

CAMLprim value
uwt_udp_bind_na(value o_udp, value o_sock, value o_flags){
  union all_sockaddr addr;
  if ( !uwt_get_sockaddr(o_sock,&addr) ){
    return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
  }
  HANDLE_NINIT_NA(t,o_udp);
  const unsigned int flags = SAFE_CONVERT_FLAG_LIST(o_flags,udp_bin_flag_table);
  const int ret = uv_udp_bind((uv_udp_t *)t->handle,&addr.addr,flags);
  if ( ret >= 0 ){
    t->initialized = 1;
  }
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_membership_na(value o_udp, value o_mul,
                          value o_int, value o_mem)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  uv_membership membership = Long_val(o_mem) ? UV_JOIN_GROUP : UV_LEAVE_GROUP;
  const char* multicast_addr = String_val(o_mul);
  char* interface_addr = NULL;
  if ( !uwt_is_safe_string(o_mul) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  if ( o_int != Val_unit ){
    value s = Field(o_int,0);
    if ( !uwt_is_safe_string(s) ){
      return VAL_UWT_INT_RESULT_ECHARSET;
    }
    interface_addr = String_val(s);
  }
  int ret = uv_udp_set_membership((uv_udp_t*)u->handle,
                                  multicast_addr,
                                  interface_addr,
                                  membership);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_multicast_loop_na(value o_udp, value o_b)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  int ret = uv_udp_set_multicast_loop((uv_udp_t*)u->handle,Long_val(o_b));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_multicast_ttl_na(value o_udp, value o_ttl)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  int ttl = Long_val(o_ttl);
  if ( ttl < 1 || ttl > 255 ){
    return VAL_UWT_INT_RESULT_UWT_EINVAL;
  }
  int ret = uv_udp_set_multicast_ttl((uv_udp_t*)u->handle,ttl);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_multicast_interface_na(value o_udp, value o_inter)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  char * iface = NULL;
  if ( o_inter != Val_unit ){
    value s = Field(o_inter,0);
    if ( !uwt_is_safe_string(s) ){
      return VAL_UWT_INT_RESULT_ECHARSET;
    }
    iface = String_val(s);
  }
  int ret = uv_udp_set_multicast_interface((uv_udp_t*)u->handle,iface);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_broadcast_na(value o_udp, value o_b)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  int ret = uv_udp_set_broadcast((uv_udp_t*)u->handle,Long_val(o_b));
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_udp_set_ttl_na(value o_udp, value o_ttl)
{
  HANDLE_NINIT_NA(u,o_udp);
  HANDLE_NO_UNINIT_NA(u);
  int ttl = Long_val(o_ttl);
  int ret;
  if ( ttl < 1 || ttl > 255 ){
    return VAL_UWT_INT_RESULT_UWT_EINVAL;
  }
  ret = uv_udp_set_ttl((uv_udp_t*)u->handle,ttl);
  return (VAL_UWT_UNIT_RESULT(ret));
}

/*
| Data of ( Bytes.t * sockaddr option)
| Partial_data of ( Bytes.t * sockaddr option)
| Empty_from of sockaddr
| Transmission_error of error
*/
#define Data_of 0
#define Partial_data 1
#define Empty_from 2
#define Transmission_error 3
static value
alloc_recv_result(ssize_t nread,
                  const uv_buf_t* buf,
                  const struct sockaddr* addr,
                  unsigned int flags)
{
  CAMLparam0();
  CAMLlocal3(param,msock_addr,bytes_t);
  if ( nread > 0 && buf->base ){
    if ( (size_t)nread > buf->len ){
      param = caml_alloc_small(1,Transmission_error);
      Field(param,0) = VAL_UWT_ERROR_UWT_EFATAL;
    }
    else {
      int tag;
      bytes_t = caml_alloc_string(nread);
      memcpy(String_val(bytes_t),buf->base,nread);
      if ( addr == NULL ){
        msock_addr = Val_unit;
      }
      else {
        param = uwt_alloc_sockaddr((union all_sockaddr *)addr);
        if ( param == Val_unit ){
          msock_addr = Val_unit;
        }
        else {
          msock_addr = caml_alloc_small(1,Some_tag);
          Field(msock_addr,0) = param;
        }
      }
      if ( (flags & UV_UDP_PARTIAL ) != 0 ){
        tag = Partial_data;
      }
      else {
        tag = Data_of;
      }
      param = caml_alloc_small(2,tag);
      Field(param,0) = bytes_t;
      Field(param,1) = msock_addr;
    }
  }
  else if ( nread == 0 ){
    msock_addr = uwt_alloc_sockaddr((union all_sockaddr *)addr);
    if ( msock_addr == Val_unit ){
      param = caml_alloc_small(1,Transmission_error);
      Field(param,0) = VAL_UWT_ERROR_UWT_UNKNOWN;
    }
    else {
      param = caml_alloc_small(1,Empty_from);
      Field(param,0) = msock_addr;
    }
  }
  else {
    param = caml_alloc_small(1,Transmission_error);
    Field(param,0) = Val_uwt_error(nread);
  }
  CAMLreturn(param);
}
#undef Data_of
#undef Partial_data
#undef Empty_from
#undef Transmission_error

static void
uwt_udp_recv_cb(uv_udp_t* handle,
                ssize_t nread,
                const uv_buf_t* buf,
                const struct sockaddr* addr,
                unsigned int flags)
{
  HANDLE_CB_INIT_WITH_CLEAN(handle);
  value exn = Val_unit;
  bool buf_not_cleaned = true;
  struct handle * uh = handle->data;
  if ( uh->close_called == 0 ){
    if ( uh->cb_read == CB_INVALID ){
      DEBUG_PF("callback lost");
    }
    else {
      /* nread == 0 && addr == NULL only means we need to clear
         the buffer */
      if ( nread != 0 || addr != NULL ){
        value p = alloc_recv_result(nread,buf,addr,flags);
        if ( nread > 0 ){
          buf_not_cleaned = false;
          free_uv_buf_t_const(buf,uh->cb_type);
        }
        exn = GET_CB_VAL(uh->cb_read);
        exn = caml_callback_exn(exn,p);
      }
    }
  }
  if ( buf_not_cleaned && buf->base ){
    free_uv_buf_t_const(buf,uh->cb_type);
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_udp_recv_start(value o_udp, value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_udp);
  HANDLE_NINIT(u,o_udp,o_cb);
  value ret;
  if ( u->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
  }
  else {
    int erg = 0;
    uv_udp_t* ux = (uv_udp_t*)u->handle;
    if ( u->can_reuse_cb_read == 1 ){
      u->can_reuse_cb_read = 0;
      u->read_waiting = 0;
      erg = uv_udp_recv_stop(ux);
    }
    if ( erg >= 0 ){
      erg = uv_udp_recv_start(ux,
                              read_start_alloc_cb,
                              uwt_udp_recv_cb);
      if ( erg >= 0 ){
        u->c_read_size = DEF_ALLOC_SIZE;
        gr_root_register(&u->cb_read,o_cb);
        ++u->in_use_cnt;
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_udp_recv_stop(value o_udp, value o_abort)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_udp);
  HANDLE_NINIT(u,o_udp);
  value ret;
  if ( u->cb_read == CB_INVALID ||
       (u->read_waiting == 1 && Long_val(o_abort) == 0 )){
    ret = VAL_UWT_INT_RESULT_UWT_ENOTACTIVE;
  }
  else {
    const int erg = uv_udp_recv_stop((uv_udp_t*)u->handle);
    if ( erg >= 0 ){
      u->can_reuse_cb_read = 0;
      if ( u->in_use_cnt ){
        --u->in_use_cnt;
      }
      gr_root_unregister(&u->cb_read);
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

static void
uwt_udp_recv_own_cb(uv_udp_t* handle,
                    ssize_t nread,
                    const uv_buf_t* buf,
                    const struct sockaddr* addr,
                    unsigned int flags)
{
  HANDLE_CB_INIT_WITH_CLEAN(handle);
  value exn = Val_unit;
  bool buf_not_cleaned = true;
  struct handle * uh = handle->data;
  const int read_ba = uh->use_read_ba;
  if ( uh->close_called == 0 ){
    if (unlikely( uh->cb_read == CB_INVALID ||
                  uh->obuf == CB_INVALID )){
      DEBUG_PF("callback lost");
    }
    else {
      /* nread == 0 && addr == NULL only means we need to clear
         the buffer */
      if ( nread != 0 || addr != NULL ){
        value param;
        uh->read_waiting = 0;
        if ( nread < 0 ){
          param = caml_alloc_small(1,Error_tag);
          Field(param,0) = Val_uwt_error(nread);
        }
        else {
          value triple = Val_unit;
          value sockaddr = Val_unit;
          value is_partial;
          param = Val_unit;
          Begin_roots3(triple,sockaddr,param);
          if ( addr != NULL ){
            param = uwt_alloc_sockaddr((union all_sockaddr *)addr);
            if ( param != Val_unit ){
              sockaddr = caml_alloc_small(1,Some_tag);
              Field(sockaddr,0) = param;
            }
          }
          if ( flags & UV_UDP_PARTIAL ){
            is_partial = Val_long(1);
          }
          else {
            is_partial = Val_long(0);
          }
          if ( nread != 0 && read_ba == 0 ){
            value o = GET_CB_VAL(uh->obuf);
            assert( Tag_val(o) == String_tag );
            size_t len = UMIN(uh->c_read_size,(size_t)nread);
            memcpy(String_val(o) + uh->obuf_offset,
                   buf->base,
                   len);
            buf_not_cleaned = false;
            free_uv_buf_t_const(buf,uh->cb_type);
          }
          triple = caml_alloc_small(3,0);
          Field(triple,0) = Val_long(nread);
          Field(triple,1) = is_partial;
          Field(triple,2) = sockaddr;
          param = caml_alloc_small(1,Ok_tag);
          Field(param,0) = triple;
          End_roots();
        }
        exn = GET_CB_VAL(uh->cb_read);
        uh->can_reuse_cb_read = 1;
        gr_root_unregister(&uh->cb_read);
        gr_root_unregister(&uh->obuf);
        if ( uh->in_use_cnt ){
          uh->in_use_cnt--;
        }
        exn = caml_callback2_exn(*uwt_global_wakeup,exn,param);
        if ( uh->close_called == 0 && uh->can_reuse_cb_read == 1 ){
          uv_udp_recv_stop(handle);
          uh->can_reuse_cb_read = 0;
        }
      }
    }
  }
  if ( read_ba == 0 && buf_not_cleaned && buf->base ){
    free_uv_buf_t_const(buf,uh->cb_type);
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_udp_recv_own(value o_udp,value o_buf,value o_offset,value o_len,value o_cb)
{
  HANDLE_NO_UNINIT_CLOSED_INT_RESULT(o_udp);
  HANDLE_NINIT(u,o_udp,o_buf,o_cb);
  const int ba = Tag_val(o_buf) != String_tag;
  value ret;
  assert( u->cb_type == CB_LWT );
  if ( u->cb_read != CB_INVALID ){
    ret = VAL_UWT_INT_RESULT_UWT_EBUSY;
  }
  else {
    int erg;
    uv_udp_t* ux = (uv_udp_t*)u->handle;
    if ( u->can_reuse_cb_read == 1 ){
      u->can_reuse_cb_read = 0;
      erg = 0;
    }
    else {
      erg = uv_udp_recv_start(ux,alloc_read_own,uwt_udp_recv_own_cb);
    }
    if ( erg >= 0 ){
      size_t len = Long_val(o_len);
      size_t offset = Long_val(o_offset);
      gr_root_register(&u->cb_read,o_cb);
      gr_root_register(&u->obuf,o_buf);
      ++u->in_use_cnt;
      u->c_read_size = len;
      u->use_read_ba = ba;
      u->read_waiting = 1;
      if ( ba == 0 ){
        u->obuf_offset = offset;
      }
      else {
        u->ba_read = Ba_buf_val(o_buf) + offset;
      }
    }
    ret = VAL_UWT_UNIT_RESULT(erg);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_udp_send_queue_size_na(value o_udp)
{
  value ret;
  struct handle * h = Handle_val(o_udp);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    ret = Val_long(0);
  }
  else {
    uv_udp_t* u = (uv_udp_t*)h->handle;
    ret = Val_long((intnat)u->send_queue_size);
  }
  return ret;
}

CAMLprim value
uwt_udp_send_queue_count_na(value o_udp)
{
  value ret;
  struct handle * h = Handle_val(o_udp);
  if ( HANDLE_IS_INVALID_UNINIT(h) ){
    ret = Val_long(0);
  }
  else {
    uv_udp_t* u = (uv_udp_t*)h->handle;
    ret = Val_long((intnat)u->send_queue_count);
  }
  return ret;
}
/* }}} Udp end */

/* {{{ Signal start */
extern int caml_convert_signal_number(int);
extern int caml_rev_convert_signal_number(int);

#ifndef _WIN32
#define uwt_convert_signal_number caml_convert_signal_number
#define uwt_rev_convert_signal_number caml_rev_convert_signal_number
#else
/* Windows: kill will handle SIGTERM, SIGKILL, SIGINT (kill).
   It wil emulate SIGBREAK, SIGHUP, SIGWINCH.
   However, the caml_* functions above can't translate all */
static int
uwt_convert_signal_number(int signum)
{
  switch( signum ){
  case -7: return SIGKILL;
  case -11: return SIGTERM;
  case -6: return SIGINT;
  case -4: return SIGHUP;
  case -50: return SIGBREAK;
  case -51: return SIGWINCH;
  default: return caml_convert_signal_number(signum);
  }
}

static int
uwt_rev_convert_signal_number(int signum)
{
  switch( signum ){
  case SIGKILL: return -7;
  case SIGTERM: return -11;
  case SIGINT: return -6;
  case SIGHUP: return -4;
  case SIGBREAK: return -50;
  case SIGWINCH: return -51;
  default: return caml_rev_convert_signal_number(signum);
  }
}
#endif

static void
signal_cb(uv_signal_t* handle, int signum)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if ( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID ){
    DEBUG_PF("callback lost");
  }
  else {
    const int x = uwt_rev_convert_signal_number(signum);
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,Val_long(x));
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_signal_start(value o_loop,
                 value o_sig,
                 value o_cb)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  uv_signal_t * t;
  GR_ROOT_ENLARGE();
  v = handle_create(UV_SIGNAL,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = v;
  h->close_executed = 0;
  t = (uv_signal_t *)h->handle;
  int erg = uv_signal_init(&l->loop,t);
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
  }
  else {
    int signum = uwt_convert_signal_number(Long_val(o_sig));
    erg = uv_signal_start(t,signal_cb,signum);
    if ( erg < 0 ){
      h->finalize_called = 1;
      handle_finalize_close(h);
    }
    else {
      ++h->in_use_cnt;
      h->initialized = 1;
      gr_root_register(&h->cb_read,o_cb);
      gr_root_register(&h->cb_listen,v);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  CAMLreturn(ret);
}
/* }}} Signal end */

/* {{{ Poll start */
static const int poll_flag_table[3] = {
  UV_READABLE, UV_WRITABLE,
#if HAVE_DECL_UV_DISCONNECT
  UV_DISCONNECT
#else
  4
#endif
};

static void
poll_cb(uv_poll_t* handle, int status, int events)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if (unlikely( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID )){
    DEBUG_PF("callback lost");
  }
  else {
    int tag = Ok_tag;
    value val = Val_unit;
    value param = Val_unit;
    Begin_roots2(val,param);
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
    param = caml_alloc_small(1,tag);
    Field(param,0) = val;
    End_roots();
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,param);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_poll_start(value o_loop,
               value o_sock_or_fd,
               value o_event,
               value o_cb)
{
  INIT_LOOP_WRAP(l,o_loop);
#ifdef _WIN32
  if ( Descr_kind_val(o_sock_or_fd) != KIND_SOCKET ){
    value ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_UWT_EINVAL;
    return ret;
  }
#endif
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  ret = Val_unit;
#ifdef _WIN32
  const uv_os_sock_t sock = Socket_val(o_sock_or_fd);
  const int orig_fd = CRT_fd_val(o_sock_or_fd);
#endif
  const int event = SAFE_CONVERT_FLAG_LIST(o_event,poll_flag_table);
#if !HAVE_DECL_UV_DISCONNECT
  if ( event & 4 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_UWT_EUNAVAIL;
    goto endp;
  }
#endif
  GR_ROOT_ENLARGE();
  v = handle_create(UV_POLL,l);
  struct handle * h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = v;
  h->close_executed = 0;
  uv_poll_t * p = (uv_poll_t*)h->handle;
#ifdef _WIN32
  int erg = uv_poll_init_socket(&l->loop, p, sock);
  h->orig_fd = orig_fd;
#else
  int erg = uv_poll_init(&l->loop, p, Long_val(o_sock_or_fd));
#endif
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
  }
  else {
    erg = uv_poll_start(p,event,poll_cb);
    if ( erg < 0 ){
      h->finalize_called = 1;
      handle_finalize_close(h);
    }
    else {
      h->initialized = 1;
      ++h->in_use_cnt;
      gr_root_register(&h->cb_read,o_cb);
      gr_root_register(&h->cb_listen,v);
    }
  }
  if ( erg < 0 ){
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
#if !HAVE_DECL_UV_DISCONNECT
endp:
#endif
  CAMLreturn(ret);
}
/* }}} Poll End */

/* {{{ Fs_event start */
static void
event_cb(uv_fs_event_t* handle,
         const char* filename,
         int events,
         int status)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if ( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID ){
    DEBUG_PF("callback lost");
  }
  else {
    value param;
    if ( status < 0 ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = Val_uwt_error(status);
    }
    else {
      value list = Val_unit;
      value str = Val_unit;
      value tup = Val_unit;
      Begin_roots3(list,str,tup);
      if ( events & UV_RENAME ){
        tup = caml_alloc_small(2,0);
        Field(tup,0) = Val_long(0);
        Field(tup,1) = list;
        list = tup;
      }
      if ( events & UV_CHANGE ){
        tup = caml_alloc_small(2,0);
        Field(tup,0) = Val_long(1);
        Field(tup,1) = list;
        list = tup;
      }
      str = s_caml_copy_string(filename);
      tup = caml_alloc_small(2,0);
      Field(tup,0) = str;
      Field(tup,1) = list;
      param = caml_alloc_small(1,Ok_tag);
      Field(param,0) = tup;
      End_roots();
    }
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,param);
  }
  HANDLE_CB_RET(ret);
}

static const int fs_event_flags[3] = {
  UV_FS_EVENT_WATCH_ENTRY,
  UV_FS_EVENT_STAT,
  UV_FS_EVENT_RECURSIVE };

CAMLprim value
uwt_fs_event_start(value o_loop,
                   value o_path,
                   value o_flags,
                   value o_cb)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam3(o_loop,o_path,o_cb);
  CAMLlocal2(ret,v);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_flags,fs_event_flags);
  if ( !uwt_is_safe_string(o_path) ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_ECHARSET;
  }
  else {
    GR_ROOT_ENLARGE();
    v = handle_create(UV_FS_EVENT,l);
    struct handle * h = Handle_val(v);
    h->close_executed = 1;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = v;
    h->close_executed = 0;
    uv_fs_event_t * f = (uv_fs_event_t*)h->handle;
    int erg = uv_fs_event_init(&l->loop,f);
    if ( erg < 0 ){
      free_mem_uv_handle_t(h);
      free_struct_handle(h);
    }
    else {
      erg = uv_fs_event_start(f,event_cb,String_val(o_path),flags);
      if ( erg < 0 ){
        h->finalize_called = 1;
        handle_finalize_close(h);
      }
      else {
        ++h->in_use_cnt;
        h->initialized = 1;
        gr_root_register(&h->cb_read,o_cb);
        gr_root_register(&h->cb_listen,v);
      }
    }
    if ( erg < 0 ){
      Field(v,1) = 0;
      Tag_val(ret) = Error_tag;
      Field(ret,0) = Val_uwt_error(erg);
    }
  }
  CAMLreturn(ret);
}
/* }}} Fs_event end */

/* {{{ Fs_poll start */
static void
fs_poll_cb(uv_fs_poll_t* handle,
           int status,
           const uv_stat_t* prev,
           const uv_stat_t* curr)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if ( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID ){
    DEBUG_PF("callback lost");
  }
  else {
    value param;
    if ( status < 0 ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = Val_uwt_error(status);
    }
    else if ( prev == NULL || curr == NULL ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = VAL_UWT_ERROR_UWT_EFATAL;
    }
    else {
      value s2 = Val_unit;
      value tup = Val_unit;
      value s1 = uv_stat_to_value(prev);
      Begin_roots3(tup,s1,s2);
      s2 = uv_stat_to_value(curr);
      tup = caml_alloc_small(2,0);
      Field(tup,0) = s1;
      Field(tup,1) = s2;
      param = caml_alloc_small(1,Ok_tag);
      Field(param,0) = tup;
      End_roots();
    }
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,param);
  }
  HANDLE_CB_RET(ret);
}

CAMLprim value
uwt_fs_poll_start(value o_loop,
                  value o_path,
                  value o_interval,
                  value o_cb)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam3(o_loop,o_path,o_cb);
  CAMLlocal2(ret,v);
  if ( !uwt_is_safe_string(o_path) ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_ECHARSET;
  }
  else {
    uv_fs_poll_t * f;
    int erg;
    GR_ROOT_ENLARGE();
    v = handle_create(UV_FS_POLL,l);
    struct handle * h = Handle_val(v);
    h->close_executed = 1;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = v;
    h->close_executed = 0;
    f = (uv_fs_poll_t*)h->handle;
    erg = uv_fs_poll_init(&l->loop,f);
    if ( erg < 0 ){
      free_mem_uv_handle_t(h);
      free_struct_handle(h);
    }
    else {
      erg = uv_fs_poll_start(f,
                             fs_poll_cb,
                             String_val(o_path),
                             Long_val(o_interval));
      if ( erg < 0 ){
        h->finalize_called = 1;
        handle_finalize_close(h);
      }
      else {
        ++h->in_use_cnt;
        h->initialized = 1;
        gr_root_register(&h->cb_read,o_cb);
        gr_root_register(&h->cb_listen,v);
      }
    }
    if ( erg < 0 ){
      Field(v,1) = 0;
      Tag_val(ret) = Error_tag;
      Field(ret,0) = Val_uwt_error(erg);
    }
  }
  CAMLreturn(ret);
}
/* }}} Fs_poll end */

/* {{{ Async start */
static void
uwt_async_cb(uv_async_t* handle)
{
  HANDLE_CB_INIT(handle);
  value exn = Val_unit;
  struct handle * wp = handle->data;
  if (unlikely( wp->cb_read == CB_INVALID || wp->cb_listen == CB_INVALID )){
    DEBUG_PF("cb lost");
  }
  else {
    value t = GET_CB_VAL(wp->cb_listen);
    exn = GET_CB_VAL(wp->cb_read);
    exn = caml_callback_exn(exn,t);
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_async_create(value o_loop, value o_cb)
{
  INIT_LOOP_WRAP(l,o_loop);
  CAMLparam2(o_loop,o_cb);
  CAMLlocal2(ret,v);
  GR_ROOT_ENLARGE();
  v = handle_create(UV_ASYNC,l);
  struct handle *h = Handle_val(v);
  h->close_executed = 1;
  ret = caml_alloc_small(1,Ok_tag);
  h->close_executed = 0;
  Field(ret,0) = v;
  uv_async_t * a = (uv_async_t *)h->handle;
  const int erg = uv_async_init(&l->loop,a,uwt_async_cb);
  if ( erg < 0 ){
    free_mem_uv_handle_t(h);
    free_struct_handle(h);
    Field(v,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
    Tag_val(ret) = Error_tag;
  }
  else {
    gr_root_register(&h->cb_read,o_cb);
    gr_root_register(&h->cb_listen,v);
    uv_unref((uv_handle_t*)a);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_async_start_na(value o_async)
{
  HANDLE_NINIT_NA(a,o_async);
  uv_ref(a->handle);
  return Val_unit;
}

CAMLprim value
uwt_async_stop_na(value o_async)
{
  HANDLE_NINIT_NA(a,o_async);
  uv_unref(a->handle);
  return Val_unit;
}

CAMLprim value
uwt_async_send_na(value o_async)
{
  HANDLE_NINIT_NA(a,o_async);
  uv_async_send((uv_async_t*)a->handle);
  return Val_unit;
}
/* }}} Async end */

/* {{{ Misc start */
CAMLprim value
uwt_guess_handle_na(value o_fd)
{
  switch ( uv_guess_handle(FD_VAL(o_fd)) ){
  case UV_FILE: return (Val_long(0));
  case UV_TTY: return (Val_long(1));
  case UV_NAMED_PIPE: return (Val_long(2));
  case UV_TCP: return (Val_long(3));
  case UV_UDP: return (Val_long(4));
  default: /* fall */
  case UV_UNKNOWN_HANDLE:
    return (Val_long(5));
  }
}

CAMLprim value
uwt_version_na(value unit)
{
  (void)unit;
  return (Val_long(uv_version()));
}

CAMLprim value
uwt_version_string(value unit)
{
  (void)unit;
  return (s_caml_copy_string(uv_version_string()));
}

CAMLprim value
uwt_resident_set_memory(value unit)
{
  (void)unit;
  value res;
  size_t rss;
  int r = uv_resident_set_memory(&rss);
  if ( r >= 0 ){
    value x = caml_copy_int64(rss);
    Begin_roots1(x);
    res = caml_alloc_small(1,Ok_tag);
    Field(res,0) = x;
    End_roots();
  }
  else {
    res = caml_alloc_small(1,Error_tag);
    Field(res,0) = Val_uwt_error(r);
  }
  return res;
}

CAMLprim value
uwt_uptime(value unit)
{
 value res;
 double up;
 int r = uv_uptime(&up);
 (void)unit;
 if ( r >= 0 ){
   value x = caml_copy_double(up);
   Begin_roots1(x);
   res = caml_alloc_small(1,Ok_tag);
   Field(res,0) = x;
   End_roots();
 }
 else {
   res = caml_alloc_small(1,Error_tag);
   Field(res,0) = Val_uwt_error(r);
 }
 return res;
}

#define IP_CONV(name,field)                                             \
  CAMLprim value                                                        \
  uwt_ ## name ##  _addr(value o_str,value o_port)                      \
  {                                                                     \
    value ret;                                                          \
    struct sockaddr_i ## field addr;                                    \
    if ( !uwt_is_safe_string(o_str) ){                                  \
      ret = caml_alloc_small(1,Error_tag);                              \
      Field(ret,0) = VAL_UWT_ERROR_ECHARSET;                            \
    }                                                                   \
    else {                                                              \
      int r = uv_ ## name ## _addr(String_val(o_str),                   \
                                   Long_val(o_port),&addr);             \
      if ( r < 0 ){                                                     \
        ret = caml_alloc_small(1,Error_tag);                            \
        Field(ret,0) = Val_uwt_error(r);                                \
      }                                                                 \
      else {                                                            \
        value so = uwt_alloc_sockaddr((union all_sockaddr *)&addr);     \
        if ( so == Val_unit ){                                          \
          ret = caml_alloc_small(1,Error_tag);                          \
          Field(ret,0) = VAL_UWT_ERROR_UWT_UNKNOWN;                     \
        }                                                               \
        else {                                                          \
          Begin_roots1(so);                                             \
          ret = caml_alloc_small(1,Ok_tag);                             \
          Field(ret,0) = so;                                            \
          End_roots();                                                  \
        }                                                               \
      }                                                                 \
    }                                                                   \
    return ret;                                                         \
  }                                                                     \
  CAMLprim value                                                        \
  uwt_ ## name ## _name(value o_sock)                                   \
  {                                                                     \
    int r;                                                              \
    value ret;                                                          \
    size_t s_size = 128;                                                \
    char dst[s_size];                                                   \
    union all_sockaddr x;                                               \
    if ( !uwt_get_sockaddr(o_sock,&x) ){                                \
      ret = caml_alloc_small(1,Error_tag);                              \
      Field(ret,0) = VAL_UWT_ERROR_UNKNOWN;                             \
      return ret;                                                       \
    }                                                                   \
    r = uv_ ## name ## _name(&x.i ## field ,dst,s_size);                \
    if ( r < 0 ){                                                       \
      ret = caml_alloc_small(1,Error_tag);                              \
      Field(ret,0) = Val_uwt_error(r);                                  \
    }                                                                   \
    else {                                                              \
      value os;                                                         \
      s_size = strnlen(dst,s_size);                                     \
      os = caml_alloc_string(s_size);                                   \
      memcpy(String_val(os),dst,s_size);                                \
      Begin_roots1(os);                                                 \
      ret = caml_alloc_small(1,Ok_tag);                                 \
      Field(ret,0) = os;                                                \
      End_roots();                                                      \
    }                                                                   \
    return ret;                                                         \
  }                                                                     \

IP_CONV(ip4,n)
IP_CONV(ip6,n6)

CAMLprim value
uwt_getrusage(value unit)
{
  CAMLparam0();
  CAMLlocal2(ar,tup);
  uv_rusage_t u;
  int r = uv_getrusage(&u);
  int tag;
  (void) unit;
  if ( r < 0 ){
    ar = Val_uwt_error(r);
    tag = Error_tag;
  }
  else {
    tag = Ok_tag;
    ar = caml_alloc(16,0);
    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(u.ru_utime.tv_sec);
    Field(tup,1) = Val_long(u.ru_utime.tv_usec);
    Store_field(ar,0,tup);

    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(u.ru_stime.tv_sec);
    Field(tup,1) = Val_long(u.ru_stime.tv_usec);
    Store_field(ar,1,tup);

    unsigned int i = 2;

#define X(y)                                    \
    tup = caml_copy_int64(u.ru_ ## y);          \
    Store_field(ar,i,tup);                      \
    i++                                         \

    X(maxrss);
    X(ixrss);
    X(idrss);
    X(isrss);
    X(minflt);
    X(majflt);
    X(nswap);
    X(inblock);
    X(oublock);
    X(msgsnd);
    X(msgrcv);
    X(nsignals);
    X(nvcsw);
    X(nivcsw);
#undef X
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar;
  CAMLreturn(res);
}

CAMLprim value
uwt_cpu_info(value unit)
{
  CAMLparam0();
  CAMLlocal4(ar_out,ar_in,tup,tmp);
  uv_cpu_info_t * cpu_infos;
  int n_cpu;
  const int r = uv_cpu_info(&cpu_infos,&n_cpu);
  int tag;
  (void) unit;
  if ( r < 0 || n_cpu < 0 ){
    ar_out = Val_uwt_error(r);
    tag = Error_tag;
  }
  else {
    tag = Ok_tag;
    ar_out = caml_alloc(n_cpu,0);
    int i;
    int j;
    for ( i = 0; i < n_cpu; ++i ){
      uv_cpu_info_t * c = &cpu_infos[i];
      tup = caml_alloc(3,0);
      tmp = s_caml_copy_string(c->model);
      Store_field(tup,0,tmp);
      Field(tup,1) = Val_long(c->speed);

      j = 0;
      ar_in = caml_alloc(5,0);
#define X(y)                                    \
      tmp = caml_copy_int64(c->cpu_times.y);    \
      Store_field(ar_in,j,tmp);                 \
      j++

      X(user);
      X(nice);
      X(sys);
      X(idle);
      X(irq);
#undef X
      Store_field(tup,2,ar_in);
      Store_field(ar_out,i,tup);
    }
    uv_free_cpu_info(cpu_infos,n_cpu);
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar_out;
  CAMLreturn(res);
}

CAMLprim value
uwt_interface_addresses(value unit)
{
  CAMLparam0();
  CAMLlocal3(ar_out,ar_in,tmp);
  uv_interface_address_t* addresses;
  int n_addresses;
  const int r = uv_interface_addresses(&addresses,&n_addresses);
  int tag;
  (void) unit;
  if ( r < 0 || n_addresses < 0 ){
    ar_out = Val_uwt_error(r);
    tag = Error_tag;
  }
  else {
    tag = Ok_tag;
    ar_out = caml_alloc(n_addresses,0);
    int i;
    for ( i = 0; i < n_addresses; ++i ){
      uv_interface_address_t *  c = &addresses[i];
      ar_in = caml_alloc(5,0);
      tmp = s_caml_copy_string(c->name);
      Store_field(ar_in,0,tmp);

      tmp = caml_alloc_string(6);
      memcpy(String_val(tmp),c->phys_addr,6);
      Store_field(ar_in,1,tmp);
      Field(ar_in,2) = Val_long( c->is_internal != 0 );

      tmp = uwt_alloc_sockaddr((union all_sockaddr *)&c->address);
      if ( tmp != Val_unit ){
        value x = caml_alloc_small(1,0);
        Field(x,0) = tmp;
        tmp = x;
      }
      Store_field(ar_in,3,tmp);

      tmp = uwt_alloc_sockaddr((union all_sockaddr *)&c->netmask);
      if ( tmp != Val_unit ){
        value x = caml_alloc_small(1,0);
        Field(x,0) = tmp;
        tmp = x;
      }
      Store_field(ar_in,4,tmp);

      Store_field(ar_out,i,ar_in);
    }
    uv_free_interface_addresses(addresses,n_addresses);
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar_out;
  CAMLreturn(res);
}

CAMLprim value
uwt_load_avg(value unit)
{
  CAMLparam0();
  CAMLlocal3(tmp1,tmp2,tmp3);
  double avg[3];
  (void) unit;
  uv_loadavg(avg);

  tmp1 = caml_copy_double(avg[0]);
  tmp2 = caml_copy_double(avg[1]);
  tmp3 = caml_copy_double(avg[2]);

  value ret = caml_alloc_small(3,0);
  Field(ret,0) = tmp1;
  Field(ret,1) = tmp2;
  Field(ret,2) = tmp3;

  CAMLreturn(ret);
}

CAMLprim value
uwt_get_total_memory(value unit)
{
  (void)unit;
  return (caml_copy_int64(uv_get_total_memory()));
}

CAMLprim value
uwt_hrtime(value unit)
{
  (void)unit;
  return (caml_copy_int64(uv_hrtime()));
}

typedef int(*os_dir)(char *, size_t *);
static value
uwt_os_dir(os_dir fdir_func)
{
  CAMLparam0();
  CAMLlocal1(p);
  value ret;
  char buffer[ALLOCA_PATH_LEN];
  size_t size = ALLOCA_PATH_LEN;
  int tag;
  const int r = fdir_func(buffer,&size);
  if ( r == 0 && size > 0 && size <= ALLOCA_PATH_LEN ){
    p = caml_alloc_string(size);
    memcpy(String_val(p), buffer, size);
    tag = Ok_tag;
  }
  else {
    p = Val_uwt_error(r);
    tag = Error_tag;
  }
  ret = caml_alloc_small(1,tag);
  Field(ret,0) = p;
  CAMLreturn(ret);
}

CAMLprim value
uwt_exepath(value unit)
{
  (void)unit;
  return (uwt_os_dir(uv_exepath));
}

CAMLprim value
uwt_cwd(value unit)
{
  (void)unit;
  return (uwt_os_dir(uv_cwd));
}

CAMLprim value
uwt_os_homedir(value unit)
{
  (void)unit;
#if HAVE_DECL_UV_OS_HOMEDIR
  return (uwt_os_dir(uv_os_homedir));
#else
  return (result_eunavail());
#endif
}

CAMLprim value
uwt_os_tmpdir(value unit)
{
  (void)unit;
#if HAVE_DECL_UV_OS_TMPDIR
  return (uwt_os_dir(uv_os_tmpdir));
#else
  return (result_eunavail());
#endif
}

CAMLprim value
uwt_chdir(value o_string)
{
  int r;
  if ( !uwt_is_safe_string(o_string) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  char * s = strdup(String_val(o_string));
  if ( s == NULL ){
    return VAL_UWT_INT_RESULT_ENOMEM;
  }
  caml_enter_blocking_section();
  r = uv_chdir(s);
  caml_leave_blocking_section();
  free(s);

  return (VAL_UWT_INT_RESULT(r));
}

CAMLprim value
uwt_get_passwd(value unit)
{
  (void)unit;
#if !HAVE_DECL_UV_OS_GET_PASSWD
  return (result_eunavail());
#else
  uv_passwd_t pwd;
  value eret;
  int i;
  /* Test the normal case */
  i = uv_os_get_passwd(&pwd);
  if ( i != 0 ){
  error_ret:
    eret = caml_alloc_small(1,Error_tag);
    Field(eret,0) = Val_uwt_error(i);
    return eret;
  }
  else {
#define BSIZE 8192
    value name = Val_unit, empty = Val_unit;
    value dir = Val_unit, shell = Val_unit;
    value res = Val_unit;

    char buf[3][BSIZE];
    const char * const to_copy[3] = {pwd.shell,pwd.homedir,pwd.username};
    for ( i = 0 ; i < 3 ; i++ ){
      const char * const src = to_copy[i];
      char *dst = buf[i];
      if ( src == NULL ){
        dst[0] = 0 ;
      }
      else {
        const size_t l = strlen(src);
        if ( l >= BSIZE ){
          uv_os_free_passwd(&pwd);
          i = UV_UWT_EFATAL;
          goto error_ret;
        }
        memcpy(dst,src,l+1);
      }
    }
#undef BSIZE
    long uid = pwd.uid;
    long gid = pwd.gid;
    uv_os_free_passwd(&pwd);

    Begin_roots4 (name, empty, dir, shell);

    shell = caml_copy_string(buf[0]);
    dir = caml_copy_string(buf[1]);
    name = caml_copy_string(buf[2]);

    empty = caml_copy_string("");
    res = caml_alloc_small(7, 0);
    Field(res, 0) = name;
    Field(res, 1) = empty;
    Field(res, 2) = Val_long(uid);
    Field(res, 3) = Val_long(gid);
    Field(res, 4) = empty;
    Field(res, 5) = dir;
    Field(res, 6) = shell;
    shell = res;
    name = caml_alloc_small(1,Ok_tag);
    Field(name,0) = shell;
    End_roots();
    return name;
  }
#endif
}

static char ** uv_setup_args_ret = NULL;
static char ** dummy_argv = NULL;

static int
uwt_setup_args(value sys_argv)
{
  if ( sys_argv == Atom(0) || Wosize_val(sys_argv) == 0 ){
    return UV_UWT_UNKNOWN;
  }
  else {
    const int argc = Wosize_val(sys_argv);
    int size = 0;
    char ** argv;
    char *p;
    char * memb;
    int i;

    argv = malloc( (argc+1) * sizeof (char**) );
    if ( argv == NULL ) {
      return UV_ENOMEM;
    }

    for ( i = 0; i < argc; i++ ) {
      value s = Field(sys_argv,i);
      if ( !uwt_is_safe_string(s) ){
        return UV_ECHARSET;
      }
      size += strlen(String_val(s)) + 1;
    }

    memb = malloc(size);
    if ( memb == NULL ){
      free(argv);
      return UV_ENOMEM;
    }

    /* libuv wants the argv arguments in this order in memory.
       Otherwise an assert failure is triggered,.... */
    p = memb;
    for ( i = 0; i < argc ; i++ ) {
      size_t n = strlen(String_val(Field(sys_argv,i)));
      argv[i] = p;
      memcpy(p,String_val(Field(sys_argv,i)),n + 1);
      p = p + n + 1;
    }
    argv[argc] = NULL;

    uv_setup_args_ret = uv_setup_args(argc,argv);
    dummy_argv = argv;

    return 0;
  }
}

CAMLprim value
uwt_get_process_title(value sys_argv)
{
  CAMLparam0();
  CAMLlocal1(p);
#define BSIZE 16384
  value ret;
  char buffer[BSIZE];
  int tag;
  int r;
  if ( uv_setup_args_ret == NULL ){
    r = uwt_setup_args(sys_argv);
    if ( r ){
      goto error_end;
    }
  }
  r = uv_get_process_title(buffer,BSIZE - 1);
  if ( r == 0 ){
    buffer[BSIZE-1] = '\0';
    p = caml_copy_string(buffer);
    tag = Ok_tag;
  }
  else {
  error_end:
    p = Val_uwt_error(r);
    tag = Error_tag;
  }
  ret = caml_alloc_small(1,tag);
  Field(ret,0) = p;
  CAMLreturn(ret);
#undef BSIZE
}

CAMLprim value
uwt_set_process_title_na(value sys_argv, value o_str)
{
  int r;
  if ( !uwt_is_safe_string(o_str) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  if ( uv_setup_args_ret == NULL ){
    r = uwt_setup_args(sys_argv);
    if ( r ){
      return (VAL_UWT_INT_RESULT(r));
    }
  }
  r = uv_set_process_title(String_val(o_str));
  return (VAL_UWT_INT_RESULT(r));
}


#ifdef _WIN32
char *
uwt_utf16_to_utf8(const WCHAR* utf16_buffer, int * error)
{
  char * utf8_buffer;
  int utf8_len;
  if ( utf16_buffer == NULL ){
    *error = UV_EINVAL;
    return NULL;
  }
  utf8_len = WideCharToMultiByte(CP_UTF8,0,utf16_buffer,-1,NULL,0,NULL,NULL);
  if ( utf8_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    return NULL;
  }
  utf8_buffer = malloc(utf8_len+1);
  if ( utf8_buffer == NULL ){
    *error = UV_ENOMEM;
    return NULL;
  }
  utf8_len = WideCharToMultiByte(CP_UTF8,0,utf16_buffer,-1,utf8_buffer,utf8_len,
                                 NULL,NULL);
  if ( utf8_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    free(utf8_buffer);
    return NULL;
  }
  return utf8_buffer;
}

WCHAR*
uwt_utf8_to_utf16(const char* utf8_buffer,int *error){
  WCHAR * utf16_buffer;
  int utf16_len;
  if ( utf8_buffer == NULL ){
    *error = UV_EINVAL;
    return NULL;
  }
  utf16_len = MultiByteToWideChar(CP_UTF8,0,utf8_buffer,-1,NULL,0);
  if ( utf16_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    return NULL;
  }
  utf16_buffer = malloc(sizeof(WCHAR) * utf16_len);
  if ( utf16_buffer == NULL ){
    *error = UV_ENOMEM;
    return NULL;
  }
  utf16_len = MultiByteToWideChar(CP_UTF8,0,utf8_buffer,-1,utf16_buffer,
                                  utf16_len);
  if ( utf16_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    free(utf16_buffer);
    return NULL;
  }
  return utf16_buffer;
}

CAMLprim value uwt_win_version(value unit);
CAMLprim value
uwt_win_version(value unit)
{
  OSVERSIONINFOEXW os;
  value ret;
  int error;
  (void) unit;
  ZeroMemory(&os, sizeof(os));
  os.dwOSVersionInfoSize = sizeof(os);
  if ( GetVersionExW((LPOSVERSIONINFOW)&os) == 0 ){
    error = uwt_translate_sys_error(GetLastError());
    goto error_end;
  }
  char * szCSDVersion = NULL;
  if ( os.szCSDVersion != NULL ){
    szCSDVersion = uwt_utf16_to_utf8(os.szCSDVersion,&error);
    if ( szCSDVersion == NULL ){
      goto error_end;
    }
  }
  value s = s_caml_copy_string(szCSDVersion);
  value cont = Val_unit;
  Begin_roots2(s,cont);
    cont = caml_alloc_small(5,0);
    Field(cont,0) = Val_long(os.dwMajorVersion);
    Field(cont,1) = Val_long(os.dwMinorVersion);
    Field(cont,2) = Val_long(os.dwBuildNumber);
    Field(cont,3) = Val_long(os.dwPlatformId);
    Field(cont,4) = s;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = cont;
  End_roots();
  return ret;
 error_end:
  ret = caml_alloc_small(1,Error_tag);
  Field(ret,0) = Val_uwt_error(error);
  return ret;
}
#endif /* _WIN32 */

#if HAVE_DECL_UV_PRINT_ALL_HANDLES || HAVE_DECL_UV_PRINT_ACTIVE_HANDLES
typedef void(*print_handles)(uv_loop_t*, FILE*);
static value
uwt_print_handles(value o_loop, value o_fd, print_handles phandles)
{
  INIT_LOOP_INT_RESULT(l,o_loop);
  int fd ;
  FILE * fp;
#ifdef _WIN32
  fd = _dup(FD_VAL(o_fd));
#else
  fd = dup(FD_VAL(o_fd));
#endif
  if ( fd == -1 ){
    return VAL_UWT_UNIT_RESULT(-errno);
  }
#ifdef _WIN32
  fp = _fdopen(fd,"w");
#else
  fp = fdopen(fd,"w");
#endif
  if ( fp == NULL ){
    return VAL_UWT_UNIT_RESULT(-errno);
  }
  phandles(&l->loop,fp);
  if ( fclose(fp) ){
    return(VAL_UWT_UNIT_RESULT(-errno));
  }
  return Val_unit;
}
#endif

CAMLprim value
uwt_print_all_handles(value a, value b)
{
#if HAVE_DECL_UV_PRINT_ALL_HANDLES
  return uwt_print_handles(a,b,uv_print_all_handles);
#else
  return VAL_UWT_INT_RESULT_UWT_EUNAVAIL;
#endif
}

CAMLprim value
uwt_print_active_handles(value a, value b)
{
#if HAVE_DECL_UV_PRINT_ACTIVE_HANDLES
  return uwt_print_handles(a,b,uv_print_active_handles);
#else
  return VAL_UWT_INT_RESULT_UWT_EUNAVAIL;
#endif
}

/* }}} Misc end */

/* {{{ DNS start */
static value
cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for ( i = 0; i < size; i++ ){
    if ( n == tbl[i] ){
      return Val_long(i);
    }
  }
  return Val_long(deflt);
}

/* must be kept in sync with getaddrinfo.c and socket.c from
   the ocaml distribution.
*/
static int
socket_domain_table[] = {
  PF_UNIX, PF_INET,
#if defined(PF_INET6)
  PF_INET6
#elif defined(PF_UNDEF)
  PF_UNDEF
#else
  0
#endif
};

static int
socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

/* copied from getaddrinfo.c, modified for vaddr */
static value
convert_addrinfo(struct addrinfo * a)
{
  CAMLparam0();
  CAMLlocal3(vres,vaddr,vcanonname);

  vaddr = uwt_alloc_sockaddr((union all_sockaddr *)a->ai_addr);
  if ( vaddr == Val_unit ){
    vres = Val_unit;
    goto endp;
  }
  vcanonname = s_caml_copy_string(a->ai_canonname);
  vres = caml_alloc_small(5, 0);
  Field(vres, 0) = cst_to_constr(a->ai_family, socket_domain_table, 3, 0);
  Field(vres, 1) = cst_to_constr(a->ai_socktype, socket_type_table, 4, 0);
  Field(vres, 2) = Val_long(a->ai_protocol);
  Field(vres, 3) = vaddr;
  Field(vres, 4) = vcanonname;
endp:
  CAMLreturn(vres);
}

static value
ret_addrinfo_list(uv_req_t * rdd)
{
  value ifo;
  struct req * wp = rdd->data;
  const int status = wp->c_param;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
    struct addrinfo* info = wp->c.p1;
    struct addrinfo * r;
    bool error_found = false;
    value e = Val_long(0);
    value v = Val_long(0);
    value vres = Val_long(0);
    value list_head = Val_long(0);
    Begin_roots4(e,v,vres,list_head);
    for ( r = info; r != NULL; r = r->ai_next ){
      e = convert_addrinfo(r);
      if ( e == Val_unit ){
        error_found = true;
        continue;
      }
      v = caml_alloc_small(2, 0);
      Field(v, 0) = e;
      Field(v, 1) = Val_long(0);
      if ( vres != Val_long(0) ){
        Store_field(vres,1,v);
      }
      else {
        list_head = v;
      }
      vres = v;
    }
    if ( list_head == Val_long(0) && error_found == true ) {
      ifo = caml_alloc_small(1,Error_tag);
      Field(ifo,0) = VAL_UWT_ERROR_UWT_UNKNOWN;
    }
    else {
      ifo = caml_alloc_small(1,Ok_tag);
      Field(ifo,0) = list_head;
    }
    End_roots();
  }
  return ifo;
}

static void
clean_addrinfo(uv_req_t * r)
{
  struct req * wp = r->data;
  if ( wp && wp->c.p1 != NULL ){
    uv_freeaddrinfo(wp->c.p1);
    wp->c.p1 = NULL;
  }
}

static void
cb_getaddrinfo (uv_getaddrinfo_t* req,
                int status,
                struct addrinfo* res)
{
  struct req * r = req->data;
  if ( r ){
    r->c_param = status;
    r->c.p1 = res;
  }
  universal_callback((void*)req);
}

#define RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req)                     \
  if (unlikely( loop == NULL || req == NULL || loop->init_called == 0 || \
                req->req == NULL || req->in_use == 1 )){                 \
    return VAL_UWT_INT_RESULT_UWT_EFATAL;                                \
  }

CAMLprim value
uwt_getaddrinfo_native(value o_node,
                       value o_serv,
                       value o_opts,
                       value o_loop,
                       value o_req,
                       value o_cb)
{
  struct loop * loop = Loop_val(o_loop);
  struct req * req = Req_val(o_req);
  RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req);
  if ( !uwt_is_safe_string(o_node) || !uwt_is_safe_string(o_serv) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  CAMLparam5(o_node,o_serv,o_opts,o_req,o_cb);
  int erg;
  char * node;
  char * serv;
  struct addrinfo hints;
  req->cb_type = loop->loop_type;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(o_opts); o_opts = Field(o_opts, 1) ){
    value v = Field(o_opts, 0);
    if ( Is_block(v) ){
      const unsigned int i = Long_val(Field(v, 0));
      switch ( Tag_val(v) ){
      case 0: /* AI_FAMILY of socket_domain */
        if ( i >= AR_SIZE(socket_domain_table) ){
          erg = UV_UWT_EINVAL;
          goto einval;
        }
        hints.ai_family = socket_domain_table[i];
        break;
      case 1: /* AI_SOCKTYPE of socket_type */
        if ( i >= AR_SIZE(socket_type_table) ){
          erg = UV_UWT_EINVAL;
          goto einval;
        }
        hints.ai_socktype = socket_type_table[i];
        break;
      case 2: /* AI_PROTOCOL of int */
        hints.ai_protocol = i;
        break;
      }
    }
    else {
      switch ( Long_val(v) ){
      case 0: /* AI_NUMERICHOST */
        hints.ai_flags |= AI_NUMERICHOST; break;
      case 1: /* AI_CANONNAME */
        hints.ai_flags |= AI_CANONNAME; break;
      case 2: /* AI_PASSIVE */
        hints.ai_flags |= AI_PASSIVE; break;
      }
    }
  }
  GR_ROOT_ENLARGE();

  if ( caml_string_length(o_node) == 0 ){
    node = NULL;
  }
  else {
    if ( !uwt_is_safe_string(o_node) ){
      erg = UV_ECHARSET;
      goto einval;
    }
    node = String_val(o_node);
  }
  if ( caml_string_length(o_serv) == 0 ){
    serv = NULL;
  }
  else {
    if ( !uwt_is_safe_string(o_serv) ){
      erg = UV_ECHARSET;
      goto einval;
    }
    serv = String_val(o_serv);
  }

  erg = uv_getaddrinfo(&loop->loop,
                       (uv_getaddrinfo_t*)req->req,
                       cb_getaddrinfo,
                       node,
                       serv,
                       &hints);
 einval:
  if ( erg < 0 ){
    Field(o_req,1) = 0;
    req_free(req);
  }
  else {
    gr_root_register(&req->cb,o_cb);
    req->in_use = 1;
    req->c_cb = ret_addrinfo_list;
    req->clean_cb = clean_addrinfo;
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}
BYTE_WRAP6(uwt_getaddrinfo)

static const int
getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

static void
cb_getnameinfo(uv_getnameinfo_t* req, int status,
               const char* hostname, const char* service)
{
  struct req * r = req->data;
  if ( r ){ /* does not work synchronously */
    r->c_param = status;
    r->c.p1 = (void *)hostname;
    r->c.p2 = (void *)service;
  }
  universal_callback((void*)req);
}

static value
ret_getnameinfo(uv_req_t * rdd)
{
  value ifo;
  struct req * wp = rdd->data;
  const int status = wp->c_param;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
    value tmp1 = Val_unit;
    value tmp2 = Val_unit;
    ifo = Val_unit;
    Begin_roots3(tmp1,tmp2,ifo);
    tmp1 = s_caml_copy_string(wp->c.p1);
    tmp2 = s_caml_copy_string(wp->c.p2);
    ifo = caml_alloc_small(2,0);
    Field(ifo,0) = tmp1;
    Field(ifo,1) = tmp2;
    tmp1 = caml_alloc_small(1,Ok_tag);
    Field(tmp1,0) = ifo;
    ifo = tmp1;
    End_roots();
  }
  return ifo;
}

CAMLprim value
uwt_getnameinfo(value o_sockaddr, value o_list, value o_loop,
                value o_req, value o_cb)
{
  union all_sockaddr addr;
  if ( !uwt_get_sockaddr(o_sockaddr,&addr) ){
    return VAL_UWT_INT_RESULT_UWT_UNKNOWN;
  }
  struct loop * loop = Loop_val(o_loop);
  struct req * req = Req_val(o_req);
  RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req);
  CAMLparam3(o_sockaddr,o_cb,o_req);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_list, getnameinfo_flag_table);
  GR_ROOT_ENLARGE();
  const int erg = uv_getnameinfo(&loop->loop,
                                 (uv_getnameinfo_t*)req->req,
                                 cb_getnameinfo,
                                 /* copied to internal storage by libuv */
                                 &addr.addr,
                                 flags);
  value ret;
  if ( erg < 0 ){
    ret = Val_uwt_int_result(erg);
    Field(o_req,1) = 0;
    req_free(req);
  }
  else {
    gr_root_register(&req->cb,o_cb);
    req->in_use = 1;
    req->c_cb = ret_getnameinfo;
    ret = Val_unit;
  }
  CAMLreturn(ret);
}
/* }}} DNS end */

/* {{{ Process start */
CAMLprim value
uwt_disable_stdio_inheritance_na(value unit){
  (void) unit;
  uv_disable_stdio_inheritance();
  return Val_unit;
}

static char **
caml_string_array_to_c_array(value p, int * e)
{
  const size_t len = Wosize_val(p);
  if ( len == 0 ){
    *e = UV_EINVAL;
    return NULL;
  }
  char ** env = malloc( (len + 1) * sizeof(char*) );
  if ( !env ){
    *e = UV_ENOMEM;
    return NULL;
  }
  size_t i;
  for ( i = 0; i < len; i++ ){
    value s = Field(p,i);
    if ( !uwt_is_safe_string(s) ){
      *e = UV_ECHARSET;
      free(env);
      return NULL;
    }
    env[i] = String_val(s);
  }
  env[len] = NULL;
  *e = 0;
  return env;
}

static struct handle *
get_handle(value o_s)
{
  if ( !Is_block(o_s) || Wosize_val(o_s) != 4 ){
    return NULL;
  }
  struct handle * s = Handle_val(o_s);
  if ( HANDLE_IS_INVALID(s) ){
    return NULL;
  }
  return s;
}

static void
spawn_exit_cb(uv_process_t*t, int64_t exit_status, int term_signal)
{
  HANDLE_CB_INIT(t);
  value exn = Val_unit;
  struct handle * h = t->data;
  if ( h ){
    if ( h->cb_read != CB_INVALID && h->cb_listen != CB_INVALID ){
      const int o_signal = uwt_rev_convert_signal_number(term_signal);
      value callback = GET_CB_VAL(h->cb_read);
      value process = GET_CB_VAL(h->cb_listen);
      gr_root_unregister(&h->cb_read);
      gr_root_unregister(&h->cb_listen);
      exn=caml_callback3_exn(callback,
                             process,
                             Val_long(exit_status),
                             Val_long(o_signal));
    }
    if ( h->in_use_cnt ){
      --h->in_use_cnt;
    }
  }
  HANDLE_CB_RET(exn);
}

CAMLprim value
uwt_spawn(value p1, value p2, value p3, value p4)
{
  INIT_LOOP_WRAP(loop,Field(p1,0));
  uv_loop_t * l = &loop->loop;
  CAMLparam4(p1,p2,p3,p4);
  CAMLlocal2(ret,op);
  unsigned int i;
  int erg = UV_UWT_EFATAL;
  bool spawn_called = false;
  uv_process_options_t t;
  uv_stdio_container_t stdio[3];
  struct handle * handle;

  GR_ROOT_ENLARGE();
  ret = caml_alloc(1,Error_tag);
  op = handle_create(UV_PROCESS,loop);
  handle = Handle_val(op);
  handle->close_executed = 1;

  /* below: now further caml allocations */
  memset(&t, 0, sizeof t);
  memset(&stdio, 0, sizeof stdio);

  value tmp = Field(p1,1);

  for ( i = 0; i < 3; ++i ){
    value cur = Field(tmp,i);
    if ( cur == Val_unit ){
      stdio[i].flags = UV_IGNORE;
      stdio[i].data.stream = NULL;
    }
    else {
      cur = Field(cur,0);
      struct handle * h;
      const int tag = Tag_val(cur);
      cur = Field(cur,0);
      if ( tag == 0 ){
        stdio[i].flags = UV_INHERIT_FD;
        stdio[i].data.fd = FD_VAL(cur);
      }
      else {
        h = get_handle(cur);
        if ( h == NULL ){
          erg = UV_UWT_EBADF;
          goto error_end;
        }
        stdio[i].data.stream = (uv_stream_t*)h->handle;
        if ( tag == 1 ){
          if ( i == 0 ){
            stdio[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
          }
          else {
            stdio[i].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
          }
        }
        else {
          assert( tag == 2 || tag == 3 );
          stdio[i].flags = UV_INHERIT_STREAM;
        }
      }
    }
  }
  t.exit_cb = spawn_exit_cb;

  tmp = Field(p4,0) ;
  if ( !uwt_is_safe_string(tmp) ){
    erg = UV_ECHARSET;
    goto error_end;
  }
  t.file = String_val(tmp);

  tmp = Field(p4,1);
  if ( tmp == Atom(0) || Wosize_val(tmp) == 0 ){
    t.args = NULL;
  }
  else {
    t.args = caml_string_array_to_c_array(tmp,&erg);
    if ( t.args == NULL ){
      goto error_end;
    }
  }

  tmp = Field(p2,0);
  if ( Wosize_val(tmp) == 0 ){
    t.env =  NULL;
  }
  else {
    t.env = caml_string_array_to_c_array(tmp,&erg);
    if ( !t.env ){
      goto error_end;
    }
  }
  tmp = Field(p2,1);
  if ( tmp == Val_unit ){
    t.cwd = NULL;
  }
  else {
    tmp = Field(tmp,0);
    if ( !uwt_is_safe_string(tmp) ){
      erg = UV_ECHARSET;
      goto error_end;
    }
    t.cwd = String_val(tmp);
  }
  t.flags = Long_val(Field(p1,3));
  t.stdio_count = 3;
  t.stdio = stdio;
  t.uid = Long_val(Field(Field(p1,2),0));
  t.gid = Long_val(Field(Field(p1,2),1));

  spawn_called = true;
  handle->close_executed = 0;
  erg = uv_spawn(l, (uv_process_t*)handle->handle, &t);
  if ( erg < 0 ){
    /* uv_process_init is called internally first, see also:
       https://groups.google.com/forum/message/raw?msg=libuv/DUBr8DtzsWk/hw11ob9sPZ4J */
    handle->finalize_called = 1;
    handle_finalize_close(handle);
  }
  else {
    for ( i = 0; i < 3; ++i ){
      if ( (stdio[i].flags & UV_CREATE_PIPE) != 0 ){
        struct handle * h = stdio[i].data.stream->data;
        h->initialized = 1;
      }
    }
  }
error_end:
  if ( t.args ){
    free(t.args);
  }
  if ( t.env ){
    free(t.env);
  }
  if ( erg < 0 ){
    if ( spawn_called == false ){
      free_mem_uv_handle_t(handle);
      free_struct_handle(handle);
    }
    Field(op,1) = 0;
    Field(ret,0) = Val_uwt_error(erg);
  }
  else {
    if ( Is_block(p3) ){
      gr_root_register(&handle->cb_read,Field(p3,0));
      gr_root_register(&handle->cb_listen,op);
    }
    handle->initialized = 1;
    ++handle->in_use_cnt;
    Tag_val(ret) = Ok_tag;
    Store_field(ret,0,op);
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_pid_na(value o_h)
{
  HANDLE_NINIT_NA(h,o_h);
  uv_process_t * p = (uv_process_t *)h->handle;
  return (Val_long(p->pid));
}

CAMLprim value
uwt_process_kill_na(value o_h,value o_sig)
{
  HANDLE_NINIT_NA(h,o_h);
  uv_process_t * p = (uv_process_t *)h->handle;
  int signum = uwt_convert_signal_number(Long_val(o_sig));
  int ret = uv_process_kill(p,signum);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_kill_na(value o_pid,value o_sig)
{
  int signum = uwt_convert_signal_number(Long_val(o_sig));
  int ret = uv_kill(Long_val(o_pid),signum);
  return (VAL_UWT_UNIT_RESULT(ret));
}
/* }}} Process end */

/* {{{ Conv start */
CAMLprim value
uwt_set_crtfd_na(value o_fd)
{
#ifndef _WIN32
  (void)o_fd;
  return (Val_long(1));
#else
  return (Val_long(set_crt_fd(o_fd) == true));
#endif
}

CAMLprim value
uwt_fileno(value ohandle)
{
  HANDLE_NO_UNINIT_CLOSED_WRAP(ohandle);
  CAMLparam1(ohandle);
  CAMLlocal2(ocont,ofd);
  struct handle * s = Handle_val(ohandle);
  uv_os_fd_t fd;
  const int r = uv_fileno(s->handle,&fd);
  if ( r < 0 ){
    ocont = caml_alloc_small(1,Error_tag);
    Field(ocont,0) = Val_uwt_error(r);
  }
  else {
#ifndef _WIN32
    ofd = Val_long(fd);
#else
    ofd = win_alloc_handle(fd);
    CRT_fd_val(ofd) = s->orig_fd;
#endif
    ocont = caml_alloc_small(1,Ok_tag);
    Field(ocont,0) = ofd;
  }
  CAMLreturn(ocont);
}

/* }}} Conv End */

/* {{{ C_worker */
static void
common_after_work_cb(uv_work_t *req, int status)
{
  GET_RUNTIME();
  struct req * r = NULL;
  if (unlikely( !req || (r = req->data) == NULL ||
                r->cb == CB_INVALID || r->c_cb == NULL )){
    DEBUG_PF("fatal, no cb");
    req_free_most(r);
  }
  else {
    CAMLparam0();
    CAMLlocal1(exn);
    r = req->data;
    r->in_cb = 1;
    if ( status != 0 ){
      exn = caml_alloc_small(1,Error_tag);
      Field(exn,0) = Val_uwt_error(status);
    }
    else {
      exn = r->c_cb((uv_req_t *)req);
      if ( r->buf_contains_ba == 1 ){
        value t = caml_alloc_small(1,Ok_tag);
        Field(t,0) = exn;
        exn = t;
      }
    }
    exn = CAML_CALLBACK1(r,cb,exn);
    if ( Is_exception_result(exn) ){
      add_exception(r->loop,exn);
    }
    r->in_cb = 0;
    req_free_most(r);
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
  CAMLlocal1(o_cb);
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

  o_cb = Field(o_uwt,2);

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
  }
  else {
    gr_root_register(&req->cb,o_cb);
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
/* }}} C_worker end */

/* {{{ Unix start */
#ifdef _WIN32
#ifdef HAVE_UV_TRANSLATE_SYSERROR
extern int uv_translate_sys_error(int);
#endif
int
uwt_translate_sys_error(DWORD sys_errno)
{
  if ( sys_errno <= 0 ){
    return sys_errno;  /* If < 0 then it's already a libuv error. */
  }

  switch ( sys_errno ){
  /* translations from libuv */
  case ERROR_NOACCESS:                    return UV_EACCES;
  case WSAEACCES:                         return UV_EACCES;
  case ERROR_ADDRESS_ALREADY_ASSOCIATED:  return UV_EADDRINUSE;
  case WSAEADDRINUSE:                     return UV_EADDRINUSE;
  case WSAEADDRNOTAVAIL:                  return UV_EADDRNOTAVAIL;
  case WSAEAFNOSUPPORT:                   return UV_EAFNOSUPPORT;
  case WSAEWOULDBLOCK:                    return UV_EAGAIN;
  case WSAEALREADY:                       return UV_EALREADY;
  case ERROR_INVALID_FLAGS:               return UV_EBADF;
  case ERROR_INVALID_HANDLE:              return UV_EBADF;
  case ERROR_LOCK_VIOLATION:              return UV_EBUSY;
  case ERROR_PIPE_BUSY:                   return UV_EBUSY;
  case ERROR_SHARING_VIOLATION:           return UV_EBUSY;
  case ERROR_OPERATION_ABORTED:           return UV_ECANCELED;
  case WSAEINTR:                          return UV_ECANCELED;
  case ERROR_NO_UNICODE_TRANSLATION:      return UV_ECHARSET;
  case ERROR_CONNECTION_ABORTED:          return UV_ECONNABORTED;
  case WSAECONNABORTED:                   return UV_ECONNABORTED;
  case ERROR_CONNECTION_REFUSED:          return UV_ECONNREFUSED;
  case WSAECONNREFUSED:                   return UV_ECONNREFUSED;
  case ERROR_NETNAME_DELETED:             return UV_ECONNRESET;
  case WSAECONNRESET:                     return UV_ECONNRESET;
  case ERROR_ALREADY_EXISTS:              return UV_EEXIST;
  case ERROR_FILE_EXISTS:                 return UV_EEXIST;
  case ERROR_BUFFER_OVERFLOW:             return UV_EFAULT;
  case WSAEFAULT:                         return UV_EFAULT;
  case ERROR_HOST_UNREACHABLE:            return UV_EHOSTUNREACH;
  case WSAEHOSTUNREACH:                   return UV_EHOSTUNREACH;
  case ERROR_INSUFFICIENT_BUFFER:         return UV_EINVAL;
  case ERROR_INVALID_DATA:                return UV_EINVAL;
  case ERROR_INVALID_PARAMETER:           return UV_EINVAL;
#ifdef ERROR_SYMLINK_NOT_SUPPORTED
  case ERROR_SYMLINK_NOT_SUPPORTED:       return UV_EINVAL;
#endif
  case WSAEINVAL:                         return UV_EINVAL;
  case WSAEPFNOSUPPORT:                   return UV_EINVAL;
  case WSAESOCKTNOSUPPORT:                return UV_EINVAL;
  case ERROR_BEGINNING_OF_MEDIA:          return UV_EIO;
  case ERROR_BUS_RESET:                   return UV_EIO;
  case ERROR_CRC:                         return UV_EIO;
  case ERROR_DEVICE_DOOR_OPEN:            return UV_EIO;
  case ERROR_DEVICE_REQUIRES_CLEANING:    return UV_EIO;
  case ERROR_DISK_CORRUPT:                return UV_EIO;
  case ERROR_EOM_OVERFLOW:                return UV_EIO;
  case ERROR_FILEMARK_DETECTED:           return UV_EIO;
  case ERROR_GEN_FAILURE:                 return UV_EIO;
  case ERROR_INVALID_BLOCK_LENGTH:        return UV_EIO;
  case ERROR_IO_DEVICE:                   return UV_EIO;
  case ERROR_NO_DATA_DETECTED:            return UV_EIO;
  case ERROR_NO_SIGNAL_SENT:              return UV_EIO;
  case ERROR_OPEN_FAILED:                 return UV_EIO;
  case ERROR_SETMARK_DETECTED:            return UV_EIO;
  case ERROR_SIGNAL_REFUSED:              return UV_EIO;
  case WSAEISCONN:                        return UV_EISCONN;
  case ERROR_CANT_RESOLVE_FILENAME:       return UV_ELOOP;
  case ERROR_TOO_MANY_OPEN_FILES:         return UV_EMFILE;
  case WSAEMFILE:                         return UV_EMFILE;
  case WSAEMSGSIZE:                       return UV_EMSGSIZE;
  case ERROR_FILENAME_EXCED_RANGE:        return UV_ENAMETOOLONG;
  case ERROR_NETWORK_UNREACHABLE:         return UV_ENETUNREACH;
  case WSAENETUNREACH:                    return UV_ENETUNREACH;
  case WSAENOBUFS:                        return UV_ENOBUFS;
  case ERROR_DIRECTORY:                   return UV_ENOENT;
  case ERROR_FILE_NOT_FOUND:              return UV_ENOENT;
  case ERROR_INVALID_NAME:                return UV_ENOENT;
  case ERROR_INVALID_DRIVE:               return UV_ENOENT;
  case ERROR_INVALID_REPARSE_DATA:        return UV_ENOENT;
  case ERROR_MOD_NOT_FOUND:               return UV_ENOENT;
  case ERROR_PATH_NOT_FOUND:              return UV_ENOENT;
  case WSAHOST_NOT_FOUND:                 return UV_ENOENT;
  case WSANO_DATA:                        return UV_ENOENT;
  case ERROR_NOT_ENOUGH_MEMORY:           return UV_ENOMEM;
  case ERROR_OUTOFMEMORY:                 return UV_ENOMEM;
  case ERROR_CANNOT_MAKE:                 return UV_ENOSPC;
  case ERROR_DISK_FULL:                   return UV_ENOSPC;
  case ERROR_EA_TABLE_FULL:               return UV_ENOSPC;
  case ERROR_END_OF_MEDIA:                return UV_ENOSPC;
  case ERROR_HANDLE_DISK_FULL:            return UV_ENOSPC;
  case ERROR_NOT_CONNECTED:               return UV_ENOTCONN;
  case WSAENOTCONN:                       return UV_ENOTCONN;
  case ERROR_DIR_NOT_EMPTY:               return UV_ENOTEMPTY;
  case WSAENOTSOCK:                       return UV_ENOTSOCK;
  case ERROR_NOT_SUPPORTED:               return UV_ENOTSUP;
  case ERROR_BROKEN_PIPE:                 return UV_EOF;
  case ERROR_ACCESS_DENIED:               return UV_EPERM;
  case ERROR_PRIVILEGE_NOT_HELD:          return UV_EPERM;
  case ERROR_BAD_PIPE:                    return UV_EPIPE;
  case ERROR_NO_DATA:                     return UV_EPIPE;
  case ERROR_PIPE_NOT_CONNECTED:          return UV_EPIPE;
  case WSAESHUTDOWN:                      return UV_EPIPE;
  case WSAEPROTONOSUPPORT:                return UV_EPROTONOSUPPORT;
  case ERROR_WRITE_PROTECT:               return UV_EROFS;
  case ERROR_SEM_TIMEOUT:                 return UV_ETIMEDOUT;
  case WSAETIMEDOUT:                      return UV_ETIMEDOUT;
  case ERROR_NOT_SAME_DEVICE:             return UV_EXDEV;
  case ERROR_INVALID_FUNCTION:            return UV_EISDIR;
  case ERROR_META_EXPANSION_TOO_LONG:     return UV_E2BIG;

  /* imported from ocaml */
  case ERROR_ARENA_TRASHED:               return UV_ENOMEM;
  case ERROR_INVALID_BLOCK:               return UV_ENOMEM;
  case ERROR_BAD_ENVIRONMENT:             return UV_E2BIG;
  case ERROR_INVALID_ACCESS:              return UV_EINVAL;
  case ERROR_CURRENT_DIRECTORY:           return UV_EACCES;
  case ERROR_NO_MORE_FILES:               return UV_ENOENT;
  case ERROR_BAD_NETPATH:                 return UV_ENOENT;
  case ERROR_NETWORK_ACCESS_DENIED:       return UV_EACCES;
  case ERROR_BAD_NET_NAME:                return UV_ENOENT;
  case ERROR_FAIL_I24:                    return UV_EACCES;
  case ERROR_NO_PROC_SLOTS:               return UV_EAGAIN;
  case ERROR_DRIVE_LOCKED:                return UV_EACCES;
  case ERROR_INVALID_TARGET_HANDLE:       return UV_EBADF;
  case ERROR_DIRECT_ACCESS_HANDLE:        return UV_EBADF;
  case ERROR_NEGATIVE_SEEK:               return UV_EINVAL;
  case ERROR_SEEK_ON_DEVICE:              return UV_EACCES;
  case ERROR_NOT_LOCKED:                  return UV_EACCES;
  case ERROR_BAD_PATHNAME:                return UV_ENOENT;
  case ERROR_MAX_THRDS_REACHED:           return UV_EAGAIN;
  case ERROR_LOCK_FAILED:                 return UV_EACCES;
  case ERROR_NOT_ENOUGH_QUOTA:            return UV_ENOMEM;
#ifdef WSANAMETOOLONG
  case WSANAMETOOLONG:                    return UV_ENAMETOOLONG;
#endif
#ifdef WSAENFILE
  case WSAENFILE:                         return UV_ENFILE;
#endif
  case WSAENOTEMPTY:                      return UV_ENOTEMPTY;
  default:
    {
#ifdef HAVE_UV_TRANSLATE_SYSERROR
      int k = uv_translate_sys_error(sys_errno);
      if ( k < 0 ){
        return k;
      }
      else {
        return UV_UNKNOWN;
      }
#else
      return UV_UNKNOWN;
#endif
    }
  }
}
#endif

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
    req_free(req);
  }
  else {
    gr_root_register(&req->cb,o_cb);
    req->c_cb = lseek_cb;
    req->in_use = 1;
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}
BYTE_WRAP6(uwt_lseek)
/* }}} Unix end */

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
  unsigned int i;
  (void) unit;
  if ( uwt_global_caml_root != Val_unit ){
    unsigned int found = 0;
    unsigned int ar_size = Wosize_val(uwt_global_caml_root);
    for ( i = 0 ; i < ar_size ; ++i ){
      value aro = Field(uwt_global_caml_root,i);
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
      uwt_global_caml_root_size = 0;
      uwt_global_caml_root_n = 0;
      caml_remove_generational_global_root(&uwt_global_caml_root);
      uwt_global_caml_root = Val_unit;
      free(uwt_global_caml_root_free_pos);
      uwt_global_caml_root_free_pos = NULL;
    }
    else {
      DEBUG_PF("uwt_global_caml_root still in use, found %u elements\n",found);
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
    DEBUG_PF("there are still %u handles ot close\n",
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
