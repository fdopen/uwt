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

#ifndef __UWT_STUBS_BASE_H
#define __UWT_STUBS_BASE_H

#include "uwt_stubs_common.h"

#ifdef __cplusplus
extern "C" {
#endif

/* memory in the OCaml heap */

typedef unsigned int cb_t;
#define CB_INVALID UINT_MAX

/* only use the access macros GET_CB_VAL / CAML_CALLBACK1,
 * uwt__gr_unregister, uwt__gr_register , everything else will be
 * changed */

UWT_LOCAL value uwt__global_caml_root;
UWT_LOCAL cb_t uwt__global_caml_root_size;
UWT_LOCAL cb_t uwt__global_caml_root_n;
UWT_LOCAL cb_t *uwt__global_caml_root_free_pos;

#define GR_ROOT_INIT_SIZE_P2 12u
#define GET_CB_VAL__I(x)                        \
  ((cb_t)(x) / (1u << GR_ROOT_INIT_SIZE_P2))
#define GET_CB_VAL__J(x)                            \
  ((cb_t)(x) & ((1u << GR_ROOT_INIT_SIZE_P2) - 1u))

#define GET_CB_VAL(cb)                                                  \
  (Field(Field(uwt__global_caml_root,GET_CB_VAL__I(cb)),GET_CB_VAL__J(cb)))

#define CAML_CALLBACK1(_wp,_ct,_val)            \
  caml_callback2_exn(*uwt__global_wakeup,       \
                     GET_CB_VAL((_wp)->_ct),    \
                     (_val))

UWT_LOCAL void
uwt__gr_unregister(cb_t *a);

UWT_LOCAL void
uwt__gr_enlarge__(void);
UWT_LOCAL void
uwt__gr_register__(cb_t *a,value x);

#define GR_ROOT_ENLARGE()                             \
  ATTR_UNUSED                                         \
  void (* const uwt__gr_register)(cb_t *a,value x) =  \
    uwt__gr_register__;                               \
  do {                                                \
    if (unlikely( uwt__global_caml_root_n + 4 >=      \
                  uwt__global_caml_root_size )){      \
      uwt__gr_enlarge__();                            \
    }                                                 \
  } while(0)

/* ocaml runtime lock */
UWT_LOCAL bool uwt_global_runtime_released;
#define GET_RUNTIME()                           \
  do {                                          \
    if ( uwt_global_runtime_released == true ){ \
      uwt_global_runtime_released = false;      \
      caml_leave_blocking_section();            \
    }                                           \
  } while (0)


/* uv_loop_t stubs */
enum cb_type {
  CB_SYNC = 0,
  CB_LWT = 1,
  CB_CB = 2, /* callback mode not implemented */
  CB_MAX = 3
};

struct loop {
    uv_loop_t loop;
    uv_prepare_t prep;
    unsigned int init_called:1;
    unsigned int exn_caught:1;
    unsigned int in_use :1;
    unsigned int do_clean: 1;
    unsigned int loop_type: 2; /* of cb_type */
};

#define Loop_val(v)                             \
  ( (struct loop *)( Field((v),1)) )

UWT_EXTERN2(uwt_run_loop);
UWT_EXTERN1(uwt_default_loop);

#define INIT_LOOP_RESULT(x,y)                     \
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

#define RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req)                     \
  if (unlikely( loop == NULL || req == NULL || loop->init_called == 0 || \
                req->req == NULL || req->in_use == 1 )){                 \
    return VAL_UWT_INT_RESULT_UWT_EFATAL;                                \
  }

/* memory handling (c heap) */
UWT_EXTERN1(uwt_init_stacks_na);

UWT_LOCAL void
uwt__malloc_uv_buf_t(uv_buf_t *, size_t, enum cb_type);

UWT_LOCAL void
uwt__free_uv_buf_t_const(const uv_buf_t *, enum cb_type);

UWT_LOCAL void
uwt__free_uv_buf_t(uv_buf_t *, enum cb_type);

/* uv_req_t stubs */

typedef value (*req_c_cb)(uv_req_t*);
typedef void (*clean_cb)(uv_req_t*);

struct req {
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

UWT_LOCAL struct req * uwt__req_create(uv_req_type, struct loop *);

/* deallocates everything possible (some parts can't
   be cleaned as long as req is referenced from the OCaml heap */
UWT_LOCAL void uwt__req_free_most(struct req *);

/* deallocate everything */
UWT_LOCAL void uwt__req_free(struct req *);

UWT_LOCAL void uwt__free_mem_uv_req_t(struct req * wp);
UWT_LOCAL void uwt__free_struct_req(struct req *r);

UWT_LOCAL void  uwt__req_callback(uv_req_t * req);
UWT_LOCAL value uwt__ret_uv_fs_result_unit(uv_req_t * r);
UWT_LOCAL value uwt__ret_unit_cparam(uv_req_t * r);

UWT_EXTERN2(uwt_req_create);
UWT_EXTERN1(uwt_req_cancel_na);
UWT_EXTERN1(uwt_req_finalize_na);

/* basic uv_handle_t stub code */
struct handle {
    uv_handle_t * handle;
    struct loop * loop;
    size_t obuf_offset; /* for read_own */
    size_t c_read_size; /* passed to the alloc function */
    void * ba_read; /* pointer to bigarray for reading */
    cb_t cb_listen;
    cb_t cb_read;
    cb_t cb_close;
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

UWT_LOCAL value uwt__handle_create(uv_handle_type, struct loop *);
UWT_LOCAL void uwt__handle_free_common(struct handle *);
UWT_LOCAL void uwt__cancel_reader(struct handle *);
UWT_LOCAL void uwt__free_mem_uv_handle_t(struct handle *);
UWT_LOCAL void uwt__free_struct_handle(struct handle *);
UWT_LOCAL void uwt__handle_finalize_close_cb(uv_handle_t *);
UWT_LOCAL void uwt__handle_finalize_close(struct handle *);

/* other */
/* currently Lwt.wakeup */
UWT_LOCAL value *uwt__global_wakeup;

/* currently !Lwt.async_exception_hook e */
UWT_LOCAL void uwt__add_exception(struct loop *l, value e);

/* debugging */
UWT_EXTERN2(uwt_test_req_leak);
UWT_EXTERN1(uwt_free_all_memory);
UWT_EXTERN1(uwt_cleanup_na);

UWT_LOCAL void uwt__clean_iovecs(uv_req_t *);
UWT_LOCAL int uwt__build_iovecs(value, struct req *);

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_BASE_H */
