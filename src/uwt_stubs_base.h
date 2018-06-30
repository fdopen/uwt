/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

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

struct loop {
    uv_loop_t loop;
    uv_prepare_t prep;
    unsigned int init_called:1;
    unsigned int stop_loop_earlier:1;
    unsigned int loop_will_wait:1;
    unsigned int in_use :1;
};

#define Loop_val(v)                             \
  ((struct loop *)(UNTAG_POINTER(v)))

#define Uv_loop_val(v)                          \
  &(Loop_val(v)->loop)

UWT_EXTERN2(uwt_run_loop);
UWT_EXTERN1(uwt_default_loop);

/* Checks disabled: there is currently only one loop
   that is initialized at startup and never closed */
#define INIT_LOOP_RESULT(x,y)                   \
  struct loop * x = Loop_val(y)

#define INIT_LOOP_INT_RESULT(x,y)               \
  struct loop * x = Loop_val(y)

/* memory handling (c heap) */
UWT_EXTERN1(uwt_init_na);
#ifdef _WIN32
UWT_EXTERN1(uwt_init_sync_na);
#endif

UWT_LOCAL void
uwt__malloc_uv_buf_t(uv_buf_t *, size_t);

UWT_LOCAL void
uwt__free_uv_buf_t_const(const uv_buf_t *);

UWT_LOCAL void
uwt__free_uv_buf_t(uv_buf_t *);

/* uv_req_t stubs */

typedef value (*req_c_cb)(uv_req_t*);
typedef void (*clean_cb)(uv_req_t*);

struct req {
    struct worker_params c;
    uv_req_t * req;
    size_t offset;
    req_c_cb c_cb;
    clean_cb clean_cb;
    uv_buf_t buf;
    cb_t cb;
    cb_t sbuf;
    unsigned int buf_contains_ba: 1; /* used for other purpose, if buf unused */
};

UWT_LOCAL struct req * uwt__req_create(uv_req_type);
UWT_LOCAL struct req * uwt__req_create_null(uv_req_type);
UWT_LOCAL struct req * uwt__req_create_res(uv_req_type, value *);
UWT_LOCAL void uwt__req_free(struct req *r);

UWT_EXTERN1(uwt_req_cancel_na);

/* basic uv_handle_t stub code */
struct handle {
    uv_handle_t * handle; /* handle must have the same lifetime as
                             struct handle. It should be allocated and freed at
                             the same time */
    size_t c_read_size; /* passed to the alloc function */
    union {
        size_t obuf_offset; /* for read_own */
        void * ba_read; /* pointer to bigarray for reading */
        int process_finished; /* don't kill processes that are finished */
    } x;
    cb_t cb_listen;
    cb_t cb_read;
    cb_t cb_close;
#ifdef _WIN32
    int orig_fd; /* when converting to and back from Unix.file_descr, I have to
                    save the original crt fd in order to avoid descriptor
                    leaking. See mantis #5258 */
#endif
    uint16_t in_use_cnt; /* only relevant for handles with finalizers attached.
                            (currently: everything that wraps a fd and
                            process handles) */

    /* initialized doesn't mean _init() was called. Some handles
       contain only garbage after init was called (e.g uv_pipe_init).
       If you pass such a handle to e.g. uv_write or uv_read, you
       program will segfault (best case).  Most errors are already
       captured by libuv, but not all.  Therefore, I try to keep track
       with this flag, if a handle is valid or not.
    */
    unsigned int initialized: 1;
    unsigned int finalize_called: 1;
    unsigned int close_called: 1;
    unsigned int use_read_ba: 1;
    unsigned int can_reuse_cb_read:1;
    unsigned int read_waiting: 1;
};

#ifdef Handle_val
#undef Handle_val
#define OCAML_Handle_val(v)                               \
  (((struct filedescr *) Data_custom_val(v))->fd.handle)
#endif

#define Handle_val(x)                           \
  ((struct handle*)(Field((x),1)))

UWT_LOCAL value uwt__handle_res_create(uv_handle_type, bool);
UWT_LOCAL int uwt__pipe_handle_reinit(struct handle *);
UWT_LOCAL void uwt__handle_unreg_camlval(struct handle *);
UWT_LOCAL void uwt__cancel_reader(struct handle *);
UWT_LOCAL void uwt__handle_finalize_close_cb(uv_handle_t *);
UWT_LOCAL void uwt__handle_finalize_close(struct handle *);
UWT_LOCAL void uwt__free_handle(struct handle *);

/* other */
/* currently Lwt.wakeup */
UWT_LOCAL value *uwt__global_wakeup;
UWT_LOCAL value *uwt_global_exception_fun;

/* debugging */
UWT_EXTERN1(uwt_free_all_memory);
UWT_EXTERN1(uwt_cleanup_na);

UWT_LOCAL int uwt__build_iovecs(value, struct req *);

#define REQ_CB_INIT(req)                        \
  uv_req_t * const req_ = (uv_req_t*)req;       \
  struct req * const wp_ = req_->data;          \
  GET_RUNTIME()

#define REQ_CB_CALL(val)                                        \
  do {                                                          \
    value exn_ = (val);                                         \
    value cb_ = GET_CB_VAL(wp_->cb);                            \
    uwt__gr_unregister(&wp_->cb);                               \
    uwt__req_free(wp_);                                         \
    exn_ = caml_callback2_exn(*uwt__global_wakeup, cb_, exn_);  \
    if (unlikely( Is_exception_result(exn_) )){                 \
      caml_callback_exn(*uwt_global_exception_fun,              \
                        Extract_exception(exn_));               \
    }                                                           \
  } while(0)

#ifdef __cplusplus
}
#endif

#endif /* __UWT_STUBS_BASE_H */
