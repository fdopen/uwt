#ifndef __UV_WORKER_H
#define __UV_WORKER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <uv.h>
#include <caml/mlvalues.h>

struct worker_params {
    void *p1;
    void *p2;
};

/*
  The worker thread is not allowed to access the OCaml heap, because
  he can run in parallel to the OCaml runtime. You have to copy all
  data you need to the c heap in your init function.

  If your lwt thread was cancelled, it's possible, that this function
  will not be called at all.
*/
typedef uv_work_cb cb_worker;

/*
   This function is used in order to create a OCaml value from the
   result of the work thread function.

   You are not allowed to return exceptions and your functions should
   be as short as possible.
*/
typedef value (*cb_camlval)(uv_req_t *);

/*
   You can set it to NULL, if you don't need to free any ressources.
   Otherwise, the cleaner function will always be called.
*/
typedef void (*cb_cleaner)(uv_req_t *);


int
uwt_add_worker(value o_uwt,
               cb_cleaner cleaner,
               cb_worker worker,
               cb_camlval camlval,
               void * p1,
               void * p2);

#ifdef __cplusplus
}
#endif

#endif /* __UV_WORKER_H */
