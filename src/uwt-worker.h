/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UV_WORKER_H
#define __UV_WORKER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <uv.h>
#include <caml/mlvalues.h>

#define Ok_tag ((unsigned char)0)
#define Error_tag ((unsigned char)1)

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
   This function should not access the OCaml heap/runtime.
*/
typedef void (*cb_cleaner)(uv_req_t *);

int
uwt_add_worker(value o_uwt,
               cb_cleaner cleaner,
               cb_worker worker,
               cb_camlval camlval,
               void * p1,
               void * p2);

int
uwt_add_worker_result(value o_uwt,
                      cb_cleaner cleaner,
                      cb_worker worker,
                      cb_camlval camlval,
                      void * p1,
                      void * p2);

#if defined(__GNUC__) && __GNUC__ >= 3
value Val_uwt_error(int n) __attribute__ ((const));
value Val_uwt_int_result(intnat n) __attribute__ ((const));
#else
value Val_uwt_error(int n);
value Val_uwt_int_result(intnat n);
#endif

#ifdef _WIN32
int uwt_translate_sys_error(DWORD);
int uwt_translate_errno(int);
#define UWT_TRANSLATE_ERRNO uwt_translate_errno
char * uwt_utf16_to_utf8(const WCHAR* utf16_buffer, int * error);
WCHAR * uwt_utf8_to_utf16(const char* utf8_buffer,int *error);
#else
#define UWT_TRANSLATE_ERRNO(x) (-(x))
#endif

int uwt_is_safe_string (value);

#ifdef __cplusplus
}
#endif

#endif /* __UV_WORKER_H */
