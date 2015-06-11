#include <string.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "../src/uwt-worker.h"

static char *
custom_strdup (const char *s)
{
  size_t len;
  void *n;
  if ( s == NULL ){
    return NULL;
  }
  len = strlen(s) + 1;
  n = malloc(len);
  if (n == NULL){
    return NULL;
  }
  memcpy(n,s,len);
  return n;
}

static void
test_worker(uv_work_t * req)
{
  FILE *f;
  struct worker_params * w = req->data;
  char * filename = w->p1;
  const char * content = w->p2;
  int s1;
  int s2;
  w->p1 = (void*)1;
  f = fopen(filename,"w");
  free(filename);
  if ( !f ){
    return ;
  }
  s1 = fputs(content,f);
  s2 = fclose(f);
  if ( s1 >= 0 && s2 == 0 ){
    w->p1 = NULL;
  }
}

static void
test_cleanup(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL && w->p1 != (void*)1 ){
    free(w->p1);
  }
  if ( w->p2 != NULL ){
    free(w->p2);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
test_camlval(uv_req_t * req)
{
  struct worker_params * w = req->data;
  return (Val_long(w->p1 == NULL));
}

CAMLextern value
uwt_external_test(value o_user, value o_uwt);

CAMLprim value
uwt_external_test(value o_user, value o_uwt)
{
  CAMLparam2(o_user,o_uwt);
  char * p1;
  char * p2;
  value ret;
  p1 = custom_strdup(String_val(Field(o_user,0)));
  if ( p1 == NULL ){
    caml_raise_out_of_memory();
  }
  p2 = custom_strdup(String_val(Field(o_user,1)));
  if ( p2 == NULL ){
    free(p1);
    caml_raise_out_of_memory();
  }
  ret = uwt_add_worker(o_uwt,
                       test_cleanup,
                       test_worker,
                       test_camlval,
                       p1,
                       p2);
  CAMLreturn(ret);
}
