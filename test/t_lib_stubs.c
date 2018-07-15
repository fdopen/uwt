#include "../src/config.h"
#include "../src/uwt-worker.h"
#include <string.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

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

struct data {
    char * fln;
    char * content;
    size_t len;
};

static void
test_worker(uv_work_t * req)
{
  FILE *f;
  struct worker_params * w = req->data;
  struct data * p = w->p1;
  size_t s1 = 0;
  int s2;
  f = fopen(p->fln,"w");
  if ( !f ){
    return ;
  }
  if ( p->len != 0 ){
    s1 = fwrite(p->content,sizeof(char),p->len,f);
  }
  s2 = fclose(f);
  if ( s1 == p->len && s2 == 0 ){
    w->p2 = (void*)1;
  }
}

static void
test_cleanup(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    struct data * p = w->p1;
    free(p->fln);
    free(p->content);
    free(p);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
test_camlval(uv_req_t * req)
{
  struct worker_params * w = req->data;
  return (Val_long(w->p2 != NULL));
}

CAMLextern value
uwt_external_test(value o_user, value o_uwt);

CAMLprim value
uwt_external_test(value o_user, value o_uwt)
{
  CAMLparam2(o_user,o_uwt);
  struct data * p1;
  p1 = malloc(sizeof *p1);
  if ( p1 == NULL ){
    caml_raise_out_of_memory();
  }
  p1->fln = custom_strdup(String_val(Field(o_user,0)));
  if ( p1->fln == NULL ){
    caml_raise_out_of_memory();
  }
  value o_str = Field(o_user,1);
  char * c_str = String_val(o_str);
  p1->len = caml_string_length(o_str);
  if (!p1->len){
    p1->content = NULL;
  }
  else {
    p1->content = malloc(p1->len);
    if ( p1->content == NULL ){
      free(p1->fln);
      free(p1);
      caml_raise_out_of_memory();
    }
    memcpy(p1->content,c_str,p1->len);
  }
  value ret;
  ret = uwt_add_worker(o_uwt,
                       test_cleanup,
                       test_worker,
                       test_camlval,
                       p1,
                       NULL);
  CAMLreturn(ret);
}
