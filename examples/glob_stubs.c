#include <glob.h>
#include <string.h>
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
glob_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  const char * pattern = w->p1;
  glob_t gl;
  size_t i;
  char ** l;
  int result;
  gl.gl_pathc = 0;
  gl.gl_pathv = NULL;
  result = glob(pattern, 0, NULL, &gl);
  w->p2 = (void*)1; /* default case: error */
  if ( result == GLOB_NOMATCH ){
    w->p2 = NULL;
    return;
  }
  if ( result != 0 || gl.gl_pathc == 0 || gl.gl_pathv == NULL ){
    /* other errors */
    return ;
  }
  l = calloc(sizeof(char *),gl.gl_pathc + 1);
  if ( l == NULL ){
    return;
  }
  for ( i = 0 ; i < gl.gl_pathc ; ++i ){
    char * c = custom_strdup(gl.gl_pathv[i]);
    if ( c == NULL ){
      goto cleanup;
    }
    l[i] = c;
  }
  l[gl.gl_pathc]=NULL;
  globfree(&gl);
  w->p2 = l;
  return;
 cleanup:
  for ( i = 0 ; i < gl.gl_pathc ; ++i ){
    if ( l[i] != NULL ){
      free(l[i]);
    }
  }
  free(l);
  globfree(&gl);
}

static void
glob_cleanup(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    free(w->p1);
  }
  if ( w->p2 != NULL && w->p2 != (void*) 1){
    char ** l = w->p2;
    char ** p = l;
    while ( *p ){
      free(*p);
      ++p;
    }
    free(l);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
glob_camlval(uv_req_t * req)
{
  CAMLparam0();
  CAMLlocal2(cont,ar);
  struct worker_params * w = req->data;
  if ( w->p2 == NULL ){ /* no matches, return Some([]) */
    cont = caml_alloc(1,0);
    Store_field(cont,0,Atom(0));
  }
  else if ( w->p2 == (void*)1 ){
    cont = Val_unit ; /* error, return None */
  }
  else {
    ar = caml_copy_string_array(w->p2);
    cont = caml_alloc(1,0);
    Store_field(cont,0,ar);
  }
  CAMLreturn(cont);
}

CAMLextern value
uwt_custom_test(value o_user, value o_uwt);

CAMLprim value
uwt_custom_test(value o_user, value o_uwt)
{
  CAMLparam2(o_user,o_uwt);
  value ret;
  char * p1 = custom_strdup(String_val(o_user));
  if ( p1 == NULL ){
    caml_raise_out_of_memory();
  }
  ret = uwt_add_worker(o_uwt,
                       glob_cleanup,
                       glob_worker,
                       glob_camlval,
                       p1,
                       NULL);
  CAMLreturn(ret);
}
