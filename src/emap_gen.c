/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "config.h"
#include <uv.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#ifdef NDEBUG
#undef NDEBUG
#endif
#include <assert.h>

#define AR_SIZE(x) ((sizeof (x)) / (sizeof *(x)))

#define UV_UWT_EFATAL (int16_t)(INT16_MAX + 17)

static const char *
get_er_msg(unsigned int d)
{
  switch(d){
  case UV_UWT_EFATAL: return "fatal uwt error";
  default: assert(0);
  }
  return "unknown uv error";
}

static const
int er_map [] = {
  UV_E2BIG,
  UV_EACCES,
  UV_EADDRINUSE,
  UV_EADDRNOTAVAIL,
  UV_EAFNOSUPPORT,
  UV_EAGAIN,
  UV_EAI_ADDRFAMILY,
  UV_EAI_AGAIN,
  UV_EAI_BADFLAGS,
  UV_EAI_BADHINTS,
  UV_EAI_CANCELED,
  UV_EAI_FAIL,
  UV_EAI_FAMILY,
  UV_EAI_MEMORY,
  UV_EAI_NODATA,
  UV_EAI_NONAME,
  UV_EAI_OVERFLOW,
  UV_EAI_PROTOCOL,
  UV_EAI_SERVICE,
  UV_EAI_SOCKTYPE,
  UV_EALREADY,
  UV_EBADF,
  UV_EBUSY,
  UV_ECANCELED,
  UV_ECHARSET,
  UV_ECONNABORTED,
  UV_ECONNREFUSED,
  UV_ECONNRESET,
  UV_EDESTADDRREQ,
  UV_EEXIST,
  UV_EFAULT,
  UV_EFBIG,
  UV_EHOSTUNREACH,
  UV_EINTR,
  UV_EINVAL,
  UV_EIO,
  UV_EISCONN,
  UV_EISDIR,
  UV_ELOOP,
  UV_EMFILE,
  UV_EMSGSIZE,
  UV_ENAMETOOLONG,
  UV_ENETDOWN,
  UV_ENETUNREACH,
  UV_ENFILE,
  UV_ENOBUFS,
  UV_ENODEV,
  UV_ENOENT,
  UV_ENOMEM,
  UV_ENONET,
  UV_ENOPROTOOPT,
  UV_ENOSPC,
  UV_ENOSYS,
  UV_ENOTCONN,
  UV_ENOTDIR,
  UV_ENOTEMPTY,
  UV_ENOTSOCK,
  UV_ENOTSUP,
  UV_EPERM,
  UV_EPIPE,
  UV_EPROTO,
  UV_EPROTONOSUPPORT,
  UV_EPROTOTYPE,
  UV_ERANGE,
  UV_EROFS,
  UV_ESHUTDOWN,
  UV_ESPIPE,
  UV_ESRCH,
  UV_ETIMEDOUT,
  UV_ETXTBSY,
  UV_EXDEV,
  UV_UNKNOWN,
  UV_EOF,
  UV_ENXIO,
  UV_EMLINK,
  UV_UWT_EFATAL,
};

static const char *
err_name(int i)
{
  switch (i){
  case UV_UWT_EFATAL: return "UWT_EFATAL";
  default: return (uv_err_name(i));
  }
}

static char *
lower_err_name(int x)
{
  const char * up = err_name(x);
  size_t len = strlen(up);
  char * low = malloc(len+1);
  if ( len && !low ){
    fputs("out of memory",stderr);
    exit(1);
  }
  size_t i;
  for ( i = 0 ; i < len ; ++i ){
    low[i] = tolower(up[i]);
  }
  low[len]='\0';
  return low;
}

static inline int neg_result(unsigned int x)
{
  return(((int)(x+1))*(-1));
}

int main(void)
{
  unsigned int i;
  unsigned int uv_unknown = UINT_MAX;

  FILE * c = fopen("map_error.h","w");
  FILE * c2 = fopen("uwt-error.h","w");
  FILE * ml = fopen("error.ml","w");

  if (!c || !c2 || !ml){
    fputs("can't create error files\n",stderr);
    return 1;
  }

  fputs("typedef enum {\n",c2);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    if ( val == UV_UNKNOWN ){
      uv_unknown = i;
    }
    if ( x[0] == 'U' && x[1] == 'W' && x[2] == 'T' ){
      fprintf(c2,"  UV_%s = (%d),\n",x,val);
    }
  }
  fputs("} uv_uwt_errno_t;\n\n",c2);

  assert( uv_unknown != UINT_MAX );

  fputs("typedef enum {\n",c2);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(c2,"  VAL_UWT_ERROR_%s = (Val_long(%u)),\n",x,i);
  }
  fputs("} val_uwt_error_t;\n\n",c2);

  fputs("typedef enum {\n",c2);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(c2,"  VAL_UWT_INT_RESULT_%s = (Val_long(%d)),\n",x,neg_result(i));
  }
  fputs("} val_uwt_int_result_errno_t;\n",c2);

  fputs("#include \"macros.h\"\n"
        "DISABLE_WARNING_ENUM_COMPARE()\n",c);

  fputs("value\n"
        "Val_uwt_error(int n)\n"
        "{\n"
        "  value erg;\n\n",c);

  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    if ( x[0] != 'U' || x[1] != 'W' || x[2] != 'T' ){
      unsigned int j;
      fprintf(c,"  _Static_assert( UV_%s < 0 && UV_%s >= INT16_MIN,"
              "\"out of range\");\n", x,x);
      for ( j = 0 ; j < AR_SIZE(er_map); ++j){
        int val2 = er_map[j];
        const char * y = err_name(val2);
        if ( y[0] == 'U' && y[1] == 'W' && y[2] == 'T' ){
          fprintf(c,"  _Static_assert( UV_%s != UV_%s ,\"error code reuse\");\n",
                  x,y);
        }
      }
    }
  }
  fputs("  switch (n) {\n",c);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(c,"  case UV_%s: erg = Val_long(%u) ; break;\n",x,i);
  }
  fputs("  default: erg = VAL_UWT_ERROR_UNKNOWN;\n"
        "  }\n"
        "  return erg;\n"
        "}\n",c);

  fputs("\n",c);
  fputs("POP_WARNING()\n\n",c);

  fputs("value\n"
        "Val_uwt_int_result(intnat n)\n"
        "{\n"
        "  value erg;\n"
        "  if ( n >= 0 ){\n"
        "    return Val_long(n);\n"
        "  }\n"
        "  switch (n) {\n",c);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(c,"  case UV_%s: erg = Val_long(%d) ; break;\n",x,neg_result(i));
  }
  fprintf(c,"  default: erg = Val_long(%d) ;\n"
          "  }\n"
          "  return erg;\n"
          "}\n",neg_result(uv_unknown));

  fputs("\n",c);


  fputs("static const int16_t error_rev_map[] = {\n",c);

  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(c," UV_%s,\n",x);
  }
  fputs("};\n",c);

  fputs("\n",c);

  fputs(
  "CAMLprim value\nuwt_strerror(value i)\n{\n"
    "  int t = Long_val(i);\n"
    "  if ( t < 0 ||\n"
    "       t >= (int)(sizeof(error_rev_map) / sizeof(error_rev_map[0]))){\n"
    "     t = UV_UWT_EFATAL;\n"
    "  }\n"
    "  else {\n"
    "     t = error_rev_map[t];\n"
    "  }\n"
    "  const char * msg;\n"
    "  switch(t){\n" ,c );

  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    if ( x[0] == 'U' && x[1] == 'W' && x[2] == 'T' ){
      fprintf(c,"  case UV_%s : msg = \"%s\"; break; \n",
              x, get_er_msg(val));
    }
  }
  fputs("  default: msg = uv_strerror(t);\n  }\n"
        "  return (caml_copy_string(msg));\n"
        "}\n" ,c );

  fputs("type error = \n",ml);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(ml,"| %s\n",x);
  }
  fputs("\n",ml);
  fputs("let err_name = function\n",ml);
  for ( i = 0; i < AR_SIZE(er_map); ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(ml,"| %s -> \"%s\"\n",x,x);
  }
  fputs("\n",ml);

  if ( fclose(c2) | fclose(c) | fclose(ml) ){
    fputs("i/o error\n",stderr);
    return 1;
  }

  ml = fopen("error_val.ml","w");
  c = fopen("error_val.mli","w");
  for ( i = 0 ; i < AR_SIZE(er_map) ; ++i ){
    int val = er_map[i];
    char * x = lower_err_name(val);
    fprintf(ml,"let %s = %d\n",x,neg_result(i));
    fprintf(c,"val %s : int\n",x);
    free(x);
  }
  fputs("\n\nlet int_to_error = function\n",ml);
  for ( i = 0 ; i < AR_SIZE(er_map) ; ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(ml,"| %d -> %s\n",neg_result(i),x);
  }
  fputs("| _ -> EINTR\n\n",ml);

  fputs("let error_to_int = function\n",ml);
  for ( i = 0 ; i < AR_SIZE(er_map) ; ++i ){
    int val = er_map[i];
    const char * x = err_name(val);
    fprintf(ml,"| %s -> %d\n",x,neg_result(i));
  }

  if ( fclose(ml) || fclose(c) ){
    fputs("i/o error\n",stderr);
  }
  return 0;
}
