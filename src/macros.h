#if HAVE_STDBOOL_H
#include <stdbool.h>
#else
#define bool char
#define false 0
#define true 1
#endif

#ifndef HAVE_STATIC_ASSERT
#define _Static_assert4__(COND,MSG) typedef char static_assertion_##MSG[(!!(COND))*2-1]
#define _Static_assert3__(X,L) _Static_assert4__(X,static_assertion_at_line_##L)
#define _Static_assert2__(X,L) _Static_assert3__(X,L)
#define _Static_assert(COND,MSG) _Static_assert2__(COND,__LINE__)
#endif

#ifdef HAVE_BUILTIN_EXPECT
#define likely(x)      __builtin_expect(!!(x), 1)
#define unlikely(x)    __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

#define AR_SIZE(x) ((sizeof (x)) / (sizeof *(x)))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define CEIL(a, b) (((a) / (b)) + (((a) % (b)) > 0 ? 1 : 0))

#define PP_NARG(...)                            \
         PP_NARG_(__VA_ARGS__,PP_RSEQ_N())

#define PP_NARG_(...)                           \
         PP_ARG_N(__VA_ARGS__)

#define PP_ARG_N(                                 \
          _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
         _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
         _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
         _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
         _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
         _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
         _61,_62,_63,N,...) N

#define PP_RSEQ_N() \
         63,62,61,60,                   \
         59,58,57,56,55,54,53,52,51,50, \
         49,48,47,46,45,44,43,42,41,40, \
         39,38,37,36,35,34,33,32,31,30, \
         29,28,27,26,25,24,23,22,21,20, \
         19,18,17,16,15,14,13,12,11,10, \
         9,8,7,6,5,4,3,2,1,0

/* todo: provide an option to disable it */
#define DEBUG_FPRINTF(...)                      \
  fprintf(stderr,__VA_ARGS__)

#define DEBUG_PF_RR_x(s,...)                                            \
  DEBUG_FPRINTF("%s (%s,%u):" s "\n", __func__, __FILE__, __LINE__,__VA_ARGS__)

#define DEBUG_PF_RR_2 DEBUG_PF_RR_x
#define DEBUG_PF_RR_3 DEBUG_PF_RR_x
#define DEBUG_PF_RR_4 DEBUG_PF_RR_x
#define DEBUG_PF_RR_5 DEBUG_PF_RR_x
#define DEBUG_PF_RR_6 DEBUG_PF_RR_x
#define DEBUG_PF_RR_7 DEBUG_PF_RR_x
#define DEBUG_PF_RR_8 DEBUG_PF_RR_x

#define DEBUG_PF_RR_1(s)                                            \
  DEBUG_FPRINTF("%s (%s,%u):" s "\n", __func__, __FILE__, __LINE__)

#define DEBUG_PF_RR(n,...)                      \
  DEBUG_PF_RR_ ## n (__VA_ARGS__)

#define DEBUG_PF_R(n,...)                       \
  DEBUG_PF_RR(n,__VA_ARGS__)

#define DEBUG_PF(...)                           \
  DEBUG_PF_R(PP_NARG(__VA_ARGS__),__VA_ARGS__)

#ifndef R_OK
# ifdef __R_OK
#  define R_OK __R_OK
# elif defined _R_OK
#  define R_OK _R_OK
# else
#  error "R_OK not defined"
# endif
#endif

#ifndef W_OK
# if defined __W_OK
#  define W_OK __W_OK
# elif defined _W_OK
#  define W_OK _W_OK
# else
# error "W_OK not defined"
# endif
#endif

#ifndef X_OK
# if defined __X_OK
#  define X_OK __X_OK
# elif defined _X_OK
#  define X_OK _X_OK
# else
#  ifdef _WIN32
#   define X_OK 4
#  else
#   define X_OK 1
#  endif
# endif
#endif

#ifndef F_OK
# if defined __F_OK
#  define F_OK __F_OK
# elif defined _F_OK
#  define F_OK _F_OK
# else
#  error "F_OK not defined"
# endif
#endif

#ifndef S_IFREG
# if defined __S_IFREG
#  define S_IFREG __S_IFREG
# elif defined _S_IFREG
#  define S_IFREG _S_IFREG
# else
#  error "S_IFREG not defined"
# endif
#endif

#ifndef S_IFDIR
# if defined __S_IFDIR
#  define S_IFDIR __S_IFDIR
# elif defined _S_IFDIR
#  define S_IFDIR _S_IFDIR
# else
#  error "S_IFDIR not defined"
# endif
#endif

#ifndef S_IFCHR
#  if defined __S_IFCHR
#  define S_IFCHR __S_IFCHR
# elif defined _S_IFCHR
#  define S_IFCHR _S_IFCHR
# else
#  define S_IFCHR 0
# endif
#endif

#ifndef S_IFBLK
# if defined __S_IFBLK
#  define S_IFBLK __S_IFBLK
# elif defined _S_IFBLK
#  define S_IFBLK _S_IFBLK
# else
#  define S_IFBLK 0
# endif
#endif

#ifndef S_IFLNK
# if defined __S_IFLNK
#  define S_IFLNK __S_IFLNK
# elif defined _S_IFLNK
#  define S_IFLNK _S_IFLNK
# else
#  define S_IFLNK 0
# endif
#endif

#ifndef S_IFIFO
# if defined __S_IFIFO
#  define S_IFIFO __S_IFIFO
# elif defined  _S_IFIFO
#  define S_IFIFO _S_IFIFO
# else
#  define S_IFIFO 0
# endif
#endif

#ifndef S_IFSOCK
# if defined __S_IFSOCK
#  define S_IFSOCK __S_IFSOCK
# elif defined _S_IFSOCK
#  define S_IFSOCK _S_IFSOCK
# else
#  define S_IFSOCK 0
# endif
#endif

#ifndef S_IFMT
# if defined __S_IFMT
#  define S_IFMT __S_IFMT
# elif defined _SIFMT
#  define S_IFMT _SIFMT
# else
#  error "S_IFMT not defined"
# endif
#endif

#define BYTE_WRAP6(name)                                    \
  CAMLprim value                                            \
  name ## _byte (value *a, int argn)                        \
  {                                                         \
    (void) argn;                                            \
    assert ( argn == 6 );                                   \
    return (name##_native(a[0],a[1],a[2],a[3],a[4],a[5]));  \
  }

#define BYTE_WRAP7(name)                                        \
  CAMLprim value                                                \
  name ## _byte (value *a, int argn)                            \
  {                                                             \
    (void)argn;                                                 \
    assert ( argn == 6 );                                       \
    return (name##_native(a[0],a[1],a[2],a[3],a[4],a[5],a[6])); \
  }

#define INT_TO_POINTER(i) ((void *) (intnat)(i))
#define POINTER_TO_INT(p) ((intnat) (intnat) (p))

#define F_EUNAVAIL2(name)                       \
  CAMLprim value                                \
    uwt_ ## name (value o1, value o2)           \
  {                                             \
    (void) o1;                                  \
    (void) o2;                                  \
    return VAL_RESULT_UV_UWT_EUNAVAIL;          \
  }

static FORCE_INLINE char *
s_strdup (const char *s){
  return (strdup( s == NULL ? "" : s ));
}

static FORCE_INLINE value
s_caml_copy_string_array(const char ** s){
  return ( s == NULL ? Atom(0) : caml_copy_string_array(s) );
}

static FORCE_INLINE value
csafe_copy_string (const char *x){
  return ( x == NULL ? caml_alloc_string(0) : caml_copy_string(x) );
}

#if !defined(HAVE_DECL_STRNLEN) || HAVE_DECL_STRNLEN != 1
static size_t
strnlen(const char *s, size_t maxlen)
{
  size_t i;
  for ( i = 0 ; i < maxlen && *s != '\0'; i++ ){
    s++;
  }
  return i;
}
#endif

#ifndef HAVE_STRDUP
static char *
strdup (const char *s)
{
  size_t len = strlen(s) + 1;
  void *n = malloc(len);
  if (n == NULL)
    return NULL;
  memcpy(n,s,len);
  return n;
}
#endif
