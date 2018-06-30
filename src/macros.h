/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#ifndef __UWT_MACROS_H
#define __UWT_MACROS_H

#ifdef __cplusplus
extern "C" {
#endif


#if HAVE_STDBOOL_H
#include <stdbool.h>
#else
#define bool char
#define false 0
#define true 1
#endif

#ifndef HAVE_STATIC_ASSERT
#define _Static_assert5__(COND,MSG) typedef char static_assertion##MSG[(!!(COND))*2-1]
#ifdef __COUNTER__
#define _Static_assert4__(X,C,L) _Static_assert5__(X,C##_at_line_##L)
#define _Static_assert3__(X,C,L) _Static_assert4__(X,C,L)
#define _Static_assert2__(X,L) _Static_assert3__(X,__COUNTER__,L)
#else
#define _Static_assert3__(X,L) _Static_assert5__(X,_at_line_##L)
#define _Static_assert2__(X,L) _Static_assert3__(X,L)
#endif
#define _Static_assert(COND,MSG) _Static_assert2__(COND,__LINE__)
#endif

#ifdef HAVE_BUILTIN_EXPECT
#define likely(x)      __builtin_expect(!!(x), 1)
#define unlikely(x)    __builtin_expect(!!(x), 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

#define BUILD_ASSERT_OR_ZERO(cond)              \
  (sizeof(char [1 - 2*!(cond)]) - 1)

#if HAVE_BUILTIN_TYPES_COMPATIBLE_P
/* &arr[0] degrades to a pointer: a different type from an array */
#define _array_size_chk(arr) \
 BUILD_ASSERT_OR_ZERO(!__builtin_types_compatible_p(typeof(arr), \
                                                    typeof(&(arr)[0])))
#else
#define _array_size_chk(arr) 0
#endif
#define AR_SIZE(x) (sizeof(x) / sizeof((x)[0]) + _array_size_chk(x))

#define UMIN_M(x, y) (((x) < (y)) ? (x) : (y))
#define UMAX_M(x, y) (((x) > (y)) ? (x) : (y))

#if defined(HAVE_TYPEOF) && defined(HAVE_STATEMENT_EXPRESSIONS)
#define UMAX_REAL2(a,b,L)                       \
  ({ typeof (a) _a##L = (a);                    \
    typeof (b) _b##L = (b);                     \
    _a##L > _b##L ?                             \
      _a##L : _b##L ; })
#define UMAX_REAL(a,b,c)                        \
  UMAX_REAL2(a,b,c)
#ifdef __COUNTER__
#define UMAX(a,b)                               \
  UMAX_REAL(a,b,__COUNTER__)
#else
#define UMAX(a,b)                               \
  UMAX_REAL(a,b,__LINE__)
#endif
#define UMIN_REAL2(a,b,L)                       \
  ({ typeof (a) _c##L = (a);                    \
    typeof (b) _d##L = (b);                     \
    _c##L < _d##L ?                             \
      _c##L : _d##L ; })
#define UMIN_REAL(a,b,c)                        \
  UMIN_REAL2(a,b,c)
#ifdef __COUNTER__
#define UMIN(a,b)                               \
  UMIN_REAL(a,b,__COUNTER__)
#else
#define UMIN(a,b)                               \
  UMIN_REAL(a,b,__LINE__)
#endif
#else
#define UMIN UMIN_M
#define UMAX UMAX_M
#endif

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

#if (!defined(UWT_DEBUG) || !UWT_DEBUG) || defined(_MSC_VER)
#define DEBUG_PF(...)
#else
#define DEBUG_PF(...)                           \
  DEBUG_PF_R(PP_NARG(__VA_ARGS__),__VA_ARGS__)
#endif

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
#  define S_IFIFO (S_IFREG | S_IFCHR)
# endif
#endif

#ifndef S_IFSOCK
# if defined __S_IFSOCK
#  define S_IFSOCK __S_IFSOCK
# elif defined _S_IFSOCK
#  define S_IFSOCK _S_IFSOCK
# else
#  define S_IFSOCK (S_IFDIR | S_IFCHR)
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

#define F_ENOSYS2(name)                         \
  CAMLprim value                                \
    uwt_ ## name (value o1, value o2)           \
  {                                             \
    (void) o1;                                  \
    (void) o2;                                  \
    return VAL_UWT_INT_RESULT_ENOSYS;           \
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
s_caml_copy_string(const char *x){
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

#ifndef _WIN32
#define ALLOC_PATH_LEN_MAX 131072
#define ALLOC_PATH_LEN_MIN  16384

#if defined(PATH_MAX) && PATH_MAX > 0 && ALLOC_PATH_LEN_MIN < PATH_MAX
#define ALLOCA_PATH_LEN \
  ((size_t)(UMIN_M(ALLOC_PATH_LEN_MAX,(UMAX_M(PATH_MAX + 1,ALLOCA_SIZE)))))
#elif defined(MAXPATHLEN) && MAXPATHLEN > 0 && ALLOC_PATH_LEN_MIN < MAXPATHLEN
#define ALLOCA_PATH_LEN \
  ((size_t)(UMIN_M(ALLOC_PATH_LEN_MAX,UMAX_M(MAXPATHLEN + 1,ALLOCA_SIZE))))
#elif defined(NAME_MAX) && NAME_MAX > 0 && ALLOC_PATH_LEN_MIN < NAME_MAX
#define ALLOCA_PATH_LEN \
  ((size_t)(UMIN_M(ALLOC_PATH_LEN_MAX,UMAX_M(NAME_MAX + 1,ALLOCA_SIZE))))
#else
#define ALLOCA_PATH_LEN ALLOC_PATH_LEN_MIN
#endif

#else /* ifndef _WIN32 */
#define ALLOCA_PATH_LEN ((size_t)(UMAX_M(32767 + 17,MAX_PATH + 17)))
#endif

#define INT_VAL_RET_IR_EINVAL(l,b)                          \
  int l;                                                    \
  do {                                                      \
    const intnat l_real = Long_val(b);                      \
    if (unlikely( l_real < INT_MIN || l_real > INT_MAX )){  \
      return VAL_UWT_INT_RESULT_EINVAL;                     \
    }                                                       \
    l = l_real;                                             \
  } while (0)

#define INT_VAL_RET_WRAP_EINVAL(l,b)                        \
  int l;                                                    \
  do {                                                      \
    const intnat l_real = Long_val(b);                      \
    if (unlikely( l_real < INT_MIN || l_real > INT_MAX )){  \
      return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);      \
    }                                                       \
    l = l_real;                                             \
  } while (0)

#define UINT_VAL_RET_IR_EINVAL(l,b)                             \
  unsigned int l;                                               \
  do {                                                          \
    const intnat l_real = Long_val(b);                          \
    if (unlikely( l_real < 0 || (uintnat)l_real > UINT_MAX )){  \
      return VAL_UWT_INT_RESULT_EINVAL;                         \
    }                                                           \
    l = l_real;                                                 \
  } while (0)

#define UINT_VAL_RET_WRAP_EINVAL(l,b)                           \
  unsigned int l;                                               \
  do {                                                          \
    const intnat l_real = Long_val(b);                          \
    if (unlikely( l_real < 0 || (uintnat)l_real > UINT_MAX )){  \
      return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);          \
    }                                                           \
    l = l_real;                                                 \
  } while (0)

#define UNTAG_POINTER(v)                        \
  ((void *) ((uintptr_t)(v) ^ (uintptr_t)1))

#define TAG_POINTER(v)                          \
  ((intnat)(((uintptr_t)(v) | (uintptr_t)1)))

#if defined(__clang__)

#define DISABLE_WARNING_TYPE_LIMIT()                    \
  _Pragma("clang diagnostic push")                      \
  _Pragma("clang diagnostic ignored \"-Wtype-limits\"")
#define DISABLE_WARNING_CAST_QUAL()                     \
  _Pragma("clang diagnostic push")                      \
  _Pragma("clang diagnostic ignored \"-Wcast-qual\"")
#define DISABLE_WARNING_ENUM_COMPARE()                  \
  _Pragma("clang diagnostic push")                      \
  _Pragma("clang diagnostic ignored \"-Wenum-compare\"")
#define POP_WARNING()                           \
  _Pragma("clang diagnostic pop")

#elif defined(__GNUC__) && ( __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6))

#define DISABLE_WARNING_TYPE_LIMIT()                                \
  _Pragma("GCC diagnostic push")                                    \
  _Pragma("GCC diagnostic ignored \"-Wtype-limits\"")
#define DISABLE_WARNING_CAST_QUAL()                                 \
  _Pragma("GCC diagnostic push")                                    \
  _Pragma("GCC diagnostic ignored \"-Wcast-qual\"")
#define DISABLE_WARNING_ENUM_COMPARE()                              \
  _Pragma("GCC diagnostic push")                                    \
  _Pragma("GCC diagnostic ignored \"-Wenum-compare\"")
#define POP_WARNING()                           \
  _Pragma("GCC diagnostic pop")

#else

#define DISABLE_WARNING_TYPE_LIMIT()
#define DISABLE_WARNING_CAST_QUAL()
#define DISABLE_WARNING_ENUM_COMPARE()
#define POP_WARNING()


#endif

#ifdef __cplusplus
}
#endif

#endif /* __UWT_MACROS_H */
