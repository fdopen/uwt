/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_common.h"
#include "map_error.h"

/* common helper functions
   and everything independend of uv_req_t, uv_handle_t, uv_loop_t and
   memory handling  */

int uwt_is_safe_string (value str)
{
  return ( caml_string_length(str) == strlen(String_val(str)) );
}

UWT_LOCAL int
uwt__safe_convert_flag_list(value list, const int flags[],size_t flags_size)
{
  int res;
  res = 0;
  while ( list != Val_int(0) ){
    const intnat pos = Long_val(Field(list, 0));
    list = Field(list, 1);
    if ( pos < 0 || (size_t)pos >= flags_size ){
      assert(0);
    }
    else {
      res |= flags[pos];
    }
  }
  return res;
}

UWT_LOCAL value
uwt__safe_rev_convert_flag_list(int res, const int flags[],size_t flags_size)
{
  CAMLparam0();
  CAMLlocal1(l);
  size_t i;
  l = Val_unit;
  for ( i = flags_size; i != 0 ; ){
    --i;
    if ( res & flags[i] ){
      value tmp = caml_alloc_small(2,0);
      Field(tmp,0) = Val_long(i);
      Field(tmp,1) = l;
      l = tmp;
    }
  }
  CAMLreturn(l);
}

UWT_LOCAL value
uwt__alloc_sockaddr(const struct sockaddr *saddr)
{
  value res;
  if ( saddr == NULL ){
    return Val_unit;
  }
  switch (saddr->sa_family) {
#ifndef _WIN32
  case AF_UNIX:
    {
      const struct sockaddr_un * addr = (const struct sockaddr_un *)saddr;
      const size_t max_len = sizeof(struct sockaddr_un) -
        offsetof(struct sockaddr_un, sun_path);
      _Static_assert(sizeof(struct sockaddr_un) -
                     offsetof(struct sockaddr_un, sun_path) >= sizeof addr->sun_path,
                     "sun path length");
      const size_t len = strnlen(addr->sun_path,max_len);
      value str = caml_alloc_string(len);
      memcpy(String_val(str),addr->sun_path,len);
      Begin_roots1(str);
      res = caml_alloc_small(1,0);
      Field(res,0) = str;
      End_roots();
      break;
    }
#endif
  case AF_INET:
    {
      const struct sockaddr_in * addr = (const struct sockaddr_in *)saddr;
      value a = caml_alloc_string(4);
      memcpy(String_val(a), &addr->sin_addr, 4);
      Begin_root (a);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(addr->sin_port));
      End_roots();
      break;
    }
  case AF_INET6:
    {
      const struct sockaddr_in6 * addr = (const struct sockaddr_in6 *) saddr;
      value a = caml_alloc_string(16);
      /* we can't use alloc_inet_addr, because OCaml and libuv
         sometimes have different opinions about ipv6 support.
         alloc_inet6_addr is not always available */
      memcpy(String_val(a), &addr->sin6_addr, 16);
      Begin_root (a);
      res = caml_alloc_small(2, 1);
      Field(res,0) = a;
      Field(res,1) = Val_int(ntohs(addr->sin6_port));
      End_roots();
      break;
    }
  default:
    {
      res = Val_unit;
    }
  }
  return res;
}

#ifndef GET_INET6_ADDR
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))
#endif

UWT_LOCAL bool
uwt__get_sockaddr(value o_addr,struct sockaddr *saddr)
{
  switch(Tag_val(o_addr)) {
#ifndef _WIN32
  case 0:
    {
      struct sockaddr_un * addr = (struct sockaddr_un *)saddr;
      value path;
      mlsize_t len;
      path = Field(o_addr, 0);
      len = caml_string_length(path); /* not yet used. abstract sockets?! */
      if (len >= sizeof(addr->sun_path)) {
        return false;
      }
      memset(addr, 0, sizeof(struct sockaddr_un));
      addr->sun_family = AF_UNIX;
      memmove (addr->sun_path, String_val(path), len + 1);
      return true;
    }
#endif
  case 1:                       /* ADDR_INET */
    if ( caml_string_length(Field(o_addr, 0)) == 4 ) {
      struct sockaddr_in * addr = (struct sockaddr_in *)saddr;
      memset(addr, 0, sizeof(struct sockaddr_in));
      addr->sin_family = AF_INET;
      addr->sin_addr = GET_INET_ADDR(Field(o_addr, 0));
      addr->sin_port = htons(Int_val(Field(o_addr, 1)));
#ifdef HAVE_SIN_LEN
      addr->sin_len = sizeof(struct sockaddr_in);
#endif
      return true;
    }
    else {
      struct sockaddr_in6 * addr = (struct sockaddr_in6 *)saddr;
      memset(addr, 0, sizeof(struct sockaddr_in6));
      addr->sin6_family = AF_INET6;
      addr->sin6_addr = GET_INET6_ADDR(Field(o_addr, 0));
      addr->sin6_port = htons(Int_val(Field(o_addr, 1)));
#ifdef HAVE_SIN6_LEN
      addr->sin6_len = sizeof(struct sockaddr_in6);
#endif
      return true;
    }
  default:
    return false;
  }
}

UWT_LOCAL value
uwt__alloc_eresult(val_uwt_error_t er)
{
  value x = caml_alloc_small(1,Error_tag);
  Field(x,0) = er;
  return x;
}

#ifdef _WIN32
UWT_LOCAL int
uwt__convert_signal_number(int signum)
{
  switch( signum ){
  case -7: return SIGKILL;
  case -11: return SIGTERM;
  case -6: return SIGINT;
  case -4: return SIGHUP;
  case -50: return SIGBREAK;
  case -51: return SIGWINCH;
  default: return caml_convert_signal_number(signum);
  }
}
UWT_LOCAL int
uwt__rev_convert_signal_number(int signum)
{
  switch( signum ){
  case SIGKILL: return -7;
  case SIGTERM: return -11;
  case SIGINT: return -6;
  case SIGHUP: return -4;
  case SIGBREAK: return -50;
  case SIGWINCH: return -51;
  default: return caml_rev_convert_signal_number(signum);
  }
}

UWT_LOCAL bool
uwt__set_crt_fd(value o_fd)
{
  bool ret = true;
  if ( CRT_fd_val(o_fd) == NO_CRT_FD ){
    const int fd = _open_osfhandle((intptr_t)Handle_val(o_fd), _O_BINARY);
    if ( fd == -1 ){
      ret = false;
    }
    else {
      CRT_fd_val(o_fd) = fd;
    }
  }
  return ret;
}
#endif

UWT_LOCAL value
uwt__stat_to_value(const uv_stat_t * sb)
{
  CAMLparam0();
  CAMLlocal5(atime,omtime,octime,btime,size);
  value v;
  value s;

  size = caml_copy_int64(sb->st_size);
  atime = caml_copy_int64(sb->st_atim.tv_sec);
  omtime = caml_copy_int64(sb->st_mtim.tv_sec);
  octime = caml_copy_int64(sb->st_ctim.tv_sec);
  btime = caml_copy_int64(sb->st_birthtim.tv_sec);

  switch ( sb->st_mode & S_IFMT ){
  case S_IFREG: v = Val_long(0); break;
  case S_IFDIR: v = Val_long(1); break;
  case S_IFCHR: v = Val_long(2); break;
  case S_IFBLK: v = Val_long(3); break;
  case S_IFLNK: v = Val_long(4); break;
  case S_IFIFO: v = Val_long(5); break;
  case S_IFSOCK: v = Val_long(6); break;
  default: v = Val_long(7);
  }
  s = caml_alloc_small(21,0);
  Field(s,1) = v;
  Field(s,2) = Val_long(sb->st_mode & 07777);

#define SET_INT(n,f)                            \
  Field(s,n) = Val_long(sb->f)
  SET_INT(0,st_dev);
  SET_INT(3,st_nlink);
  SET_INT(4,st_uid);
  SET_INT(5,st_gid);
  SET_INT(6,st_rdev);
  SET_INT(7,st_ino);
  SET_INT(9,st_blksize);
  SET_INT(10,st_blocks);
  SET_INT(11,st_flags);
  SET_INT(12,st_gen);

  SET_INT(14,st_atim.tv_nsec);
  SET_INT(16,st_mtim.tv_nsec);
  SET_INT(18,st_ctim.tv_nsec);
  SET_INT(20,st_birthtim.tv_nsec);
#undef SET_INT

  Field(s,8) = size;
  Field(s,13) = atime;
  Field(s,15) = omtime;
  Field(s,17) = octime;
  Field(s,19) = btime;

  CAMLreturn(s);
}

#ifdef _WIN32
#if defined(HAVE_UV_TRANSLATE_SYSERROR) && (!defined(HAVE_DECL_UV_TRANSLATE_SYS_ERROR) || HAVE_DECL_UV_TRANSLATE_SYS_ERROR == 0)
extern int uv_translate_sys_error(int);
#endif
/* mapping is arbitrary under Windows, -errno doesn't work */
int
uwt_translate_errno(int x)
{
  switch ( x ) {
  case ENOEXEC: /* fall */
  case ENOENT: return UV_ENOENT;
  case E2BIG: return UV_E2BIG;
  case EACCES: return UV_EACCES;
  case EAGAIN: return UV_EAGAIN;
  case EBADF: return UV_EBADF;
  case EBUSY: return UV_EBUSY;
  case EEXIST: return UV_EEXIST;
  case EFAULT: return UV_EFAULT;
  case EFBIG: return UV_EFBIG;
  case EINTR: return UV_EINTR;
  case EINVAL: return UV_EINVAL;
  case EIO: return UV_EIO;
  case EISDIR: return UV_EISDIR;
  case EMFILE: return UV_EMFILE;
  case EMLINK: return UV_EMLINK;
  case ENAMETOOLONG: return UV_ENAMETOOLONG;
  case ENFILE: return UV_ENFILE;
  case ENODEV: return UV_ENODEV;
  case ENOMEM: return UV_ENOMEM;
  case ENOSPC: return UV_ENOSPC;
  case ENOSYS: return UV_ENOSYS;
  case ENOTDIR: return UV_ENOTDIR;
  case ENOTEMPTY: return UV_ENOTEMPTY;
  case ENXIO: return UV_ENXIO;
  case EPERM: return UV_EPERM;
  case EPIPE: return UV_EPIPE;
  case ERANGE: return UV_ERANGE;
  case EROFS: return UV_EROFS;
  case ESPIPE: return UV_ESPIPE;
  case ESRCH: return UV_ESRCH;
  case EXDEV: return UV_EXDEV;
  default:
    return UV_UNKNOWN;
  }
}

int
uwt_translate_sys_error(DWORD sys_errno)
{
  if ( sys_errno == 0 || sys_errno > INT_MAX ){
    return UV_UNKNOWN;
  }

#if HAVE_DECL_UV_TRANSLATE_SYS_ERROR
  {
    int x = uv_translate_sys_error(sys_errno);
    if ( x != UV_UNKNOWN ){
      return x;
    }
  }
#endif

  switch ( sys_errno ){
  /* translations from libuv */
  case ERROR_NOACCESS:                    return UV_EACCES;
  case WSAEACCES:                         return UV_EACCES;
#ifdef ERROR_ELEVATION_REQUIRED
  case ERROR_ELEVATION_REQUIRED:          return UV_EACCES;
#else
  case 740:                               return UV_EACCES;
#endif
  case ERROR_ADDRESS_ALREADY_ASSOCIATED:  return UV_EADDRINUSE;
  case WSAEADDRINUSE:                     return UV_EADDRINUSE;
  case WSAEADDRNOTAVAIL:                  return UV_EADDRNOTAVAIL;
  case WSAEAFNOSUPPORT:                   return UV_EAFNOSUPPORT;
  case WSAEWOULDBLOCK:                    return UV_EAGAIN;
  case WSAEALREADY:                       return UV_EALREADY;
  case ERROR_INVALID_FLAGS:               return UV_EBADF;
  case ERROR_INVALID_HANDLE:              return UV_EBADF;
  case ERROR_LOCK_VIOLATION:              return UV_EBUSY;
  case ERROR_PIPE_BUSY:                   return UV_EBUSY;
  case ERROR_SHARING_VIOLATION:           return UV_EBUSY;
  case ERROR_OPERATION_ABORTED:           return UV_ECANCELED;
  case WSAEINTR:                          return UV_ECANCELED;
  case ERROR_NO_UNICODE_TRANSLATION:      return UV_ECHARSET;
  case ERROR_CONNECTION_ABORTED:          return UV_ECONNABORTED;
  case WSAECONNABORTED:                   return UV_ECONNABORTED;
  case ERROR_CONNECTION_REFUSED:          return UV_ECONNREFUSED;
  case WSAECONNREFUSED:                   return UV_ECONNREFUSED;
  case ERROR_NETNAME_DELETED:             return UV_ECONNRESET;
  case WSAECONNRESET:                     return UV_ECONNRESET;
  case ERROR_ALREADY_EXISTS:              return UV_EEXIST;
  case ERROR_FILE_EXISTS:                 return UV_EEXIST;
  case ERROR_BUFFER_OVERFLOW:             return UV_EFAULT;
  case WSAEFAULT:                         return UV_EFAULT;
  case ERROR_HOST_UNREACHABLE:            return UV_EHOSTUNREACH;
  case WSAEHOSTUNREACH:                   return UV_EHOSTUNREACH;
  case ERROR_INSUFFICIENT_BUFFER:         return UV_EINVAL;
  case ERROR_INVALID_DATA:                return UV_EINVAL;
  case ERROR_INVALID_PARAMETER:           return UV_EINVAL;
#ifdef ERROR_SYMLINK_NOT_SUPPORTED
  case ERROR_SYMLINK_NOT_SUPPORTED:       return UV_EINVAL;
#endif
  case WSAEINVAL:                         return UV_EINVAL;
  case WSAEPFNOSUPPORT:                   return UV_EINVAL;
  case WSAESOCKTNOSUPPORT:                return UV_EINVAL;
  case ERROR_BEGINNING_OF_MEDIA:          return UV_EIO;
  case ERROR_BUS_RESET:                   return UV_EIO;
  case ERROR_CRC:                         return UV_EIO;
  case ERROR_DEVICE_DOOR_OPEN:            return UV_EIO;
  case ERROR_DEVICE_REQUIRES_CLEANING:    return UV_EIO;
  case ERROR_DISK_CORRUPT:                return UV_EIO;
  case ERROR_EOM_OVERFLOW:                return UV_EIO;
  case ERROR_FILEMARK_DETECTED:           return UV_EIO;
  case ERROR_GEN_FAILURE:                 return UV_EIO;
  case ERROR_INVALID_BLOCK_LENGTH:        return UV_EIO;
  case ERROR_IO_DEVICE:                   return UV_EIO;
  case ERROR_NO_DATA_DETECTED:            return UV_EIO;
  case ERROR_NO_SIGNAL_SENT:              return UV_EIO;
  case ERROR_OPEN_FAILED:                 return UV_EIO;
  case ERROR_SETMARK_DETECTED:            return UV_EIO;
  case ERROR_SIGNAL_REFUSED:              return UV_EIO;
  case WSAEISCONN:                        return UV_EISCONN;
  case ERROR_CANT_RESOLVE_FILENAME:       return UV_ELOOP;
  case ERROR_TOO_MANY_OPEN_FILES:         return UV_EMFILE;
  case WSAEMFILE:                         return UV_EMFILE;
  case WSAEMSGSIZE:                       return UV_EMSGSIZE;
  case ERROR_FILENAME_EXCED_RANGE:        return UV_ENAMETOOLONG;
  case ERROR_NETWORK_UNREACHABLE:         return UV_ENETUNREACH;
  case WSAENETUNREACH:                    return UV_ENETUNREACH;
  case WSAENOBUFS:                        return UV_ENOBUFS;
  case ERROR_DIRECTORY:                   return UV_ENOENT;
  case ERROR_FILE_NOT_FOUND:              return UV_ENOENT;
  case ERROR_INVALID_NAME:                return UV_ENOENT;
  case ERROR_INVALID_DRIVE:               return UV_ENOENT;
  case ERROR_INVALID_REPARSE_DATA:        return UV_ENOENT;
  case ERROR_MOD_NOT_FOUND:               return UV_ENOENT;
  case ERROR_PATH_NOT_FOUND:              return UV_ENOENT;
  case WSAHOST_NOT_FOUND:                 return UV_ENOENT;
  case WSANO_DATA:                        return UV_ENOENT;
  case ERROR_NOT_ENOUGH_MEMORY:           return UV_ENOMEM;
  case ERROR_OUTOFMEMORY:                 return UV_ENOMEM;
  case ERROR_CANNOT_MAKE:                 return UV_ENOSPC;
  case ERROR_DISK_FULL:                   return UV_ENOSPC;
  case ERROR_EA_TABLE_FULL:               return UV_ENOSPC;
  case ERROR_END_OF_MEDIA:                return UV_ENOSPC;
  case ERROR_HANDLE_DISK_FULL:            return UV_ENOSPC;
  case ERROR_NOT_CONNECTED:               return UV_ENOTCONN;
  case WSAENOTCONN:                       return UV_ENOTCONN;
  case ERROR_DIR_NOT_EMPTY:               return UV_ENOTEMPTY;
  case WSAENOTSOCK:                       return UV_ENOTSOCK;
  case ERROR_NOT_SUPPORTED:               return UV_ENOTSUP;
  case ERROR_BROKEN_PIPE:                 return UV_EOF;
  case ERROR_ACCESS_DENIED:               return UV_EPERM;
  case ERROR_PRIVILEGE_NOT_HELD:          return UV_EPERM;
  case ERROR_BAD_PIPE:                    return UV_EPIPE;
  case ERROR_NO_DATA:                     return UV_EPIPE;
  case ERROR_PIPE_NOT_CONNECTED:          return UV_EPIPE;
  case WSAESHUTDOWN:                      return UV_EPIPE;
  case WSAEPROTONOSUPPORT:                return UV_EPROTONOSUPPORT;
  case ERROR_WRITE_PROTECT:               return UV_EROFS;
  case ERROR_SEM_TIMEOUT:                 return UV_ETIMEDOUT;
  case WSAETIMEDOUT:                      return UV_ETIMEDOUT;
  case ERROR_NOT_SAME_DEVICE:             return UV_EXDEV;
  case ERROR_INVALID_FUNCTION:            return UV_EISDIR;
  case ERROR_META_EXPANSION_TOO_LONG:     return UV_E2BIG;

  /* imported from ocaml */
  case ERROR_ARENA_TRASHED:               return UV_ENOMEM;
  case ERROR_INVALID_BLOCK:               return UV_ENOMEM;
  case ERROR_BAD_ENVIRONMENT:             return UV_E2BIG;
  case ERROR_INVALID_ACCESS:              return UV_EINVAL;
  case ERROR_CURRENT_DIRECTORY:           return UV_EACCES;
  case ERROR_NO_MORE_FILES:               return UV_ENOENT;
  case ERROR_BAD_NETPATH:                 return UV_ENOENT;
  case ERROR_NETWORK_ACCESS_DENIED:       return UV_EACCES;
  case ERROR_BAD_NET_NAME:                return UV_ENOENT;
  case ERROR_FAIL_I24:                    return UV_EACCES;
  case ERROR_NO_PROC_SLOTS:               return UV_EAGAIN;
  case ERROR_DRIVE_LOCKED:                return UV_EACCES;
  case ERROR_INVALID_TARGET_HANDLE:       return UV_EBADF;
  case ERROR_DIRECT_ACCESS_HANDLE:        return UV_EBADF;
  case ERROR_NEGATIVE_SEEK:               return UV_EINVAL;
  case ERROR_SEEK_ON_DEVICE:              return UV_EACCES;
  case ERROR_NOT_LOCKED:                  return UV_EACCES;
  case ERROR_BAD_PATHNAME:                return UV_ENOENT;
  case ERROR_MAX_THRDS_REACHED:           return UV_EAGAIN;
  case ERROR_LOCK_FAILED:                 return UV_EACCES;
  case ERROR_NOT_ENOUGH_QUOTA:            return UV_ENOMEM;
#ifdef WSANAMETOOLONG
  case WSANAMETOOLONG:                    return UV_ENAMETOOLONG;
#endif
#ifdef WSAENFILE
  case WSAENFILE:                         return UV_ENFILE;
#endif
  case WSAENOTEMPTY:                      return UV_ENOTEMPTY;
  default:
    {
#if defined(HAVE_UV_TRANSLATE_SYSERROR) && !HAVE_DECL_UV_TRANSLATE_SYS_ERROR
      return (uv_translate_sys_error(sys_errno));
#else
      return UV_UNKNOWN;
#endif
    }
  }
}

char *
uwt_utf16_to_utf8(const WCHAR* utf16_buffer, int * error)
{
  char * utf8_buffer;
  int utf8_len;
  if ( utf16_buffer == NULL ){
    *error = UV_EINVAL;
    return NULL;
  }
  utf8_len = WideCharToMultiByte(CP_UTF8,0,utf16_buffer,-1,NULL,0,NULL,NULL);
  if ( utf8_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    return NULL;
  }
  utf8_buffer = malloc(utf8_len+1);
  if ( utf8_buffer == NULL ){
    *error = UV_ENOMEM;
    return NULL;
  }
  utf8_len = WideCharToMultiByte(CP_UTF8,0,utf16_buffer,-1,utf8_buffer,utf8_len,
                                 NULL,NULL);
  if ( utf8_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    free(utf8_buffer);
    return NULL;
  }
  return utf8_buffer;
}

WCHAR*
uwt_utf8_to_utf16(const char* utf8_buffer,int *error){
  WCHAR * utf16_buffer;
  int utf16_len;
  if ( utf8_buffer == NULL ){
    *error = UV_EINVAL;
    return NULL;
  }
  utf16_len = MultiByteToWideChar(CP_UTF8,0,utf8_buffer,-1,NULL,0);
  if ( utf16_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    return NULL;
  }
  utf16_buffer = malloc(sizeof(WCHAR) * utf16_len);
  if ( utf16_buffer == NULL ){
    *error = UV_ENOMEM;
    return NULL;
  }
  utf16_len = MultiByteToWideChar(CP_UTF8,0,utf8_buffer,-1,utf16_buffer,
                                  utf16_len);
  if ( utf16_len == 0 ){
    *error = uwt_translate_sys_error(GetLastError());
    free(utf16_buffer);
    return NULL;
  }
  return utf16_buffer;
}
#endif /* #ifdef _WIN32 */
