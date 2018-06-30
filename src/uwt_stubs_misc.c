/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_misc.h"

CAMLprim value
uwt_guess_handle_na(value o_fd)
{
  int t = UV_UNKNOWN_HANDLE;
#ifndef _WIN32
  t = uv_guess_handle(FD_VAL(o_fd));
#else
  /* uv_guess_handle works with crt file descriptors */
  if ( Descr_kind_val(o_fd) == KIND_HANDLE ){
    HANDLE handle = OCAML_Handle_val(o_fd);
    DWORD mode;
    switch (GetFileType(handle)) {
    case FILE_TYPE_CHAR:
      if (GetConsoleMode(handle, &mode)) {
        t = UV_TTY;
      }
      else {
        t = UV_FILE;
      }
      break;
    case FILE_TYPE_PIPE:
      t = UV_NAMED_PIPE;
      break;
    case FILE_TYPE_DISK:
      t = UV_FILE;
      break;
    }
  }
  else {
    if ( Descr_kind_val(o_fd) == KIND_SOCKET ){
      SOCKET handle = Socket_val(o_fd);
      WSAPROTOCOL_INFO pi;
      int size = sizeof(pi);
      if ( getsockopt(handle, SOL_SOCKET, SO_PROTOCOL_INFO,
                      (char *)&pi, &size) == 0 ){
        if ( pi.iAddressFamily == AF_INET || pi.iAddressFamily == AF_INET6 ){
          if ( pi.iSocketType == SOCK_STREAM ){
            t = UV_TCP;
          }
          else {
            if ( pi.iSocketType == SOCK_DGRAM ){
              t = UV_UDP;
            }
          }
        }
      }
    }
  }
#endif
  switch ( t ){
  case UV_FILE: return (Val_long(0));
  case UV_TTY: return (Val_long(1));
  case UV_NAMED_PIPE: return (Val_long(2));
  case UV_TCP: return (Val_long(3));
  case UV_UDP: return (Val_long(4));
  default: /* fall */
  case UV_UNKNOWN_HANDLE:
    return (Val_long(5));
  }
}

CAMLprim value
uwt_version_na(value unit)
{
  (void)unit;
  return (Val_long(uv_version()));
}

CAMLprim value
uwt_version_string(value unit)
{
  (void)unit;
  return (s_caml_copy_string(uv_version_string()));
}

CAMLprim value
uwt_resident_set_memory(value unit)
{
  (void)unit;
  value res;
  size_t rss;
  int r = uv_resident_set_memory(&rss);
  if ( r >= 0 ){
    value x = caml_copy_int64(rss);
    Begin_roots1(x);
    res = caml_alloc_small(1,Ok_tag);
    Field(res,0) = x;
    End_roots();
  }
  else {
    res = caml_alloc_small(1,Error_tag);
    Field(res,0) = Val_uwt_error(r);
  }
  return res;
}

CAMLprim value
uwt_uptime(value unit)
{
 value res;
 double up;
 int r = uv_uptime(&up);
 (void)unit;
 if ( r >= 0 ){
   value x = caml_copy_double(up);
   Begin_roots1(x);
   res = caml_alloc_small(1,Ok_tag);
   Field(res,0) = x;
   End_roots();
 }
 else {
   res = caml_alloc_small(1,Error_tag);
   Field(res,0) = Val_uwt_error(r);
 }
 return res;
}

typedef int(*ipx_addr)(const char *, int, void*);
static value
uwt_ipx_addr(value o_str,value o_port, ipx_addr func)
{
  INT_VAL_RET_WRAP_EINVAL(port, o_port);
  if ( !uwt_is_safe_string(o_str) ){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ECHARSET);
  }
  value ret;
  struct sockaddr_storage addr;
  const int r = func(String_val(o_str), port, &addr);
  if ( r < 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(r);
  }
  else {
    value so = uwt__alloc_sockaddr((struct sockaddr *)&addr);
    if ( so == Val_unit ){
      ret = caml_alloc_small(1,Error_tag);
      Field(ret,0) = VAL_UWT_ERROR_UNKNOWN;
    }
    else {
      Begin_roots1(so);
      ret = caml_alloc_small(1,Ok_tag);
      Field(ret,0) = so;
      End_roots();
    }
  }
  return ret;
}

typedef int (*ipx_name)(void*, char*, size_t);
static value
uwt_ipx_name(value o_sock, ipx_name func)
{
  struct sockaddr_storage addr;
  if ( !uwt__get_sockaddr(o_sock,(struct sockaddr *) &addr) ){
    return uwt__alloc_eresult(VAL_UWT_ERROR_UNKNOWN);
  }
  enum { init_size = 47 };
  value ret;
  char dst[init_size];
  const int r = func(&addr , dst, init_size);
  if ( r < 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(r);
  }
  else {
    value os = caml_copy_string(dst);
    Begin_roots1(os);
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = os;
    End_roots();
  }
  return ret;
}

CAMLprim value uwt_ip6_addr(value o_str,value o_port)
{
  return uwt_ipx_addr(o_str,o_port,(ipx_addr)uv_ip6_addr);
}

CAMLprim value uwt_ip4_addr(value o_str,value o_port)
{
  return uwt_ipx_addr(o_str,o_port,(ipx_addr)uv_ip4_addr);
}

CAMLprim value uwt_ip6_name(value o_sock)
{
  return uwt_ipx_name(o_sock,(ipx_name)uv_ip6_name);
}

CAMLprim value uwt_ip4_name(value o_sock)
{
  return uwt_ipx_name(o_sock,(ipx_name)uv_ip4_name);
}

CAMLprim value
uwt_getrusage(value unit)
{
  CAMLparam0();
  CAMLlocal2(ar,tup);
  uv_rusage_t u;
  int r = uv_getrusage(&u);
  uint8_t tag;
  (void) unit;
  if ( r < 0 ){
    ar = Val_uwt_error(r);
    tag = Error_tag;
  }
  else {
    tag = Ok_tag;
    ar = caml_alloc(16,0);
    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(u.ru_utime.tv_sec);
    Field(tup,1) = Val_long(u.ru_utime.tv_usec);
    Store_field(ar,0,tup);

    tup = caml_alloc_small(2,0);
    Field(tup,0) = Val_long(u.ru_stime.tv_sec);
    Field(tup,1) = Val_long(u.ru_stime.tv_usec);
    Store_field(ar,1,tup);

    unsigned int i = 2;

#define X(y)                                    \
    tup = caml_copy_int64(u.ru_ ## y);          \
    Store_field(ar,i,tup);                      \
    i++                                         \

    X(maxrss);
    X(ixrss);
    X(idrss);
    X(isrss);
    X(minflt);
    X(majflt);
    X(nswap);
    X(inblock);
    X(oublock);
    X(msgsnd);
    X(msgrcv);
    X(nsignals);
    X(nvcsw);
    X(nivcsw);
#undef X
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar;
  CAMLreturn(res);
}

CAMLprim value
uwt_cpu_info(value unit)
{
  CAMLparam0();
  CAMLlocal4(ar_out,ar_in,tup,tmp);
  uv_cpu_info_t * cpu_infos;
  int n_cpu;
  const int r = uv_cpu_info(&cpu_infos,&n_cpu);
  uint8_t tag;
  (void) unit;
  if ( r < 0 || n_cpu <= 0 ){
    ar_out = Val_uwt_error(r);
    tag = Error_tag;
  }
  else {
    tag = Ok_tag;
    ar_out = caml_alloc(n_cpu,0);
    int i;
    for ( i = 0; i < n_cpu; ++i ){
      uv_cpu_info_t * c = &cpu_infos[i];
      tup = caml_alloc(3,0);
      tmp = s_caml_copy_string(c->model);
      Store_field(tup,0,tmp);
      Field(tup,1) = Val_long(c->speed);

      ar_in = caml_alloc(5,0);
      int j = 0;
#define X(y)                                    \
      tmp = caml_copy_int64(c->cpu_times.y);    \
      Store_field(ar_in,j,tmp);                 \
      j++

      X(user);
      X(nice);
      X(sys);
      X(idle);
      X(irq);
#undef X
      Store_field(tup,2,ar_in);
      Store_field(ar_out,i,tup);
    }
    uv_free_cpu_info(cpu_infos,n_cpu);
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar_out;
  CAMLreturn(res);
}

CAMLprim value
uwt_interface_addresses(value unit)
{
  CAMLparam0();
  CAMLlocal3(ar_out,ar_in,tmp);
  uv_interface_address_t* addresses;
  int n_addresses;
  const int r = uv_interface_addresses(&addresses,&n_addresses);
  uint8_t tag;
  (void) unit;
  if ( r < 0 || n_addresses < 0 ){
    ar_out = Val_uwt_error(r);
    tag = Error_tag;
  }
  else if ( n_addresses == 0 ){
    ar_out = Atom(0);
    tag = Ok_tag;
    uv_free_interface_addresses(addresses,n_addresses);
  }
  else {
    tag = Ok_tag;
    ar_out = caml_alloc(n_addresses,0);
    int i;
    for ( i = 0; i < n_addresses; ++i ){
      uv_interface_address_t *  c = &addresses[i];
      ar_in = caml_alloc(5,0);
      tmp = s_caml_copy_string(c->name);
      Store_field(ar_in,0,tmp);

      tmp = caml_alloc_string(6);
      memcpy(String_val(tmp),c->phys_addr,6);
      Store_field(ar_in,1,tmp);
      Field(ar_in,2) = Val_long( c->is_internal != 0 );

      tmp = uwt__alloc_sockaddr((struct sockaddr*)&c->address);
      if ( tmp != Val_unit ){
        value x = caml_alloc_small(1,0);
        Field(x,0) = tmp;
        tmp = x;
      }
      Store_field(ar_in,3,tmp);

      tmp = uwt__alloc_sockaddr((struct sockaddr*)&c->netmask);
      if ( tmp != Val_unit ){
        value x = caml_alloc_small(1,0);
        Field(x,0) = tmp;
        tmp = x;
      }
      Store_field(ar_in,4,tmp);

      Store_field(ar_out,i,ar_in);
    }
    uv_free_interface_addresses(addresses,n_addresses);
  }
  value res = caml_alloc_small(1,tag);
  Field(res,0) = ar_out;
  CAMLreturn(res);
}

CAMLprim value
uwt_load_avg(value unit)
{
  CAMLparam0();
  CAMLlocal3(tmp1,tmp2,tmp3);
  double avg[3];
  (void) unit;
  uv_loadavg(avg);

  tmp1 = caml_copy_double(avg[0]);
  tmp2 = caml_copy_double(avg[1]);
  tmp3 = caml_copy_double(avg[2]);

  value ret = caml_alloc_small(3,0);
  Field(ret,0) = tmp1;
  Field(ret,1) = tmp2;
  Field(ret,2) = tmp3;

  CAMLreturn(ret);
}

CAMLprim value
uwt_get_total_memory(value unit)
{
  (void)unit;
  return (caml_copy_int64(uv_get_total_memory()));
}

CAMLprim value
uwt_hrtime(value unit)
{
  (void)unit;
  return (caml_copy_int64(uv_hrtime()));
}

typedef int(*os_dir)(char *, size_t *);
static value
uwt_os_dir(os_dir fdir_func)
{
  CAMLparam0();
  CAMLlocal1(p);
  value ret;
  char buffer[ALLOCA_PATH_LEN];
  size_t size = ALLOCA_PATH_LEN;
  int tag;
  caml_enter_blocking_section();
  const int r = fdir_func(buffer,&size);
  caml_leave_blocking_section();
  if ( r == 0 && size > 0 && size <= ALLOCA_PATH_LEN ){
    p = caml_alloc_string(size);
    memcpy(String_val(p), buffer, size);
    tag = Ok_tag;
  }
  else {
    p = Val_uwt_error(r);
    tag = Error_tag;
  }
  ret = caml_alloc_small(1,tag);
  Field(ret,0) = p;
  CAMLreturn(ret);
}

CAMLprim value
uwt_exepath(value unit)
{
  (void)unit;
  return (uwt_os_dir(uv_exepath));
}

CAMLprim value
uwt_cwd(value unit)
{
  (void)unit;
  return (uwt_os_dir(uv_cwd));
}

CAMLprim value
uwt_os_homedir(value unit)
{
  (void)unit;
  return (uwt_os_dir(uv_os_homedir));
}

CAMLprim value
uwt_os_tmpdir(value unit)
{
  (void)unit;
#if HAVE_DECL_UV_OS_TMPDIR
  return (uwt_os_dir(uv_os_tmpdir));
#else
  return (uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS));
#endif
}

CAMLprim value
uwt_chdir(value o_string)
{
  int r;
  if ( !uwt_is_safe_string(o_string) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  char * s = strdup(String_val(o_string));
  if ( s == NULL ){
    return VAL_UWT_INT_RESULT_ENOMEM;
  }
  caml_enter_blocking_section();
  r = uv_chdir(s);
  caml_leave_blocking_section();
  free(s);

  return (VAL_UWT_INT_RESULT(r));
}

CAMLprim value
uwt_get_passwd(value unit)
{
  (void)unit;
#if !HAVE_DECL_UV_OS_GET_PASSWD
  return (uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS));
#else
  uv_passwd_t pwd;
  value eret;
  int i;
  caml_enter_blocking_section();
  i = uv_os_get_passwd(&pwd);
  caml_leave_blocking_section();
  if ( i != 0 ){
  error_ret:
    eret = caml_alloc_small(1,Error_tag);
    Field(eret,0) = Val_uwt_error(i);
    return eret;
  }
  else {
#define BSIZE 8192
    value name = Val_unit, empty = Val_unit;
    value dir = Val_unit, shell = Val_unit;
    value res = Val_unit;

    char buf[3][BSIZE];
    const char * const to_copy[3] = {pwd.shell,pwd.homedir,pwd.username};
    for ( i = 0 ; i < 3 ; i++ ){
      const char * const src = to_copy[i];
      char *dst = buf[i];
      if ( src == NULL ){
        dst[0] = 0 ;
      }
      else {
        const size_t l = strlen(src);
        if ( l >= BSIZE ){
          uv_os_free_passwd(&pwd);
          i = UV_UNKNOWN;
          goto error_ret;
        }
        memcpy(dst,src,l+1);
      }
    }
#undef BSIZE
    long uid = pwd.uid;
    long gid = pwd.gid;
    uv_os_free_passwd(&pwd);

    Begin_roots4 (name, empty, dir, shell);

    shell = caml_copy_string(buf[0]);
    dir = caml_copy_string(buf[1]);
    name = caml_copy_string(buf[2]);

    empty = caml_copy_string("");
    res = caml_alloc_small(7, 0);
    Field(res, 0) = name;
    Field(res, 1) = empty;
    Field(res, 2) = Val_long(uid);
    Field(res, 3) = Val_long(gid);
    Field(res, 4) = empty;
    Field(res, 5) = dir;
    Field(res, 6) = shell;
    shell = res;
    name = caml_alloc_small(1,Ok_tag);
    Field(name,0) = shell;
    End_roots();
    return name;
  }
#endif
}

static char ** uv_setup_args_ret = NULL;
static char ** dummy_argv = NULL;

static int
uwt_setup_args(value sys_argv)
{
  const unsigned int argc = Wosize_val(sys_argv);
  size_t size = 0;
  char ** argv;
  char *p;
  char * memb;
  unsigned int i;
  if ( argc == 0 ){
    return UV_UNKNOWN;
  }
  argv = malloc( (argc+1) * sizeof (char*) );
  if ( argv == NULL ) {
    return UV_ENOMEM;
  }

  for ( i = 0; i < argc; i++ ) {
    value s = Field(sys_argv,i);
    if ( !uwt_is_safe_string(s) ){
      free(argv);
      return UV_ECHARSET;
    }
    size += caml_string_length(s) + 1;
  }

  memb = malloc(size);
  if ( memb == NULL ){
    free(argv);
    return UV_ENOMEM;
  }

  /* libuv wants the argv arguments in this order in memory.
     Otherwise an assert failure is triggered,.... */
  p = memb;
  for ( i = 0; i < argc ; i++ ) {
    size_t n = caml_string_length(Field(sys_argv,i));
    argv[i] = p;
    memcpy(p,String_val(Field(sys_argv,i)),n + 1);
    p = p + n + 1;
  }
  argv[argc] = NULL;

  uv_setup_args_ret = uv_setup_args(argc,argv);
  dummy_argv = argv;

  return 0;
}

CAMLprim value
uwt_get_process_title(value sys_argv)
{
  CAMLparam0();
  CAMLlocal1(p);
#define BSIZE 16384
  value ret;
  char buffer[BSIZE];
  uint8_t tag;
  int r;
  if ( uv_setup_args_ret == NULL ){
    r = uwt_setup_args(sys_argv);
    if ( r ){
      goto error_end;
    }
  }
  r = uv_get_process_title(buffer,BSIZE - 1);
  if ( r == 0 ){
    buffer[BSIZE-1] = '\0';
    p = caml_copy_string(buffer);
    tag = Ok_tag;
  }
  else {
  error_end:
    p = Val_uwt_error(r);
    tag = Error_tag;
  }
  ret = caml_alloc_small(1,tag);
  Field(ret,0) = p;
  CAMLreturn(ret);
#undef BSIZE
}

CAMLprim value
uwt_set_process_title_na(value sys_argv, value o_str)
{
  int r;
  if ( !uwt_is_safe_string(o_str) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  if ( uv_setup_args_ret == NULL ){
    r = uwt_setup_args(sys_argv);
    if ( r ){
      return (VAL_UWT_INT_RESULT(r));
    }
  }
  r = uv_set_process_title(String_val(o_str));
  return (VAL_UWT_INT_RESULT(r));
}


#ifdef _WIN32
static int rtl_get_version(OSVERSIONINFOEXW *os)
{
  typedef NTSTATUS (WINAPI ptrRtlGetVersion)(PRTL_OSVERSIONINFOEXW);
  ptrRtlGetVersion *RtlGetVersion;
  HMODULE hm = GetModuleHandleW(L"ntdll.dll");
  if ( hm == NULL ){
    return (uwt_translate_sys_error(GetLastError()));
  }
  RtlGetVersion = (ptrRtlGetVersion *)GetProcAddress(hm, "RtlGetVersion");
  if ( RtlGetVersion == NULL ){
    return (uwt_translate_sys_error(GetLastError()));
  }
  RtlGetVersion(os);
  return 0;
}

CAMLprim value uwt_win_version(value unit);
CAMLprim value
uwt_win_version(value unit)
{
  OSVERSIONINFOEXW os;
  value ret;
  (void) unit;
  ZeroMemory(&os, sizeof(os));
  os.dwOSVersionInfoSize = sizeof(os);
  int error = rtl_get_version(&os);
  if ( error != 0 ){
    goto error_end;
  }
  char * szCSDVersion = NULL;
  os.wServicePackMajor = 0; /* not used, first value after szCSDVersion */
  szCSDVersion = uwt_utf16_to_utf8(os.szCSDVersion,&error);
  if ( szCSDVersion == NULL ){
    goto error_end;
  }
  value s = s_caml_copy_string(szCSDVersion);
  free(szCSDVersion);
  value cont = Val_unit;
  Begin_roots2(s,cont);
    cont = caml_alloc_small(5,0);
    Field(cont,0) = Val_long(os.dwMajorVersion);
    Field(cont,1) = Val_long(os.dwMinorVersion);
    Field(cont,2) = Val_long(os.dwBuildNumber);
    Field(cont,3) = Val_long(os.dwPlatformId);
    Field(cont,4) = s;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = cont;
  End_roots();
  return ret;
 error_end:
  ret = caml_alloc_small(1,Error_tag);
  Field(ret,0) = Val_uwt_error(error);
  return ret;
}
#endif /* _WIN32 */

typedef void(*print_handles)(uv_loop_t*, FILE*);
static value
uwt_print_handles(value o_loop, value o_fd, print_handles phandles)
{
  INIT_LOOP_INT_RESULT(l,o_loop);
  int fd ;
  FILE * fp;
#ifdef _WIN32
  fd = _dup(FD_VAL(o_fd));
#else
  fd = dup(FD_VAL(o_fd));
#endif
  if ( fd == -1 ){
    return VAL_UWT_UNIT_RESULT(UWT_TRANSLATE_ERRNO(errno));
  }
#ifdef _WIN32
  fp = _fdopen(fd,"w");
#else
  fp = fdopen(fd,"w");
#endif
  if ( fp == NULL ){
#ifdef _WIN32
    _close(fd);
#else
    close(fd);
#endif
    return VAL_UWT_UNIT_RESULT(UWT_TRANSLATE_ERRNO(errno));
  }
  phandles(&l->loop,fp);
  if ( fclose(fp) ){
    return(VAL_UWT_UNIT_RESULT(UWT_TRANSLATE_ERRNO(errno)));
  }
  return Val_unit;
}

CAMLprim value
uwt_print_all_handles(value a, value b)
{
  return uwt_print_handles(a,b,uv_print_all_handles);
}

CAMLprim value
uwt_print_active_handles(value a, value b)
{
  return uwt_print_handles(a,b,uv_print_active_handles);
}

CAMLprim value
uwt_os_getenv(value on)
{
#if HAVE_DECL_UV_OS_GETENV
  CAMLparam1(on);
  CAMLlocal1(p);
  char buffer[ALLOCA_PATH_LEN];
  size_t size = ALLOCA_PATH_LEN;
  uint8_t tag = Ok_tag;
  if (unlikely (!uwt_is_safe_string(on))){
    p = VAL_UWT_ERROR_ECHARSET;
    tag = Error_tag;
  }
  else {
    int r = uv_os_getenv(String_val(on), buffer, &size);
    if ( r == 0 ){
      p = caml_alloc_string(size);
      memcpy(String_val(p), buffer, size);
    }
    else {
      if ( r == UV_ENOBUFS ){
        p = caml_alloc_string(size-1);
        r = uv_os_getenv(String_val(on), String_val(p), &size);
      }
      if ( r != 0 ){
        p = Val_uwt_error(r);
        tag = Error_tag;
      }
    }
  }
  value ret = caml_alloc_small(1,tag);
  Field(ret,0) = p;
  CAMLreturn(ret);
#else
  (void) on;
  return (uwt__alloc_eresult(VAL_UWT_ERROR_ENOSYS));
#endif
}

CAMLprim value
uwt_os_setenv_na(value on, value ov)
{
#if HAVE_DECL_UV_OS_SETENV
  if (unlikely( !uwt_is_safe_string(on) ||
                !uwt_is_safe_string(ov) )){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  const int r = uv_os_setenv(String_val(on), String_val(ov));
  return (VAL_UWT_UNIT_RESULT(r));
#else
  (void) on;
  (void) ov;
  return VAL_UWT_INT_RESULT_ENOSYS;
#endif
}

CAMLprim value
uwt_os_unsetenv_na(value on)
{
#if HAVE_DECL_UV_OS_UNSETENV
  if (unlikely( !uwt_is_safe_string(on) )){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  const int r = uv_os_unsetenv(String_val(on));
  return (VAL_UWT_UNIT_RESULT(r));
#else
  (void) on;
  return VAL_UWT_INT_RESULT_ENOSYS;
#endif
}

CAMLprim value
uwt_os_getppid_na(value unit)
{
  (void) unit;
#if HAVE_DECL_UV_OS_GETPPID
  uv_pid_t p = uv_os_getppid();
#ifdef _WIN32
  if ( p < 0 ) { /* in error case, just `-1`, no error translation */
    return VAL_UWT_INT_RESULT_UNKNOWN;
  }
#endif
  return (Val_long(p));
#else
  return VAL_UWT_INT_RESULT_ENOSYS;
#endif
}
