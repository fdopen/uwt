/* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
 * Module Uwt.Unix
 * Copyright (C) 2015 Andreas Hauptmann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "config.h"
#include <uv.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_GRP_H
#include <grp.h>
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#define CAML_NAME_SPACE 1
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>

#ifdef _WIN32
#include <io.h>
#include <wchar.h>
#include <Lmcons.h>
#endif

#include "uwt-error.h"
#include "uwt-worker.h"
#include "macros.h"

#define P(x)                                    \
  CAMLextern value uwt_ ## x (value,value)

P(getlogin);
P(getcwd);
P(chdir);
P(getpwnam);
P(getpwuid);
P(getgrnam);
P(getgrgid);
P(chroot);
P(gethostbyname);
P(gethostbyaddr);
P(getservbyname);
P(getservbyport);
P(getprotobyname);
P(getprotobynumber);
P(gethostname);
P(lockf);
P(realpath);
#undef P

CAMLextern value uwt_pipe(value);

#define ALLOCA_SIZE 16384

static void
free_p1(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL && w->p1 != (void*)1 ){
    free(w->p1);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
getstrp1_camlval(uv_req_t * req)
{
  CAMLparam0();
  CAMLlocal1(tmp);
  value ret;
  struct worker_params * w = req->data;
  if ( w->p1 == NULL ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(POINTER_TO_INT(w->p2));
  }
  else {
    tmp = caml_copy_string(w->p1);
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = tmp;
  }
  CAMLreturn(ret);
}

static value
getunitp2_camlval(uv_req_t * req)
{
  value ret;
  struct worker_params * w = req->data;
  int er = POINTER_TO_INT(w->p2);
  if ( er != 0 ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = Val_uwt_error(POINTER_TO_INT(er));
  }
  else {
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = Val_unit;
  }
  return ret;
}

static char **
c_copy_string_array(char **src)
{
  if ( src == NULL ){
    return NULL;
  }
  char ** p = src;
  while ( *p ){
    p++;
  }
  const size_t len = p - src;
  p = malloc((len+1) * sizeof(char *));
  if ( p == NULL ){
    return NULL;
  }
  size_t i;
  for ( i = 0 ; i < len ; ++i ){
    p[i] = strdup(src[i]);
    if ( p[i] == NULL ){
      size_t j;
      for ( j = 0 ; j < i ; ++j ){
        free(p[j]);
      }
      free(p);
      return NULL;
    }
  }
  p[len] = NULL;
  return p;
}

static char **
c_copy_addr_array(char ** src, int addr_len)
{
  if ( src == NULL ){
    return NULL;
  }
  char ** p = src;
  while ( *p ){
    p++;
  }
  const size_t ar_len = p - src;
  p = malloc((ar_len+1) * sizeof(char*));
  if ( p == NULL ){
    return NULL;
  }
  size_t i;
  for ( i = 0 ; i < ar_len ; ++i ){
    p[i] = malloc(addr_len);
    if ( p[i] == NULL ){
      size_t j;
      for ( j = 0 ; j < i ; ++j ){
        free(p[j]);
      }
      free(p);
      return NULL;
    }
    memcpy(p[i],src[i],addr_len);
  }
  p[ar_len] = NULL;
  return p;
}

static void
c_free_string_array(char ** src)
{
  if ( src ){
    char ** p = src;
    while (*p){
      free(*p);
      ++p;
    }
    free(src);
  }
}

static struct hostent *
dup_hostent(struct hostent *orig)
{
  if ( orig == NULL ){
    return NULL;
  }
  struct hostent *h = malloc(sizeof *h);
  if ( h == NULL ){
    return NULL;
  }
  h->h_name = s_strdup(orig->h_name);
  if ( !h->h_name ){
    goto nomem1;
  }
  if ( !orig->h_aliases ){
    h->h_aliases = NULL;
  }
  else {
    h->h_aliases = c_copy_string_array(orig->h_aliases);
    if ( !h->h_aliases){
      goto nomem2;
    }
  }
  if ( !orig->h_addr_list ){
    h->h_addr_list = NULL;
  }
  else {
    h->h_addr_list = c_copy_addr_array(orig->h_addr_list,orig->h_length);
    if ( !h->h_addr_list ){
      goto nomem3;
    }
  }
  h->h_addrtype = orig->h_addrtype;
  h->h_length = orig->h_length;
  return h;
nomem3:
  c_free_string_array(h->h_aliases);
nomem2:
  free(h->h_name);
nomem1:
  free(h);
  return NULL;
}

static void
gethostbyname_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  char *name = w->p1;
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct hostent result_buf;
  struct hostent *host = NULL;
  char buf[ALLOCA_SIZE];
  int hno;
  int err = gethostbyname_r(name,&result_buf,&buf[0],ALLOCA_SIZE,&host,&hno);
  if ( err != 0 || host == NULL ){
    w->p1 = NULL;
    host = NULL;
    if ( hno == HOST_NOT_FOUND ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
#ifdef TRY_AGAIN
    else if ( hno == TRY_AGAIN ){
      w->p2 = INT_TO_POINTER(UV_EAGAIN);
    }
#endif
    else {
      w->p2 = INT_TO_POINTER(UV_UNKNOWN);
    }
  }
#else
  struct hostent * host = gethostbyname(name);
  if ( host == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct hostent * h = dup_hostent(host);
    if ( h == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = h;
      w->p2 = (void*)1;
    }
  }
  free(name);
}

static void
gethostbyname_cleaner(uv_req_t *req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    if ( w->p2 == NULL || w->p2 == (void*)2 ){ /* see gethostbyaddr */
      free(w->p1);
    }
    else {
      struct hostent * h = w->p1;
      c_free_string_array(h->h_addr_list);
      c_free_string_array(h->h_aliases);
      free(h->h_name);
      free(h);
    }
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value alloc_one_addr(char const *a)
{
  struct in_addr addr;
  memmove (&addr, a, 4);
  return alloc_inet_addr(&addr);
}

static value alloc_one_addr6(char const *a)
{
  struct in6_addr addr;
  memmove(&addr, a, 16);
  return alloc_inet6_addr(&addr);
}

static value
gethostent_value(uv_req_t * req)
{
  value ret;
  struct worker_params * w = req->data;
  struct hostent * entry = w->p1;
  if ( entry == NULL ){
    value t = Val_uwt_error(POINTER_TO_INT(w->p2));
    if ( t == VAL_UWT_ERROR_UWT_UNKNOWN ){
      t = VAL_UWT_ERROR_ENOENT;
    }
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = t;
  }
  else {
    value res;
    value name = Val_unit, aliases = Val_unit;
    value addr_list = Val_unit, adr = Val_unit;

    Begin_roots4 (name, aliases, addr_list, adr);
    name = s_caml_copy_string(entry->h_name);
    /* PR#4043: protect against buggy implementations of gethostbyname()
       that return a NULL pointer in h_aliases */
    aliases = s_caml_copy_string_array((const char**)entry->h_aliases);
    if ( entry->h_addr_list == NULL )
      addr_list = Atom(0);
    else if (entry->h_length == 16)
      addr_list = caml_alloc_array(alloc_one_addr6,(const char**)entry->h_addr_list);
    else
      addr_list = caml_alloc_array(alloc_one_addr,(const char**)entry->h_addr_list);
    res = caml_alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    switch (entry->h_addrtype) {
    case PF_UNIX:          Field(res, 2) = Val_int(0); break;
    case PF_INET:          Field(res, 2) = Val_int(1); break;
    default: /*PF_INET6 */ Field(res, 2) = Val_int(2); break;
    }
    Field(res, 3) = addr_list;
    name = caml_alloc_small(1,Ok_tag);
    Field(name,0) = res;
    ret = name;
    End_roots();
  }
  return ret;
}


CAMLprim value
uwt_gethostbyname(value o_name, value o_uwt)
{
  value ret;
  const char * mname = String_val(o_name);
  char * name;
  if ( mname == NULL || *mname == '\0' ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
  }
  else if ( (name = strdup(mname)) == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
  }
  else {
    ret = uwt_add_worker_result(o_uwt,
                                gethostbyname_cleaner,
                                gethostbyname_worker,
                                gethostent_value,
                                name,
                                NULL);
  }
  return ret;
}

static void
gethostbyaddr_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  int j = POINTER_TO_INT(w->p2);
  void * addr = w->p1;
  socklen_t len;
  int type;
  if ( j == 0 ){ /* ip6 */
    len = sizeof(struct in6_addr);
    type = AF_INET6;
  }
  else {
    len = sizeof(struct in_addr);
    type = AF_INET;
  }
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct hostent result_buf;
  struct hostent *host = NULL;
  char buf[ALLOCA_SIZE];
  int hno;
  int err = gethostbyaddr_r(addr,len,type,&result_buf,&buf[0],ALLOCA_SIZE,
                            &host,&hno);
  if ( err != 0 || host == NULL ){
    w->p1 = NULL;
    host = NULL;
    if ( hno == HOST_NOT_FOUND ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
#ifdef TRY_AGAIN
    else if ( hno == TRY_AGAIN ){
      w->p2 = INT_TO_POINTER(UV_EAGAIN);
    }
#endif
    else {
      w->p2 = INT_TO_POINTER(UV_UNKNOWN);
    }
  }
#else
  struct hostent * host = gethostbyaddr(addr,len,type);
  if ( host == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct hostent * h = dup_hostent(host);
    if ( h == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = h;
      w->p2 = (void*)1;
    }
  }
  free(addr);
}

CAMLprim value
uwt_gethostbyaddr(value o_ip, value o_uwt)
{
  void *p1;
  void *p2;
  if ( caml_string_length(o_ip) == 16 ){
#ifndef GET_INET6_ADDR
    return VAL_UWT_INT_RESULT_UWT_EUNAVAIL;
#else
    struct in6_addr * s = malloc(sizeof *s);
    if ( !s ){
      return VAL_UWT_INT_RESULT_ENOMEM;
    }
    *s = GET_INET6_ADDR(o_ip);
    p1 = s;
    p2 = INT_TO_POINTER(0);
#endif
  }
  else {
    struct in_addr * s = malloc(sizeof *s);
    if ( !s ){
      return VAL_UWT_INT_RESULT_ENOMEM;
    }
    *s = GET_INET_ADDR(o_ip);
    p1 = s;
    p2 = INT_TO_POINTER(2);
  }
  value ret;
  ret = uwt_add_worker_result(o_uwt,
                              gethostbyname_cleaner,
                              gethostbyaddr_worker,
                              gethostent_value,
                              p1,
                              p2);
  return ret;
}

static struct servent *
dup_servent(const struct servent * serv)
{
  if (!serv){
    return NULL;
  }
  struct servent * s = malloc(sizeof *s);
  if ( s == NULL ){
    goto nomem1;
  }
  s->s_name = s_strdup(serv->s_name);
  if ( s->s_name == NULL ){
    goto nomem2;
  }
  s->s_proto = s_strdup(serv->s_proto);
  if ( s->s_proto == NULL ){
    goto nomem3;
  }
  s->s_aliases = c_copy_string_array(serv->s_aliases);
  if ( s->s_aliases == NULL && serv->s_aliases != NULL ){
    goto nomem4;
  }
  s->s_port = serv->s_port;
  return s;
nomem4:
  free(s->s_proto);
nomem3:
  free(s->s_name);
nomem2:
  free(s);
nomem1:
  return NULL;
}

static void
getservbyname_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  char * name = w->p1;
  char * p2_orig = w->p2;
  char * proto = *p2_orig == '\0' ? NULL : p2_orig;
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct servent result_buf;
  struct servent *serv = NULL;
  char buf[ALLOCA_SIZE];
  int err = getservbyname_r(name,proto,&result_buf,&buf[0],ALLOCA_SIZE,&serv);
  if ( err != 0 || serv == NULL ){
    w->p1 = NULL;
    /* err == 1: older APIs, that don't return errno numbers */
    if ( err == ENOENT || (err == 0 && serv == NULL) || err == 1 || err < 0 ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
    else {
      w->p2 = INT_TO_POINTER(-err);
    }
    serv = NULL;
  }
#else
  struct servent * serv = getservbyname(name,proto);
  if ( serv == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct servent * s = dup_servent(serv);
    if ( s == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = s;
      w->p2 = NULL;
    }
  }
  free(name);
  free(p2_orig);
}

static void
getservbyname_cleaner(uv_req_t *req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    if ( w->p2 != NULL ){
      free(w->p1);
      free(w->p2);
    }
    else {
      struct servent * s = w->p1;
      free(s->s_proto);
      free(s->s_name);
      c_free_string_array(s->s_aliases);
      free(s);
    }
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
getservent_value(uv_req_t * req)
{
  struct worker_params * w = req->data;
  value ret;
  struct servent *entry = w->p1 ;
  if ( entry == NULL ){
    value t = Val_uwt_error(POINTER_TO_INT(w->p2));
    if ( t == VAL_UWT_ERROR_UWT_UNKNOWN ){
      t = VAL_UWT_ERROR_ENOENT;
    }
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = t;
  }
  else {
    value res;
    value name = Val_unit, aliases = Val_unit, proto = Val_unit;
    Begin_roots3(name, aliases, proto);
    name = s_caml_copy_string(entry->s_name);
    aliases = s_caml_copy_string_array((const char **)entry->s_aliases);
    proto = s_caml_copy_string(entry->s_proto);
    res = caml_alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(ntohs(entry->s_port));
    Field(res,3) = proto;
    aliases = res;
    name = caml_alloc_small(1,Ok_tag);
    Field(name,0) = aliases;
    ret = name;
    End_roots();
  }
  return ret;
}

CAMLprim value
uwt_getservbyname(value o_b, value o_uwt)
{
  value ret;
  char * p1;
  char * p2;
  value o_name = Field(o_b,0);
  value o_proto = Field(o_b,1);
  p1 = s_strdup(String_val(o_name));
  p2 = s_strdup(String_val(o_proto));
  if ( p1 == NULL || p2 == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
  }
  else {
    ret = uwt_add_worker_result(o_uwt,
                                getservbyname_cleaner,
                                getservbyname_worker,
                                getservent_value,
                                p1,
                                p2);
  }
  return ret;
}

static void
getservbyport_cleaner(uv_req_t *req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    if ( w->p2 != NULL ){
      free(w->p1);
    }
    else {
      struct servent * s = w->p1;
      free(s->s_proto);
      free(s->s_name);
      c_free_string_array(s->s_aliases);
      free(s);
    }
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static void
getservbyport_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  int port = POINTER_TO_INT(w->p2);
  char * p1_orig = w->p1;
  char * proto = *p1_orig == '\0' ? NULL : p1_orig;
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct servent result_buf;
  struct servent *serv = NULL;
  char buf[ALLOCA_SIZE];
  int err = getservbyport_r(port,proto,&result_buf,&buf[0],ALLOCA_SIZE,&serv);
  if ( err != 0 || serv == NULL ){
    w->p1 = NULL;
    if ( err == ENOENT || (err == 0 && serv == NULL) || err == 1 || err < 0 ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
    else {
      w->p2 = INT_TO_POINTER(-err);
    }
    serv = NULL;
  }
#else
  struct servent * serv = getservbyport(port,proto);
  if ( serv == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct servent * s = dup_servent(serv);
    if ( s == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = s;
      w->p2 = NULL;
    }
  }
  free(p1_orig);
}

CAMLprim value
uwt_getservbyport(value o_b, value o_uwt)
{
  value ret;
  value o_port = Field(o_b,0);
  value o_proto = Field(o_b,1);
  void *p1;
  void *p2;

  p2 = INT_TO_POINTER(htons(Long_val(o_port)));
  p1 = s_strdup(String_val(o_proto));
  if ( p1 == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
  }
  else {
    ret = uwt_add_worker_result(o_uwt,
                                getservbyport_cleaner,
                                getservbyport_worker,
                                getservent_value,
                                p1,
                                p2);
  }
  return ret;
}

static struct protoent *
dup_protoent(const struct protoent * proto)
{
  if (!proto){
    return NULL;
  }
  struct protoent * p = malloc(sizeof *p);
  if ( p == NULL ){
    return NULL;
  }
  p->p_name = s_strdup(proto->p_name);
  if ( p->p_name == NULL ){
    goto nomem1;
  }
  p->p_aliases = c_copy_string_array( proto->p_aliases );
  if ( p->p_aliases == NULL && proto->p_aliases != NULL){
    goto nomem2;
  }
  p->p_proto = proto->p_proto;
  return p;
nomem2:
  free(p->p_name);
nomem1:
  free(p);
  return NULL;
}

static value
getprotoent_value(uv_req_t * req)
{
  value ret;
  struct worker_params * w = req->data;
  struct protoent *entry = w->p1 ;
  if ( entry == NULL ){
    value t = Val_uwt_error(POINTER_TO_INT(w->p2));
    if ( t == VAL_UWT_ERROR_UWT_UNKNOWN ){
      t = VAL_UWT_ERROR_ENOENT;
    }
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = t;
  }
  else {
    value name = Val_unit, aliases = Val_unit;
    Begin_roots2 (aliases, name);
    name = s_caml_copy_string(entry->p_name);
    aliases = s_caml_copy_string_array((const char**)entry->p_aliases);
    ret = caml_alloc_small(3, 0);
    Field(ret,0) = name;
    Field(ret,1) = aliases;
    Field(ret,2) = Val_int(entry->p_proto);
    aliases = ret;
    ret = caml_alloc_small(1,0);
    Field(ret,0) = aliases;
    End_roots();
  }
  return ret;
}

static void
getprotobyname_cleaner(uv_req_t * req)
{
  struct worker_params * w = req->data;
  if ( w->p1 != NULL ){
    if ( w->p2 == NULL ){
      free(w->p1);
    }
    else {
      struct protoent * p = w->p1;
      free(p->p_name);
      c_free_string_array(p->p_aliases);
      free(p);
    }
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static void
getprotobyname_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  char * name = w->p1;
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct protoent result_buf;
  struct protoent *proto = NULL;
  char buf[ALLOCA_SIZE];
  int err = getprotobyname_r(name,&result_buf,&buf[0],ALLOCA_SIZE,&proto);
  if ( err != 0 || proto == NULL ){
    w->p1 = NULL;
    if ( err == ENOENT || (err == 0 && proto == NULL ) || err == 1 || err < 0 ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
    else {
      w->p2 = INT_TO_POINTER(-err);
    }
  }
#else
  struct protoent * proto = getprotobyname(name);
  if ( proto == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct protoent * p = dup_protoent(proto);
    if ( p == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = p;
      w->p2 = (void*)1;
    }
  }
  free(name);
}

CAMLprim value
uwt_getprotobyname(value o_name, value o_uwt)
{
  value ret;
  char *p1;
  const char * mname = String_val(o_name);
  if (  mname == NULL || *mname == '\0' ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
  }
  else if ( (p1 = strdup(mname)) == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
  }
  else {
    ret = uwt_add_worker_result(o_uwt,
                                getprotobyname_cleaner,
                                getprotobyname_worker,
                                getprotoent_value,
                                p1,
                                NULL);
  }
  return ret;
}

static void
getprotobynumber_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int number = POINTER_TO_INT(w->p2);
#ifdef HAVE_GETxxxxBYyyyy_R_POSIX
  struct protoent result_buf;
  struct protoent *proto = NULL;
  char buf[ALLOCA_SIZE];
  int err = getprotobynumber_r(number,&result_buf,&buf[0],ALLOCA_SIZE,&proto);
  if ( err != 0 || proto == NULL ){
    w->p1 = NULL;
    if ( err == ENOENT || (err == 0 && proto == NULL) || err == 1 || err < 0 ){
      w->p2 = INT_TO_POINTER(UV_ENOENT);
    }
    else {
      w->p2 = INT_TO_POINTER(-err);
    }
  }
#else
  struct protoent * proto = getprotobynumber(number);
  if ( proto == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(UV_ENOENT);
  }
#endif
  else {
    struct protoent * p = dup_protoent(proto);
    if ( p == NULL ){
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
    else {
      w->p1 = p;
      w->p2 = (void*)2;
    }
  }
}

CAMLprim value
uwt_getprotobynumber(value o_number, value o_uwt)
{
  value ret;
  void * p2 = INT_TO_POINTER(Long_val(o_number));
  ret = uwt_add_worker_result(o_uwt,
                              getprotobyname_cleaner,
                              getprotobynumber_worker,
                              getprotoent_value,
                              NULL,
                              p2);
  return ret;
}

static void
gethostname_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  char name[ALLOCA_SIZE];
  int er = gethostname(name,ALLOCA_SIZE-1);
  name[ALLOCA_SIZE-1]='\0';
  if ( er != 0 ){
    w->p2 = INT_TO_POINTER(-er);
  }
  else {
    w->p1 = s_strdup(name);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_gethostname(value o_void, value o_uwt)
{
  (void) o_void;
  value ret;
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              gethostname_worker,
                              getstrp1_camlval,
                              NULL,
                              NULL);
  return ret;
}

static void
getcwd_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  char name[ALLOCA_SIZE];
  size_t len = ALLOCA_SIZE;
  int er;
  name[0] = 0;
  er = uv_cwd(name,&len);
  if ( er != 0 ){
    w->p2 = INT_TO_POINTER(er);
  }
  else {
#if !defined(_WIN32) && (UV_VERSION_MAJOR == 1) && ( UV_VERSION_MINOR < 1 )
    size_t len = strnlen(name,ALLOCA_SIZE);
    if ( len > 1 && name[len-1] == '/' ){
      name[len-1] = '\0';
    }
#endif
    w->p1 = s_strdup(name);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_getcwd(value o_void, value o_uwt)
{
  (void) o_void;
  value ret;
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              getcwd_worker,
                              getstrp1_camlval,
                              NULL,
                              NULL);
  return ret;
}


static void
chdir_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int er = uv_chdir((char*)w->p1);
  if ( er != 0 ){
    w->p2 = INT_TO_POINTER(er);
  }
  else {
    w->p2 = 0;
  }
}

CAMLprim value
uwt_chdir(value o_path, value o_uwt)
{
  value ret;
  char * cpath;
  char * opath = String_val(o_path);
  if ( opath == NULL && *opath == 0 ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
    goto endp;
  }
  cpath = strdup(opath);
  if ( cpath == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
    goto endp;
  }
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              chdir_worker,
                              getunitp2_camlval,
                              cpath,
                              NULL);
endp:
  return ret;
}



#if defined(_WIN32) || HAVE_GETLOGIN || HAVE_GETLOGIN_R || HAVE_CUSERID

#ifdef _WIN32
static void
getlogin_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  WCHAR name[UNLEN+1];
  DWORD len = UNLEN+1;
  w->p1 = NULL;
  if ( GetUserNameW(name, &len) == 0 ){
    int er = uwt_translate_sys_error(GetLastError());
    w->p2 = INT_TO_POINTER(er);
  }
  else {
    int er;
    w->p1 = uwt_utf16_to_utf8(name,&er);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(er);
    }
  }
}
#else
static void
getlogin_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  char * name;
  int e;
  errno = 0;
#ifdef HAVE_GETLOGIN_R
  char buf[ALLOCA_SIZE];
  e = getlogin_r(buf,ALLOCA_SIZE);
  name = e == 0 ? buf : NULL;
  if ( e == 1 ){
    e = -UV_ENOENT;
  }
  else {
    if ( e < 0 ){
      e = errno;
    }
  }
#elif defined(HAVE_GETLOGIN)
  name = getlogin();
  e = errno;
#elif defined(HAVE_CUSERID)
  name = cuserid(NULL);
  e = errno;
#else
#error "getlogin not supported"
#endif
  if ( name == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(-e);
  }
  else {
    w->p1 = strdup(name);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}
#endif /* _WIN32 */

CAMLprim value
uwt_getlogin(value o_void, value o_uwt)
{
  (void) o_void;
  value ret;
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              getlogin_worker,
                              getstrp1_camlval,
                              NULL,
                              NULL);
  return ret;
}

#else /* defined(_WIN32) || HAVE_GETLOGIN || HAVE_GETLOGIN_R || HAVE_CUSERID */
F_EUNAVAIL2(getlogin)
#endif

#if defined(HAVE_GETPWNAM_R) || defined(HAVE_GETPWNAM)
static struct passwd *
dup_passwd(const struct passwd * orig)
{
  struct passwd * copy = malloc(sizeof *copy);
  if ( copy == NULL){
    return NULL;
  }
  copy->pw_name = s_strdup(orig->pw_name);
  copy->pw_passwd = s_strdup(orig->pw_passwd);
#if !defined(__BEOS__)
  copy->pw_gecos = s_strdup(orig->pw_gecos);
#endif
  copy->pw_dir = s_strdup(orig->pw_dir);
  copy->pw_shell = s_strdup(orig->pw_shell);

  if (!copy->pw_name || !copy->pw_passwd || !copy->pw_gecos ||
      !copy->pw_dir || !copy->pw_shell)
  {
    free(copy->pw_name);
    free(copy->pw_passwd);
#if !defined(__BEOS__)
    free(copy->pw_gecos);
#endif
    free(copy->pw_dir);
    free(copy->pw_shell);
    free(copy);
    return NULL;
  }
  copy->pw_uid = orig->pw_uid;
  copy->pw_gid = orig->pw_gid;
  return(copy);
}

static void
passwd_cleaner(uv_req_t * req)
{
  struct worker_params * w = req->data;
  struct passwd *pw = w->p1;
  if ( pw != NULL ){
    free(pw->pw_name);
    free(pw->pw_passwd);
#if !defined(__BEOS__)
    free(pw->pw_gecos);
#endif
    free(pw->pw_dir);
    free(pw->pw_shell);
    free(pw);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
passwd_camlval(uv_req_t * req)
{
  struct worker_params * w = req->data;
  struct passwd *pw = w->p1;
  value erg;
  if ( w->p1 == NULL ){
    value t = Val_uwt_error(POINTER_TO_INT(w->p2));
    if ( t == VAL_UWT_ERROR_UWT_UNKNOWN ){
      t = VAL_UWT_ERROR_ENOENT;
    }
    erg = caml_alloc_small(1,Error_tag);
    Field(erg,0) = t;
  }
  else {
    value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
    value dir = Val_unit, shell = Val_unit;
    value res;
    Begin_roots5 (name, passwd, gecos, dir, shell);
    name = caml_copy_string(pw->pw_name);
    passwd = caml_copy_string(pw->pw_passwd);
#if !defined(__BEOS__)
    gecos = caml_copy_string(pw->pw_gecos);
#else
    gecos = caml_copy_string("");
#endif
    dir = caml_copy_string(pw->pw_dir);
    shell = caml_copy_string(pw->pw_shell);
    res = caml_alloc_small(7, 0);
    Field(res, 0) = name;
    Field(res, 1) = passwd;
    Field(res, 2) = Val_int(pw->pw_uid);
    Field(res, 3) = Val_int(pw->pw_gid);
    Field(res, 4) = gecos;
    Field(res, 5) = dir;
    Field(res, 6) = shell;
    shell = res;
    name = caml_alloc_small(1,Ok_tag);
    Field(name,0) = shell;
    erg = name;
    End_roots();
  }
  return erg;
}

static void
getpwnam_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int e;
  struct passwd *res = NULL;
#ifdef HAVE_GETPWNAM_R
  char buf[ALLOCA_SIZE];
  struct passwd result;
  e = getpwnam_r((char*)w->p1,&result, buf, ALLOCA_SIZE, &res);
  if ( e != 0 ){
    res = NULL;
  }
  if ( e == 1 || (e == 0 && res == NULL) || e < 0 ){
    e = UV_ENOENT;
  }
#else
  errno = 0;
  res = getpwnam((char*)w->p1);
  e = errno == 0 ? UV_ENOENT : -errno;
#endif
  free(w->p1);
  if ( res == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(e);
  }
  else {
    w->p1 = dup_passwd(res);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_getpwnam(value o_path, value o_uwt)
{
  value ret;
  char * cpath;
  char * opath = String_val(o_path);
  if ( opath == NULL && *opath == 0 ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
    goto endp;
  }
  cpath = strdup(opath);
  if ( cpath == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
    goto endp;
  }
  ret = uwt_add_worker_result(o_uwt,
                              passwd_cleaner,
                              getpwnam_worker,
                              passwd_camlval,
                              cpath,
                              NULL);
endp:
  return ret;
}

#else
F_EUNAVAIL2(getpwnam)
#endif

#if defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWUID)
static void
getpwuid_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int e;
  struct passwd *res = NULL;
  uid_t uid = POINTER_TO_INT(w->p2);
#ifdef HAVE_GETPWUID_R
  char buf[ALLOCA_SIZE];
  struct passwd result;
  e = getpwuid_r(uid,&result, buf, ALLOCA_SIZE, &res);
  if ( e != 0 ){
    res = NULL;
  }
  if ( e == 1 || (e == 0 && res == NULL) || e < 0 ){
    e = UV_ENOENT;
  }
#else
  errno = 0;
  res = getpwuid(uid);
  e = errno == 0 ? UV_ENOENT : -errno;
#endif
  if ( res == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(e);
  }
  else {
    w->p1 = dup_passwd(res);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_getpwuid(value o_uid, value o_uwt)
{
  value ret;
  void * uid = INT_TO_POINTER(Long_val(o_uid));
  ret = uwt_add_worker_result(o_uwt,
                              passwd_cleaner,
                              getpwuid_worker,
                              passwd_camlval,
                              NULL,
                              uid);
  return ret;
}
#else
F_EUNAVAIL2(getpwuid)
#endif

#if defined(HAVE_GETGRNAM_R) || defined(HAVE_GETGRNAM)
static struct group *
dup_group(const struct group * orig)
{
  struct group * copy = malloc(sizeof *copy);
  if ( copy == NULL){
    return NULL;
  }
  copy->gr_name = s_strdup(orig->gr_name);
  copy->gr_passwd = s_strdup(orig->gr_passwd);
  copy->gr_mem = c_copy_string_array(orig->gr_mem);

  if ( !copy->gr_name || !copy->gr_passwd ||
       (!copy->gr_mem && orig->gr_mem) ){
    free(copy->gr_name);
    free(copy->gr_passwd);
    c_free_string_array(copy->gr_mem);
    return NULL;
  }
  copy->gr_gid = orig->gr_gid;
  return(copy);
}

static void
group_cleaner(uv_req_t * req)
{
  struct worker_params * w = req->data;
  struct group *gr = w->p1;
  if ( gr != NULL ){
    free(gr->gr_name);
    free(gr->gr_passwd);
    c_free_string_array(gr->gr_mem);
    free(gr);
  }
  w->p1 = NULL;
  w->p2 = NULL;
}

static value
group_camlval(uv_req_t * req)
{
  struct worker_params * w = req->data;
  struct group *entry = w->p1;
  value erg;
  if ( entry == NULL ){
    value t = Val_uwt_error(POINTER_TO_INT(w->p2));
    if ( t == VAL_UWT_ERROR_UWT_UNKNOWN ){
      t = VAL_UWT_ERROR_ENOENT;
    }
    erg = caml_alloc_small(1,Error_tag);
    Field(erg,0) = t;
  }
  else {
    value res;
    value name = Val_unit, pass = Val_unit, mem = Val_unit;

    Begin_roots3 (name, pass, mem);
    name = caml_copy_string(entry->gr_name);
    pass = caml_copy_string(entry->gr_passwd);
    mem = s_caml_copy_string_array((const char**)entry->gr_mem);
    res = caml_alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = pass;
    Field(res, 2) = Val_int(entry->gr_gid);
    Field(res, 3) = mem;
    name = res;
    pass = caml_alloc_small(1,Ok_tag);
    Field(pass,0) = name;
    erg = pass;
    End_roots();
  }
  return erg;
}

static void
getgrnam_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int e;
  struct group *res = NULL;
#ifdef HAVE_GETGRNAM_R
  char buf[ALLOCA_SIZE];
  struct group result;
  e = getgrnam_r((char*)w->p1,&result, buf, ALLOCA_SIZE, &res);
  if ( e != 0 ){
    res = NULL;
  }
  if ( e == 1 || (e == 0 && res == NULL) || e < 0 ){
    e = UV_ENOENT;
  }
#else
  errno = 0;
  res = getgrnam((char*)w->p1);
  e = errno == 0 ? UV_ENOENT : -errno;
#endif
  free(w->p1);
  if ( res == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(e);
  }
  else {
    w->p1 = dup_group(res);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_getgrnam(value o_name, value o_uwt)
{
  value ret;
  char * cname;
  char * oname = String_val(o_name);
  if ( oname == NULL && *oname == 0 ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
    goto endp;
  }
  cname = strdup(oname);
  if ( cname == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
    goto endp;
  }
  ret = uwt_add_worker_result(o_uwt,
                              group_cleaner,
                              getgrnam_worker,
                              group_camlval,
                              cname,
                              NULL);
endp:
  return ret;
}

#else
F_EUNAVAIL2(getgrnam)
#endif

#if defined(HAVE_GETGRGID_R) || defined(HAVE_GETGRGID)
static void
getgrgid_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int e;
  struct group *res = NULL;
  uid_t uid = POINTER_TO_INT(w->p2);
#ifdef HAVE_GETGRGID_R
  char buf[ALLOCA_SIZE];
  struct group result;
  e = getgrgid_r(uid,&result, buf, ALLOCA_SIZE, &res);
  if ( e != 0 ){
    res = NULL;
  }
  if ( e == 1 || (e == 0 && res == NULL) || e < 0 ){
    e = UV_ENOENT;
  }
#else
  errno = 0;
  res = getgrgid(uid);
  e = errno == 0 ? UV_ENOENT : -errno;
#endif
  if ( res == NULL ){
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(e);
  }
  else {
    w->p1 = dup_group(res);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
    }
  }
}

CAMLprim value
uwt_getgrgid(value o_gid, value o_uwt)
{
  value ret;
  void * gid = INT_TO_POINTER(Long_val(o_gid));
  ret = uwt_add_worker_result(o_uwt,
                              group_cleaner,
                              getgrgid_worker,
                              group_camlval,
                              NULL,
                              gid);
  return ret;
}
#else
F_EUNAVAIL2(getgrgid)
#endif

#ifdef HAVE_CHROOT
static void
chroot_worker(uv_work_t * req)
{
  struct worker_params * w = req->data;
  int er ;
  errno = 0;
  er = chroot((char*)w->p1);
  if ( er != 0 ){
    w->p2 = INT_TO_POINTER(-errno);
  }
  else {
    w->p2 = 0;
  }
}

CAMLprim value
uwt_chroot(value o_name, value o_uwt)
{
  value ret;
  char * cname;
  char * oname = String_val(o_name);
  if ( oname == NULL && *oname == 0 ){
    ret = VAL_UWT_INT_RESULT_EINVAL;
    goto endp;
  }
  cname = strdup(oname);
  if ( cname == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
    goto endp;
  }
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              chroot_worker,
                              getunitp2_camlval,
                              cname,
                              NULL);
endp:
  return ret;
}
#else
F_EUNAVAIL2(chroot)
#endif

#if defined(HAVE_LOCKF) || (defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW))
/* lockf unix code copied from lwt_unix_unix.c */
struct job_lockf {
  int64_t length;
  int fd;
  int command;
};
#elif defined(_WIN32)
struct job_lockf {
  int64_t length;
  HANDLE handle;
  int command;
};
#endif

#if defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW)
static void worker_lockf(uv_work_t * req)
{
  struct worker_params * w = req->data;
  struct job_lockf *job = w->p1;
  struct flock l;
  int result = -1;
  int error_code = 0;
  errno = 0;
  memset(&l,0,sizeof l);
  l.l_whence = 1;
  if (job->length < 0) {
    l.l_start = job->length;
    l.l_len = (off_t)-job->length;
  } else {
    l.l_start = 0L;
    l.l_len = (off_t)job->length;
  }
  switch (job->command) {
  case 0: /* F_ULOCK */
    l.l_type = F_UNLCK;
    result = fcntl(job->fd, F_SETLK, &l);
    error_code = errno;
    break;
  case 1: /* F_LOCK */
    l.l_type = F_WRLCK;
    result = fcntl(job->fd, F_SETLKW, &l);
    error_code = errno;
    break;
  case 2: /* F_TLOCK */
    l.l_type = F_WRLCK;
    result = fcntl(job->fd, F_SETLK, &l);
    error_code = errno;
    break;
  case 3: /* F_TEST */
    l.l_type = F_WRLCK;
    result = fcntl(job->fd, F_GETLK, &l);
    if (result != -1) {
      if (l.l_type == F_UNLCK) {
        result = 0;
      } else {
        result = -1;
        error_code = EACCES;
      }
    }
    break;
  case 4: /* F_RLOCK */
    l.l_type = F_RDLCK;
    result = fcntl(job->fd, F_SETLKW, &l);
    error_code = errno;
    break;
  case 5: /* F_TRLOCK */
    l.l_type = F_RDLCK;
    result = fcntl(job->fd, F_SETLK, &l);
    error_code = errno;
    break;
  default:
    result = -1;
    error_code = EINVAL;
  }
  if ( result == -1 ){
    int ec = error_code != 0 ? -error_code : UV_UNKNOWN;
    w->p2 = INT_TO_POINTER(ec);
  }
}
#elif defined(HAVE_LOCKF)
static int lock_command_table[] = {
  F_ULOCK, F_LOCK, F_TLOCK, F_TEST, F_LOCK, F_TLOCK
};
static void worker_lockf(uv_work_t * req)
{
  struct worker_params * w = req->data;
  struct job_lockf *job = w->p1;
  int result;
  errno = 0;
  result = lockf(job->fd, lock_command_table[job->command], job->length);
  if ( result == -1 ){
    int ec = errno != 0 ? -errno : UV_UNKNOWN;
    w->p2 = INT_TO_POINTER(ec);
  }
}
#elif defined(_WIN32)
/* copied from lockf.c */

static int set_file_pointer(HANDLE h, LARGE_INTEGER gohere,
                             PLARGE_INTEGER output, DWORD method)
{
  LONG high = gohere.HighPart;
  DWORD ret = SetFilePointer(h, gohere.LowPart, &high, method);
  if(ret == INVALID_SET_FILE_POINTER) {
    DWORD err = GetLastError();
    if(err != NO_ERROR) {
      int ec = err == 0 ? UV_UNKNOWN : uwt_translate_sys_error(err);
      return ec;
    }
  }
  if(output != NULL) {
    output->LowPart = ret;
    output->HighPart = high;
  }
  return 0;
}

static void worker_lockf(uv_work_t * req)
{
  struct worker_params * w = req->data;
  struct job_lockf *job = w->p1;

  OVERLAPPED overlap;
  int64_t l_len;
  HANDLE h;
  LARGE_INTEGER cur_position;
  LARGE_INTEGER beg_position;
  LARGE_INTEGER lock_len;
  LARGE_INTEGER zero;
  DWORD err = NO_ERROR;
  int result;
  h = job->handle;
  l_len = job->length;

  /* No matter what, we need the current position in the file */
  zero.HighPart = zero.LowPart = 0;
  result = set_file_pointer(h, zero, &cur_position, FILE_CURRENT);
  if ( result != 0 ){
    w->p2 = INT_TO_POINTER(result);
    return;
  }

  /* All unused fields must be set to zero */
  memset(&overlap, 0, sizeof(overlap));

  if(l_len == 0) {
    /* Lock from cur to infinity */
    lock_len.QuadPart = -1;
    overlap.OffsetHigh = cur_position.HighPart;
    overlap.Offset     = cur_position.LowPart ;
  }
  else if(l_len > 0) {
    /* Positive file offset */
    lock_len.QuadPart = l_len;
    overlap.OffsetHigh = cur_position.HighPart;
    overlap.Offset     = cur_position.LowPart ;
  }
  else {
    /* Negative file offset */
    lock_len.QuadPart = - l_len;
    if (lock_len.QuadPart > cur_position.QuadPart) {
      w->p2 = INT_TO_POINTER(UV_EINVAL);
      return;
    }
    beg_position.QuadPart = cur_position.QuadPart - lock_len.QuadPart;
    overlap.OffsetHigh = beg_position.HighPart;
    overlap.Offset     = beg_position.LowPart ;
  }

  switch(job->command){
  case 0: /* F_ULOCK - unlock */
    if (! UnlockFileEx(h, 0,
                       lock_len.LowPart, lock_len.HighPart, &overlap)){
      result = -1;
      err = GetLastError();
    }
    break;
  case 1: /* F_LOCK - blocking write lock */
    if (! LockFileEx(h, LOCKFILE_EXCLUSIVE_LOCK, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap)){
      result = -1;
      err = GetLastError();
    }
    break;
  case 2: /* F_TLOCK - non-blocking write lock */
    if (! LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap)){
      result = -1;
      err = GetLastError();
    }
    break;
  case 3: /* F_TEST - check whether a write lock can be obtained */
    /*  I'm doing this by aquiring an immediate write
     * lock and then releasing it. It is not clear that
     * this behavior matches anything in particular, but
     * it is not clear the nature of the lock test performed
     * by ocaml (unix) currently. */
    if (LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY | LOCKFILE_EXCLUSIVE_LOCK, 0,
                   lock_len.LowPart, lock_len.HighPart, &overlap)) {
      UnlockFileEx(h, 0, lock_len.LowPart, lock_len.HighPart, &overlap);
    } else {
      err = GetLastError();
      result = -1;
    }
    break;
  case 4: /* F_RLOCK - blocking read lock */
    if (! LockFileEx(h, 0, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap)){
      result = -1;
      err = GetLastError();
    }
    break;
  case 5: /* F_TRLOCK - non-blocking read lock */
    if (! LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY, 0,
                     lock_len.LowPart, lock_len.HighPart, &overlap)){
      result = -1;
      err = GetLastError();
    }
    break;
  default:
    w->p2 = INT_TO_POINTER(UV_EINVAL);
    return;
  }
  if ( result != 0) {
    int ec = err == 0 ? UV_UNKNOWN : uwt_translate_sys_error(err);
    w->p2 = INT_TO_POINTER(ec);
  }
  return;
}
#endif /* #if defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW) */

#if defined(HAVE_LOCKF) || (defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW)) || defined(_WIN32)
CAMLprim value
uwt_lockf(value o_t, value o_uwt)
{
  value ret;
  struct job_lockf * p1 = malloc(sizeof *p1);
  if ( p1 == NULL ){
    ret = VAL_UWT_INT_RESULT_ENOMEM;
  }
  else {
#ifdef _WIN32
    p1->handle = Handle_val(Field(o_t,0));
#else
    p1->fd = Long_val(Field(o_t,0));
#endif
    p1->command = Long_val(Field(o_t,1));
    p1->length = Int64_val(Field(o_t,2));
    ret = uwt_add_worker_result(o_uwt,
                                free_p1,
                                worker_lockf,
                                getunitp2_camlval,
                                p1,
                                NULL);
  }
  return ret;
}
#else
F_EUNAVAIL2(lockf)
#endif

#ifndef _WIN32
static int
pipe_normal(int fds[2],bool cloexec)
{
  int er;
  int rv ;
  errno=0;
  rv = pipe(fds);
  if (rv == -1){
    return rv;
  }
  errno=0;
  if (fcntl(fds[0], F_SETFL, O_NONBLOCK) == -1 ||
      fcntl(fds[1], F_SETFL, O_NONBLOCK) == -1 ||
      (cloexec && ( fcntl(fds[0], F_SETFD, FD_CLOEXEC) == -1 ||
                    fcntl(fds[1], F_SETFD, FD_CLOEXEC) == -1 ))){
    er = errno;
    close(fds[0]);
    close(fds[1]);
    errno = er;
    rv = -1;
  }
  return rv;
}
#if defined(HAVE_PIPE2) && defined(O_CLOEXEC)
static int
my_pipe(int fds[2],bool close_exec)
{
  static int enosys = 0;
  int ec;
  int flags;
  if ( enosys ){
    return (pipe_normal(fds,close_exec));
  }
  flags = O_NONBLOCK;
  if ( close_exec ){
    flags |= O_CLOEXEC;
  }
  errno = 0;
  ec = pipe2(fds, flags);
  if ( ec == -1 && errno == ENOSYS ){
    enosys = 1;
    return (pipe_normal(fds,close_exec));
  }
  return ec;
}
#else /* #if defined(HAVE_PIPE2) && defined(O_CLOEXEC) */
#define my_pipe pipe_normal
#endif

CAMLprim value
uwt_pipe(value o_close_exec)
{
  CAMLparam0();
  CAMLlocal2(tup,ret);
  int fd[2];
  bool close_exec = Long_val(o_close_exec) == 1;
  tup = caml_alloc(2,0);
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = tup;
  if (my_pipe(fd,close_exec) == -1){
    value er = Val_uwt_error(-errno);
    Tag_val(ret) = Error_tag;
    Store_field(ret,0,er);
  }
  else {
    Field(tup,0) = Val_long(fd[0]);
    Field(tup,1) = Val_long(fd[1]);
  }
  CAMLreturn(ret);
}

#else /* #ifndef _WIN32 */
CAMLprim value
uwt_pipe(value o_close_exec)
{
  CAMLparam0();
  CAMLlocal4(tup,ret,oread,owrite);
  SECURITY_ATTRIBUTES attr;
  bool close_exec = Long_val(o_close_exec) == 1;
  HANDLE readh, writeh;

  oread = win_alloc_handle(INVALID_HANDLE_VALUE);
  owrite = win_alloc_handle(INVALID_HANDLE_VALUE);
  tup = caml_alloc_small(2,0);
  Field(tup,0) = oread;
  Field(tup,1) = owrite;
  ret = caml_alloc_small(1,Ok_tag);
  Field(ret,0) = tup;

  attr.nLength = sizeof(attr);
  attr.lpSecurityDescriptor = NULL;
  attr.bInheritHandle = close_exec ? FALSE : TRUE;
  if ( !CreatePipe(&readh, &writeh, &attr, UNIX_BUFFER_SIZE) ){
    value er = Val_uwt_error(uwt_translate_sys_error(GetLastError()));
    Tag_val(ret) = Error_tag;
    Store_field(ret,0,er);
  }
  else {
    Handle_val(oread) = readh;
    Handle_val(owrite) = writeh;
  }
  CAMLreturn(ret);
}
#endif

#ifdef _WIN32
#define BSIZE 8192
static void
realpath_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  WCHAR buf[BSIZE];
  WCHAR *name = w->p1;
  WCHAR *fullpath = NULL;
  DWORD size = GetFullPathNameW(name,BSIZE-1,buf,NULL);
  if ( size == 0 ){
    int e = uwt_translate_sys_error(GetLastError());
    free(w->p1);
    w->p1 = NULL;
    w->p2 = INT_TO_POINTER(e);
    return;
  }
  if ( size < BSIZE - 1 ){
    fullpath = buf;
  }
  else {
    fullpath = malloc(size * sizeof(*fullpath));
    if ( fullpath == NULL ){
      free(w->p1);
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(UV_ENOMEM);
      return;
    }
    DWORD size2 = GetFullPathNameW(name,size,fullpath,NULL);
    if ( size2 == 0 || size2 > size ){
      int e ;
      if ( size == 0 ){
        e = uwt_translate_sys_error(GetLastError());
      }
      else {
        e = UV_UNKNOWN;
      }
      free(w->p1);
      w->p1 = NULL;
      w->p2 = INT_TO_POINTER(e);
      return;
    }
  }
  DWORD h = GetFileAttributesW(fullpath);
  if ( h == INVALID_FILE_ATTRIBUTES ){
    DWORD er = GetLastError();
    switch ( er ){
    case ERROR_BAD_NETPATH: /* fall */
    case ERROR_BAD_PATHNAME:  /* fall */
    case ERROR_FILE_NOT_FOUND: /* fall */
    case ERROR_INVALID_DRIVE:  /* fall */
    case ERROR_INVALID_NAME:  /* fall */
    case ERROR_INVALID_PARAMETER:  /* fall */
    case ERROR_NOT_READY:  /* fall */
    case ERROR_PATH_NOT_FOUND:  /* fall */
      w->p2 = INT_TO_POINTER(UV_ENOENT);
      break;
    default:
      w->p2 = INT_TO_POINTER(uwt_translate_sys_error(er));
    }
    free(w->p1);
    w->p1 = NULL;
  }
  else {
    free(w->p1);
    int er;
    w->p1 = uwt_utf16_to_utf8(fullpath,&er);
    if ( w->p1 == NULL ){
      w->p2 = INT_TO_POINTER(er);
    }
    else {
      w->p2 = NULL;
    }
  }
  if ( fullpath != buf ){
    free(fullpath);
  }
}
#undef BSIZE
#else /* #ifdef _WIN32 */

static void
realpath_worker(uv_work_t *req)
{
  struct worker_params * w = req->data;
  char *name = w->p1;
  char * fp;
#ifdef HAVE_CANONICALIZE_FILE_NAME
  errno = 0;
  fp = canonicalize_file_name(name);
#elif defined(HAVE_REALPATH_NULL)
  errno = 0;
  fp = realpath(name,NULL);
#else
#ifdef PATH_MAX
  char buf[UMAX(PATH_MAX,ALLOCA_SIZE)];
#elif defined(MAXPATHLEN)
  char buf[UMAX(MAXPATHLEN,ALLOCA_SIZE)];
#elif defined(NAME_MAX)
  char buf[UMAX(NAME_MAX,ALLOCA_SIZE)];
#else
#error "PATH_MAX not defined!"
#endif /* PATH_MAX */
  errno = 0;
  fp = realpath(name,buf);
  if ( fp != NULL ){
    fp = strdup(fp);
    if ( fp == NULL ){
      errno = -UV_ENOMEM;
    }
  }
#endif /* HAVE_CANONICALIZE_FILE_NAME */
  if ( fp == NULL ){
    w->p2 = INT_TO_POINTER(-errno);
  }
  else {
    w->p2 = NULL;
  }
  free(w->p1);
  w->p1 = fp;
}
#endif /* #ifdef _WIN32 */

CAMLprim value
uwt_realpath(value o_name, value o_uwt)
{
  value ret;
  const char * mname = String_val(o_name);
  void * name;
  if ( mname == NULL || *mname == '\0' ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
#ifndef _WIN32
  if ( (name = strdup(mname)) == NULL ){
    return VAL_UWT_INT_RESULT_ENOMEM;
  }
#else
  int er;
  name = uwt_utf8_to_utf16(mname,&er);
  if ( name == NULL ){
    er = uwt_translate_sys_error(er);
    return (Val_uwt_int_result(er));
  }
#endif
  ret = uwt_add_worker_result(o_uwt,
                              free_p1,
                              realpath_worker,
                              getstrp1_camlval,
                              name,
                              NULL);
  return ret;
}
