/* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
 * Copyright (C) 2015-2016 Andreas Hauptmann
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

#include "uwt_stubs_dns.h"

static value
cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for ( i = 0; i < size; i++ ){
    if ( n == tbl[i] ){
      return Val_long(i);
    }
  }
  return Val_long(deflt);
}

/* must be kept in sync with getaddrinfo.c and socket.c from
   the ocaml distribution.
*/
static int
socket_domain_table[] = {
  PF_UNIX, PF_INET,
#if defined(PF_INET6)
  PF_INET6
#elif defined(PF_UNDEF)
  PF_UNDEF
#else
  0
#endif
};

static int
socket_type_table[] = {
  SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET
};

/* copied from getaddrinfo.c, modified for vaddr */
static value
convert_addrinfo(struct addrinfo * a)
{
  CAMLparam0();
  CAMLlocal3(vres,vaddr,vcanonname);

  vaddr = uwt__alloc_sockaddr(a->ai_addr);
  if ( vaddr == Val_unit ){
    vres = Val_unit;
    goto endp;
  }
  vcanonname = s_caml_copy_string(a->ai_canonname);
  vres = caml_alloc_small(5, 0);
  Field(vres, 0) = cst_to_constr(a->ai_family, socket_domain_table, 3, 0);
  Field(vres, 1) = cst_to_constr(a->ai_socktype, socket_type_table, 4, 0);
  Field(vres, 2) = Val_long(a->ai_protocol);
  Field(vres, 3) = vaddr;
  Field(vres, 4) = vcanonname;
endp:
  CAMLreturn(vres);
}

static value
ret_addrinfo_list(uv_req_t * rdd)
{
  value ifo;
  struct req * wp = rdd->data;
  const int status = wp->c_param;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
    struct addrinfo* info = wp->c.p1;
    struct addrinfo * r;
    bool error_found = false;
    value e = Val_long(0);
    value v = Val_long(0);
    value vres = Val_long(0);
    value list_head = Val_long(0);
    Begin_roots4(e,v,vres,list_head);
    for ( r = info; r != NULL; r = r->ai_next ){
      e = convert_addrinfo(r);
      if ( e == Val_unit ){
        error_found = true;
        continue;
      }
      v = caml_alloc_small(2, 0);
      Field(v, 0) = e;
      Field(v, 1) = Val_long(0);
      if ( vres != Val_long(0) ){
        Store_field(vres,1,v);
      }
      else {
        list_head = v;
      }
      vres = v;
    }
    if ( list_head == Val_long(0) && error_found == true ) {
      ifo = caml_alloc_small(1,Error_tag);
      Field(ifo,0) = VAL_UWT_ERROR_UNKNOWN;
    }
    else {
      ifo = caml_alloc_small(1,Ok_tag);
      Field(ifo,0) = list_head;
    }
    End_roots();
  }
  return ifo;
}

static void
clean_addrinfo(uv_req_t * r)
{
  struct req * wp = r->data;
  if ( wp && wp->c.p1 != NULL ){
    uv_freeaddrinfo(wp->c.p1);
    wp->c.p1 = NULL;
  }
}

static void
cb_getaddrinfo (uv_getaddrinfo_t* req,
                int status,
                struct addrinfo* res)
{
  struct req * r = req->data;
  if ( r ){
    r->c_param = status;
    r->c.p1 = res;
  }
  uwt__req_callback((void*)req);
}

CAMLprim value
uwt_getaddrinfo_native(value o_node,
                       value o_serv,
                       value o_opts,
                       value o_loop,
                       value o_req,
                       value o_cb)
{
  struct loop * loop = Loop_val(o_loop);
  struct req * req = Req_val(o_req);
  RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req);
  if ( !uwt_is_safe_string(o_node) || !uwt_is_safe_string(o_serv) ){
    return VAL_UWT_INT_RESULT_ECHARSET;
  }
  CAMLparam5(o_node,o_serv,o_opts,o_req,o_cb);
  int erg;
  char * node;
  char * serv;
  struct addrinfo hints;
  req->cb_type = loop->loop_type;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(o_opts); o_opts = Field(o_opts, 1) ){
    value v = Field(o_opts, 0);
    if ( Is_block(v) ){
      const unsigned int i = Long_val(Field(v, 0));
      switch ( Tag_val(v) ){
      case 0: /* AI_FAMILY of socket_domain */
        if ( i >= AR_SIZE(socket_domain_table) ){
          erg = UV_EINVAL;
          goto einval;
        }
        hints.ai_family = socket_domain_table[i];
        break;
      case 1: /* AI_SOCKTYPE of socket_type */
        if ( i >= AR_SIZE(socket_type_table) ){
          erg = UV_EINVAL;
          goto einval;
        }
        hints.ai_socktype = socket_type_table[i];
        break;
      case 2: /* AI_PROTOCOL of int */
        hints.ai_protocol = i;
        break;
      }
    }
    else {
      switch ( Long_val(v) ){
      case 0: /* AI_NUMERICHOST */
        hints.ai_flags |= AI_NUMERICHOST; break;
      case 1: /* AI_CANONNAME */
        hints.ai_flags |= AI_CANONNAME; break;
      case 2: /* AI_PASSIVE */
        hints.ai_flags |= AI_PASSIVE; break;
      }
    }
  }
  GR_ROOT_ENLARGE();

  if ( caml_string_length(o_node) == 0 ){
    node = NULL;
  }
  else {
    node = String_val(o_node);
  }
  if ( caml_string_length(o_serv) == 0 ){
    serv = NULL;
  }
  else {
    serv = String_val(o_serv);
  }

  erg = uv_getaddrinfo(&loop->loop,
                       (uv_getaddrinfo_t*)req->req,
                       cb_getaddrinfo,
                       node,
                       serv,
                       &hints);
 einval:
  if ( erg < 0 ){
    Field(o_req,1) = 0;
    uwt__req_free(req);
  }
  else {
    uwt__gr_register(&req->cb,o_cb);
    req->in_use = 1;
    req->c_cb = ret_addrinfo_list;
    req->clean_cb = clean_addrinfo;
  }
  CAMLreturn(VAL_UWT_UNIT_RESULT(erg));
}
BYTE_WRAP6(uwt_getaddrinfo)

static const int
getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

static void
cb_getnameinfo(uv_getnameinfo_t* req, int status,
               const char* hostname, const char* service)
{
  struct req * r = req->data;
  if ( r ){ /* does not work synchronously */
    r->c_param = status;
    r->c.p1 = (void *)hostname;
    r->c.p2 = (void *)service;
  }
  uwt__req_callback((void*)req);
}

static value
ret_getnameinfo(uv_req_t * rdd)
{
  value ifo;
  struct req * wp = rdd->data;
  const int status = wp->c_param;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
    value tmp1 = Val_unit;
    value tmp2 = Val_unit;
    ifo = Val_unit;
    Begin_roots3(tmp1,tmp2,ifo);
    tmp1 = s_caml_copy_string(wp->c.p1);
    tmp2 = s_caml_copy_string(wp->c.p2);
    ifo = caml_alloc_small(2,0);
    Field(ifo,0) = tmp1;
    Field(ifo,1) = tmp2;
    tmp1 = caml_alloc_small(1,Ok_tag);
    Field(tmp1,0) = ifo;
    ifo = tmp1;
    End_roots();
  }
  return ifo;
}

CAMLprim value
uwt_getnameinfo(value o_sockaddr, value o_list, value o_loop,
                value o_req, value o_cb)
{
  struct sockaddr_storage addr;
  if ( !uwt__get_sockaddr(o_sockaddr,(struct sockaddr *)&addr) ){
    return VAL_UWT_INT_RESULT_UNKNOWN;
  }
  struct loop * loop = Loop_val(o_loop);
  struct req * req = Req_val(o_req);
  RETURN_INT_RESULT_INVALID_LOOP_REQ(loop,req);
  CAMLparam2(o_cb,o_req);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_list, getnameinfo_flag_table);
  GR_ROOT_ENLARGE();
  const int erg = uv_getnameinfo(&loop->loop,
                                 (uv_getnameinfo_t*)req->req,
                                 cb_getnameinfo,
                                 /* copied to internal storage by libuv */
                                 (struct sockaddr *)&addr,
                                 flags);
  value ret;
  if ( erg < 0 ){
    ret = Val_uwt_int_result(erg);
    Field(o_req,1) = 0;
    uwt__req_free(req);
  }
  else {
    uwt__gr_register(&req->cb,o_cb);
    req->in_use = 1;
    req->c_cb = ret_getnameinfo;
    ret = Val_unit;
  }
  CAMLreturn(ret);
}
