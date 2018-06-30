/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

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

static void
cb_getaddrinfo (uv_getaddrinfo_t* req,
                int status,
                struct addrinfo* info)
{
  REQ_CB_INIT(req);
  value ifo;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
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
  uv_freeaddrinfo(info);
  REQ_CB_CALL(ifo);
}

CAMLprim value
uwt_getaddrinfo(value o_node,
                value o_serv,
                value o_opts,
                value o_loop,
                value o_cb)
{
  uv_loop_t * loop = Uv_loop_val(o_loop);
  if (unlikely( !uwt_is_safe_string(o_node) || !uwt_is_safe_string(o_serv) )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_ECHARSET);
  }
  char * node = String_val(o_node);
  char * serv = String_val(o_serv);
  if (unlikely( *node == '\0' && *serv == '\0' )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_EINVAL);
  }
  CAMLparam3(o_node,o_serv,o_cb);
  int erg;
  struct addrinfo hints;
  memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(o_opts); o_opts = Field(o_opts, 1) ){
    value v = Field(o_opts, 0);
    if ( Is_block(v) ){
      const unsigned int i = Long_val(Field(v, 0));
      switch ( Tag_val(v) ){
      case 0: /* AI_FAMILY of socket_domain */
        hints.ai_family = socket_domain_table[i];
        break;
      case 1: /* AI_SOCKTYPE of socket_type */
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
  value o_ret;
  struct req * req = uwt__req_create_res(UV_GETADDRINFO, &o_ret);
  node = String_val(o_node);
  serv = String_val(o_serv);
  if ( *node == '\0' ){
    node = NULL;
  }
  if ( *serv == '\0' ){
    serv = NULL;
  }
  erg = uv_getaddrinfo(loop,
                       (uv_getaddrinfo_t*)req->req,
                       cb_getaddrinfo,
                       node,
                       serv,
                       &hints);
  if ( erg >= 0 ){
    uwt__gr_register(&req->cb,o_cb);
  }
  else {
    uwt__req_free(req);
    Field(o_ret,0) = Val_uwt_error(erg);
    Tag_val(o_ret) = Error_tag;
  }
  CAMLreturn(o_ret);
}

static const int
getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

static void
cb_getnameinfo(uv_getnameinfo_t* req, int status,
               const char* hostname, const char* service)
{
  REQ_CB_INIT(req);
  value ifo;
  if ( status < 0 ){
    ifo = caml_alloc_small(1,Error_tag);
    Field(ifo,0) = Val_uwt_error(status);
  }
  else {
    value tmp1 = Val_unit;
    value tmp2 = Val_unit;
    ifo = Val_unit;
    Begin_roots3(tmp1,tmp2,ifo);
    tmp1 = s_caml_copy_string(hostname);
    tmp2 = s_caml_copy_string(service);
    ifo = caml_alloc_small(2,0);
    Field(ifo,0) = tmp1;
    Field(ifo,1) = tmp2;
    tmp1 = caml_alloc_small(1,Ok_tag);
    Field(tmp1,0) = ifo;
    ifo = tmp1;
    End_roots();
  }
  REQ_CB_CALL(ifo);
}

CAMLprim value
uwt_getnameinfo(value o_sockaddr, value o_list, value o_loop, value o_cb)
{
  struct sockaddr_storage addr;
  if (unlikely( !uwt__get_sockaddr(o_sockaddr,(struct sockaddr *)&addr) )){
    return uwt__alloc_eresult(VAL_UWT_ERROR_UNKNOWN);
  }
  CAMLparam1(o_cb);
  uv_loop_t * loop = Uv_loop_val(o_loop);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_list, getnameinfo_flag_table);
  GR_ROOT_ENLARGE();
  value o_ret;
  struct req * req = uwt__req_create_res(UV_GETNAMEINFO, &o_ret);
  const int erg = uv_getnameinfo(loop,
                                 (uv_getnameinfo_t*)req->req,
                                 cb_getnameinfo,
                                 /* copied to internal storage by libuv */
                                 (struct sockaddr *)&addr,
                                 flags);
  if ( erg >= 0 ){
    uwt__gr_register(&req->cb,o_cb);
  }
  else {
    uwt__req_free(req);
    Field(o_ret,0) = Val_uwt_error(erg);
    Tag_val(o_ret) = Error_tag;
  }
  CAMLreturn(o_ret);
}
