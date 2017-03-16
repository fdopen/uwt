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

#include "uwt_stubs_fs_event.h"

static void
event_cb(uv_fs_event_t* handle,
         const char* filename,
         int events,
         int status)
{
  HANDLE_CB_INIT(handle);
  value ret = Val_unit;
  struct handle * h = handle->data;
  if ( h->cb_read == CB_INVALID || h->cb_listen == CB_INVALID ){
    DEBUG_PF("callback lost");
  }
  else {
    value param;
    if ( status < 0 ){
      param = caml_alloc_small(1,Error_tag);
      Field(param,0) = Val_uwt_error(status);
    }
    else {
      value list = Val_unit;
      value str = Val_unit;
      value tup = Val_unit;
      Begin_roots3(list,str,tup);
      if ( events & UV_RENAME ){
        tup = caml_alloc_small(2,0);
        Field(tup,0) = Val_long(0);
        Field(tup,1) = list;
        list = tup;
      }
      if ( events & UV_CHANGE ){
        tup = caml_alloc_small(2,0);
        Field(tup,0) = Val_long(1);
        Field(tup,1) = list;
        list = tup;
      }
      str = s_caml_copy_string(filename);
      tup = caml_alloc_small(2,0);
      Field(tup,0) = str;
      Field(tup,1) = list;
      param = caml_alloc_small(1,Ok_tag);
      Field(param,0) = tup;
      End_roots();
    }
    value cb = GET_CB_VAL(h->cb_read);
    value t = GET_CB_VAL(h->cb_listen);
    ret = caml_callback2_exn(cb,t,param);
  }
  HANDLE_CB_RET(ret);
}

static const int fs_event_flags[3] = {
  UV_FS_EVENT_WATCH_ENTRY,
  UV_FS_EVENT_STAT,
  UV_FS_EVENT_RECURSIVE };

CAMLprim value
uwt_fs_event_start(value o_loop,
                   value o_path,
                   value o_flags,
                   value o_cb)
{
  INIT_LOOP_RESULT(l,o_loop);
  CAMLparam3(o_loop,o_path,o_cb);
  CAMLlocal2(ret,v);
  const int flags = SAFE_CONVERT_FLAG_LIST(o_flags,fs_event_flags);
  if ( !uwt_is_safe_string(o_path) ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_ECHARSET;
  }
  else if ( String_val(o_path)[0] == '\0' ){
    ret = caml_alloc_small(1,Error_tag);
    Field(ret,0) = VAL_UWT_ERROR_EINVAL;
  }
  else {
    GR_ROOT_ENLARGE();
    v = uwt__handle_create(UV_FS_EVENT,l);
    struct handle * h = Handle_val(v);
    h->close_executed = 1;
    ret = caml_alloc_small(1,Ok_tag);
    Field(ret,0) = v;
    h->close_executed = 0;
    uv_fs_event_t * f = (uv_fs_event_t*)h->handle;
    int erg = uv_fs_event_init(&l->loop,f);
    if ( erg < 0 ){
      uwt__free_mem_uv_handle_t(h);
      uwt__free_struct_handle(h);
    }
    else {
      erg = uv_fs_event_start(f,event_cb,String_val(o_path),flags);
      if ( erg < 0 ){
        h->finalize_called = 1;
        uwt__handle_finalize_close(h);
      }
      else {
        ++h->in_use_cnt;
        h->initialized = 1;
        uwt__gr_register(&h->cb_read,o_cb);
        uwt__gr_register(&h->cb_listen,v);
      }
    }
    if ( erg < 0 ){
      Field(v,1) = 0;
      Tag_val(ret) = Error_tag;
      Field(ret,0) = Val_uwt_error(erg);
    }
  }
  CAMLreturn(ret);
}
