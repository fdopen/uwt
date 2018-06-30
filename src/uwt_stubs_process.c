/* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. */

#include "uwt_stubs_process.h"

CAMLprim value
uwt_disable_stdio_inheritance_na(value unit){
  (void) unit;
  uv_disable_stdio_inheritance();
  return Val_unit;
}

static char **
caml_string_array_to_c_array(value p, int * e)
{
  const size_t len = Wosize_val(p);
  if ( len == 0 ){
    *e = UV_EINVAL;
    return NULL;
  }
  char ** env = malloc( (len + 1) * sizeof(char*) );
  if ( !env ){
    *e = UV_ENOMEM;
    return NULL;
  }
  size_t i;
  for ( i = 0; i < len; i++ ){
    value s = Field(p,i);
    if ( !uwt_is_safe_string(s) ){
      *e = UV_ECHARSET;
      free(env);
      return NULL;
    }
    env[i] = String_val(s);
  }
  env[len] = NULL;
  *e = 0;
  return env;
}

static struct handle *
get_handle(value o_s)
{
  if ( !Is_block(o_s) || Wosize_val(o_s) != 4 ){
    return NULL;
  }
  struct handle * s = Handle_val(o_s);
  if ( HANDLE_IS_INVALID(s) ){
    return NULL;
  }
  return s;
}

static void
spawn_exit_cb(uv_process_t*t, int64_t exit_status, int term_signal)
{
  HANDLE_CB_INIT(h, t);
  value exn = Val_unit;
  h->x.process_finished = 1;
  /* exit_cb is only optional in OCaml, we use it here to cleanup (or not...) */
  if ( h->cb_read != CB_INVALID && h->cb_listen != CB_INVALID ){
    const int o_signal = uwt__rev_convert_signal_number(term_signal);
    value callback = GET_CB_VAL(h->cb_read);
    value process = GET_CB_VAL(h->cb_listen);
    uwt__gr_unregister(&h->cb_read);
    uwt__gr_unregister(&h->cb_listen);
    exn=caml_callback3_exn(callback,
                           process,
                           Val_long(exit_status),
                           Val_long(o_signal));
  }
  --h->in_use_cnt;
  HANDLE_CB_RET(exn);
}

static value
close_tmp_fds(value tmp)
{
  CAMLparam1(tmp);
  CAMLlocal1(cur);
  int i;
  for ( i = 0; i < 3; ++i ){
    cur = Field(tmp,i);
    if ( cur == Val_unit ){
      continue;
    }
    cur = Field(cur,0);
    struct handle * h;
    const int tag = Tag_val(cur);
    cur = Field(cur,0);
    if ( tag == 0 ){
      continue;
    }
    h = get_handle(cur);
    if ( h == NULL ){
      continue;
    }
    switch(tag){
    case 2: /* fall */ /* Inherit_pipe */
    case 3: /* Inherit_stream */
      continue;
    case 1:  /* Create_pipe */
    case 4: /* Create_pipe_read */
    case 5: /* Create_pipe_write */
    case 6: /* Create_pipe_duplex */
      break;
    default:
      assert(false);
    }
    uv_os_fd_t fd;
    int r = uv_fileno(h->handle,&fd);
    if ( r < 0 ){
      continue;
    }
    r = uwt__pipe_handle_reinit(h);
    if ( r != 0 ){
      /* unfortune case, the user pipe's handle
         is invalid now. This can only happen
         in case of a malloc failure. uv_pipe_init
         can't fail */
      uwt_close_nowait(cur);
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value
uwt_spawn(value p1, value p2, value p3, value p4)
{
  INIT_LOOP_RESULT(loop,Field(p1,0));
  uv_loop_t * l = &loop->loop;
  CAMLparam4(p1,p2,p3,p4);
  unsigned int i;
  int erg = UV_UWT_EFATAL;
  bool spawn_called = false;
  uv_process_options_t t;
  uv_stdio_container_t stdio[3];

  GR_ROOT_ENLARGE();
  value ret = uwt__handle_res_create(UV_PROCESS, true);
  value op = Field(ret,0);
  struct handle * handle = Handle_val(op);

  /* below: now further caml allocations */
  memset(&t, 0, sizeof t);
  memset(&stdio, 0, sizeof stdio);

  value tmp = Field(p1,1);

  for ( i = 0; i < 3; ++i ){ /* keep in sync with close_tmp_fds! */
    value cur = Field(tmp,i);
    if ( cur == Val_unit ){
      stdio[i].flags = UV_IGNORE;
      stdio[i].data.stream = NULL;
    }
    else {
      cur = Field(cur,0);
      struct handle * h;
      const int tag = Tag_val(cur);
      cur = Field(cur,0);
      if ( tag == 0 ){
        stdio[i].flags = UV_INHERIT_FD;
        stdio[i].data.fd = FD_VAL(cur);
      }
      else {
        h = get_handle(cur);
        if ( h == NULL ){
          erg = UV_EBADF;
          goto error_end;
        }
        stdio[i].data.stream = (uv_stream_t*)h->handle;
        switch(tag){
        case 1:  /* Create_pipe */
          if ( i == 0 ){
            stdio[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
          }
          else {
            stdio[i].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
          }
          break;
        case 2: /* fall */ /* Inherit_pipe */
        case 3: /* Inherit_stream */
          if ( h->initialized == 0 ){
            erg = UV_EINVAL;
            goto error_end;
          }
          stdio[i].flags = UV_INHERIT_STREAM;
          break;
        case 4: /* Create_pipe_read */
          stdio[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE;
          break;
        case 5: /* Create_pipe_write */
          stdio[i].flags = UV_CREATE_PIPE | UV_WRITABLE_PIPE;
          break;
        case 6: /* Create_pipe_duplex */
          stdio[i].flags = UV_CREATE_PIPE | UV_READABLE_PIPE | UV_WRITABLE_PIPE;
          break;
        default:
          assert(false);
        }
        if ( h->initialized == 1 && (stdio[i].flags & UV_CREATE_PIPE) != 0 ){
          erg = UV_EINVAL;
          goto error_end;
        }
      }
    }
  }
  t.exit_cb = spawn_exit_cb;

  tmp = Field(p4,0) ;
  if ( !uwt_is_safe_string(tmp) ){
    erg = UV_ECHARSET;
    goto error_end;
  }
  t.file = String_val(tmp);
  if ( *(t.file) == '\0' ){
    erg = UV_EINVAL;
    goto error_end;
  }

  tmp = Field(p4,1);
  if ( Wosize_val(tmp) == 0 ){
    t.args = NULL;
  }
  else {
    t.args = caml_string_array_to_c_array(tmp,&erg);
    if ( t.args == NULL ){
      goto error_end;
    }
  }

  tmp = Field(p2,0);
  if ( Wosize_val(tmp) == 0 ){
    t.env =  NULL;
  }
  else {
    t.env = caml_string_array_to_c_array(tmp,&erg);
    if ( !t.env ){
      goto error_end;
    }
  }
  tmp = Field(p2,1);
  if ( tmp == Val_unit ){
    t.cwd = NULL;
  }
  else {
    tmp = Field(tmp,0);
    if ( !uwt_is_safe_string(tmp) ){
      erg = UV_ECHARSET;
      goto error_end;
    }
    t.cwd = String_val(tmp);
  }
  t.flags = Long_val(Field(p1,3));
  t.stdio_count = 3;
  t.stdio = stdio;
  intnat p_overflow = Long_val(Field(Field(p1,2),0));
  t.uid = p_overflow;

  DISABLE_WARNING_TYPE_LIMIT();
  if ( t.uid != p_overflow || ((uv_uid_t)-1 >= 0 && p_overflow < 0 )){
    erg = UV_EINVAL;
    goto error_end;
  }
  POP_WARNING();

  p_overflow = Long_val(Field(Field(p1,2),1));
  t.gid = p_overflow;

  DISABLE_WARNING_TYPE_LIMIT();
  if ( t.gid != p_overflow || ((uv_gid_t)-1 >= 0 && p_overflow < 0 )){
    erg = UV_EINVAL;
    goto error_end;
  }
  POP_WARNING();

  spawn_called = true;
  erg = uv_spawn(l, (uv_process_t*)handle->handle, &t);
  if ( erg < 0 ){
    /* uv_process_init is called internally first, see also:
       https://groups.google.com/forum/message/raw?msg=libuv/DUBr8DtzsWk/hw11ob9sPZ4J */
    uwt__handle_finalize_close(handle);
  }
  else {
    for ( i = 0; i < 3; ++i ){
      if ( (stdio[i].flags & UV_CREATE_PIPE) != 0 ){
        struct handle * h = stdio[i].data.stream->data;
        h->initialized = 1;
      }
    }
  }
error_end:
  if ( t.args ){
    free(t.args);
  }
  if ( t.env ){
    free(t.env);
  }
  if ( erg < 0 ){
    if ( spawn_called == false ){
      uwt__free_handle(handle);
    }
    else {
      close_tmp_fds(Field(p1,1));
    }
    Field(op,1) = 0;
    Tag_val(ret) = Error_tag;
    Field(ret,0) = Val_uwt_error(erg);
  }
  else {
    if ( Is_block(p3) ){
      uwt__gr_register(&handle->cb_read,Field(p3,0));
      uwt__gr_register(&handle->cb_listen,op);
    }
    handle->initialized = 1;
    ++handle->in_use_cnt;
  }
  CAMLreturn(ret);
}

CAMLprim value
uwt_pid_na(value o_h)
{
  HANDLE_INIT_NA(h, o_h);
  uv_process_t * p = (uv_process_t *)h->handle;
  return (Val_long(p->pid));
}

CAMLprim value
uwt_process_kill_na(value o_h,value o_sig)
{
  HANDLE_INIT_NA(h, o_h);
  INT_VAL_RET_IR_EINVAL(sig, o_sig);
  if ( h->x.process_finished == 1 ){
    return VAL_UWT_INT_RESULT_EINVAL;
  }
  uv_process_t * p = (uv_process_t *)h->handle;
  int signum = uwt__convert_signal_number(sig);
  int ret = uv_process_kill(p,signum);
  return (VAL_UWT_UNIT_RESULT(ret));
}

CAMLprim value
uwt_kill_na(value o_pid,value o_sig)
{
  INT_VAL_RET_IR_EINVAL(pid, o_pid);
  INT_VAL_RET_IR_EINVAL(sig, o_sig);
  int signum = uwt__convert_signal_number(sig);
  int ret = uv_kill(pid, signum);
  return (VAL_UWT_UNIT_RESULT(ret));
}
