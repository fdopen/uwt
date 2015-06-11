(* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
 * Module Uwt
 * Copyright (C) 2015 Andreas Hauptmann
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * * Neither the name of the author nor the names of its contributors
 *   may be used to endorse or promote products derived from this
 *   software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Lwt.Infix
external init_stacks : unit -> unit = "uwt_init_stacks_na" "noalloc"
let () = init_stacks ()

include Uwt_base

module LInt_result = struct

  let mfail ~name ~param (x: 'a Int_result.t) =
    Lwt.fail(Int_result.to_exn ~param ~name x)

  let fail ?(name="") ?(param="") (x:'a Int_result.t) =
    if Int_result.is_error x then
      mfail ~name ~param x
    else
      Lwt.fail_invalid_arg "Uwt.Int_result.fail"
end

type 'a cb = 'a result Lwt.u

type int_cb = Int_result.int Lwt.u
type unit_cb = Int_result.unit Lwt.u
type loop

(*
type loop_mode =
  | Sync
  | Lwt
  | Cb
*)
type uv_run_mode =
  | Run_once
  | Run_nowait
  (* | Run_default *)

(* not sure, if I can leave it that simple. Lwt.wakeup will
   raise an exception, if the corresponding thread is neither sleeping
   nor canceled *)
let () = Callback.register "uwt.wakeup" Lwt.wakeup

(* external uv_loop_close: loop -> Int_result.unit = "uwt_loop_close" *)
external uv_run_loop: loop -> uv_run_mode -> Int_result.int = "uwt_run_loop"

external uv_default_loop: int -> loop result = "uwt_default_loop"
let loop =
  match uv_default_loop 1 with (* Lwt of disabled loop_mode *)
  | Error _ ->
    prerr_endline "Can't init default loop";
    exit(3)
  | Ok x -> x

let param = ""

module Req = struct
  type t
  type type' =
    | Fs
    | Getaddr
    | Getname
    | Work

  external create: loop -> type' -> t = "uwt_req_create"
  external cancel_noerr: t -> unit = "uwt_req_cancel_noerr"
  external finalize: t -> unit = "uwt_req_finalize_na" "noalloc"

  let ql ~typ ~f ~name ~param =
    let sleeper,waker = Lwt.task () in
    let req = create loop typ in
    let (x: Int_result.unit) = f loop req waker in
    if Int_result.is_error x then
      LInt_result.mfail ~name ~param x
    else
      let () = Lwt.on_cancel sleeper ( fun () -> cancel_noerr req ) in
      sleeper >>= fun x ->
      finalize req;
      match x with
      | Ok x -> Lwt.return x
      | Error x -> Lwt.fail (Uwt_error(x,name,param))

  let qlu ~typ ~f ~name ~param =
    let sleeper,waker = Lwt.task () in
    let req = create loop typ in
    let (x: Int_result.unit) = f loop req waker in
    if Int_result.is_error x then
      LInt_result.mfail ~name ~param x
    else
      let () = Lwt.on_cancel sleeper ( fun () -> cancel_noerr req ) in
      sleeper >>= fun (x: Int_result.unit) ->
      finalize req;
      if Int_result.is_error x then
        LInt_result.mfail ~name ~param x
      else
        Lwt.return_unit

  let qli ~typ ~f ~name ~param =
    let sleeper,waker = Lwt.task () in
    let req = create loop typ in
    let (x: Int_result.unit) = f loop req waker in
    if Int_result.is_error x then
      LInt_result.mfail ~name ~param x
    else
      let () = Lwt.on_cancel sleeper ( fun () -> cancel_noerr req ) in
      sleeper >>= fun (x: Int_result.int) ->
      finalize req;
      if Int_result.is_error x then
        LInt_result.mfail ~name ~param x
      else
        let x : int = (x :> int) in
        Lwt.return x

  let qlfile ~typ ~f ~name ~param =
    qli ~typ ~f ~name ~param >>= fun (i:int) ->
    Lwt.return (Obj.magic i)

end

module Fs = struct
  include Fs_types

  let typ = Req.Fs

  external openfile:
    string -> uv_open_flag list -> int ->
    loop -> Req.t -> int_cb ->
    Int_result.unit =
    "uwt_fs_open_byte" "uwt_fs_open_native"

  let openfile ?(perm=0o644) ~mode fln =
    Req.qlfile ~typ ~name:"uv_fs_open" ~param:fln ~f:(openfile fln mode perm)

  external read:
    file -> 'a -> int -> int ->
    loop -> Req.t -> int_cb ->
    Int_result.unit =
    "uwt_fs_read_byte" "uwt_fs_read_native"

  let read ?(pos=0) ?len t ~buf ~dim =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Fs.read")
    else
      Req.qli ~typ ~name:"uv_fs_read" ~param ~f:(read t buf pos len)

  let read_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    read ?pos ?len ~dim ~buf t

  let read ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    read ?pos ?len ~dim ~buf t

  external write:
    file -> 'a -> int -> int ->
    loop -> Req.t -> int_cb ->
    Int_result.unit =
    "uwt_fs_write_byte" "uwt_fs_write_native"

  let write ?(pos=0) ?len ~dim t ~buf =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Fs.write")
    else
      Req.qli ~typ ~name:"uv_fs_write" ~param ~f:(write t buf pos len)

  let write_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    write ~dim ?pos ?len t ~buf

  let write_string ?pos ?len t ~buf =
    let dim = String.length buf in
    write ~dim ?pos ?len t ~buf

  let write ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    write ~dim ?pos ?len t ~buf

  external close:
    file -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_close"

  let close fd = Req.qlu ~typ ~f:(close fd) ~name:"uv_fs_close" ~param

  external unlink:
    string -> loop -> Req.t -> unit_cb -> Int_result.unit = "uwt_fs_unlink"
  let unlink param = Req.qlu ~typ ~f:(unlink param) ~name:"uv_fs_unlink" ~param

  external mkdir:
    string -> int -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_mkdir"
  let mkdir ?(perm=0o777) param =
    Req.qlu ~typ ~f:(mkdir param perm) ~name:"uv_fs_mkdir" ~param

  external rmdir:
    string -> loop -> Req.t -> unit_cb -> Int_result.unit = "uwt_fs_rmdir"
  let rmdir param =
    Req.qlu ~typ ~f:(rmdir param) ~name:"uv_fs_rmdir" ~param

  external rename:
    string -> string -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_rename"
  let rename ~src ~dst =
    Req.qlu ~typ ~f:(rename src dst) ~name:"uv_fs_rename" ~param:src

  external link:
    string -> string -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_link"
  let link ~target ~link_name =
    Req.qlu ~typ ~f:(link target link_name) ~name:"uv_fs_link" ~param:target

  external fsync: file -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_fsync"
  let fsync file =
    Req.qlu ~typ ~f:(fsync file) ~name:"uv_fs_fsync" ~param

  external fdatasync:
    file -> loop -> Req.t -> unit_cb -> Int_result.unit = "uwt_fs_fsync"
  let fdatasync file =
    Req.qlu ~typ ~f:(fdatasync file) ~name:"uv_fs_fdatasync" ~param

  external ftruncate:
    file -> int64 -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_ftruncate"
  let ftruncate file ~len =
    Req.qlu ~typ ~f:(ftruncate file len) ~name:"uv_fs_ftruncate" ~param

  external stat:
    string -> loop -> Req.t -> stats cb -> Int_result.unit = "uwt_fs_stat"
  let stat param = Req.ql ~typ ~f:(stat param) ~name:"uv_fs_stat" ~param

  external lstat:
    string -> loop -> Req.t -> stats cb -> Int_result.unit = "uwt_fs_lstat"
  let lstat param = Req.ql ~typ ~f:(lstat param) ~name:"uv_fs_stat" ~param

  external fstat:
    file -> loop -> Req.t -> stats cb -> Int_result.unit = "uwt_fs_fstat"
  let fstat fd = Req.ql ~typ ~f:(fstat fd) ~name:"uv_fs_fstat" ~param

  external symlink:
    string -> string -> symlink_mode -> loop -> Req.t -> unit_cb ->
    Int_result.unit = "uwt_fs_symlink_byte" "uwt_fs_symlink_native"
  let symlink ?(mode=S_Default) ~src ~dst () =
    Req.qlu ~typ ~f:(symlink src dst mode) ~name:"uv_fs_symlink" ~param:dst

  external mkdtemp:
    string -> loop -> Req.t -> string cb -> Int_result.unit = "uwt_fs_mkdtemp"
  let mkdtemp param =
    Req.ql ~typ ~f:(mkdtemp param) ~name:"uv_fs_mkdtemp" ~param

  external sendfile:
    file -> file -> int64 -> int64 -> loop -> Req.t -> int64 cb ->
    Int_result.unit = "uwt_fs_sendfile_byte" "uwt_fs_sendfile_native"
  let sendfile ?(pos=0L) ?(len=Int64.max_int)  ~dst ~src () =
    Req.ql ~typ ~f:(sendfile dst src pos len) ~name:"uv_fs_sendfile" ~param

  external utime:
    string -> float -> float -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_utime_byte" "uwt_fs_utime_native"
  let utime s ~access ~modif =
    Req.qlu ~typ ~f:(utime s access modif) ~name:"uv_fs_utime" ~param:s

  external futime:
    file -> float -> float -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_futime_byte" "uwt_fs_futime_native"
  let futime fd ~access ~modif =
    Req.qlu ~typ ~f:(futime fd access modif) ~name:"uv_fs_futime" ~param

  external readlink:
    string -> loop -> Req.t -> string cb -> Int_result.unit = "uwt_fs_readlink"
  let readlink param =
    Req.ql ~typ ~f:(readlink param) ~name:"uv_fs_readlink" ~param

  external access:
    string -> access_permission list -> loop -> Req.t -> unit_cb ->
    Int_result.unit = "uwt_fs_access"
  let access s al = Req.qlu ~typ ~f:(access s al) ~name:"uv_fs_access" ~param:s

  external chmod:
    string -> int -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_chmod"
  let chmod param ~perm =
    Req.qlu ~typ ~f:(chmod param perm) ~name:"uv_fs_chmod" ~param

  external fchmod:
    file -> int -> loop -> Req.t -> unit_cb -> Int_result.unit = "uwt_fs_fchmod"
  let fchmod fd ~perm =
    Req.qlu ~typ ~f:(fchmod fd perm) ~name:"uv_fs_fchmod" ~param

  external chown:
    string -> int -> int -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_chown_byte" "uwt_fs_chown_native"
  let chown s ~uid ~gid =
    Req.qlu ~typ ~f:(chown s uid gid) ~name:"uv_fs_chown" ~param:s

  external fchown:
    file -> int -> int -> loop -> Req.t -> unit_cb -> Int_result.unit =
    "uwt_fs_fchown_byte" "uwt_fs_fchown_native"
  let fchown fd ~uid ~gid =
    Req.qlu ~typ ~f:(fchown fd uid gid) ~name:"uv_fs_fchown" ~param

  external scandir:
    string -> loop -> Req.t -> (file_kind * string) array cb ->
    Int_result.unit = "uwt_fs_scandir"
  let scandir param =
    Req.ql ~typ ~f:(scandir param) ~name:"uv_fs_scandir" ~param
end

let qsu_common ~name sleeper (x: Int_result.unit) =
  if (x :> int) < 0 then
    LInt_result.mfail ~name ~param x
  else
    sleeper >>= fun (x: Int_result.unit) ->
    if (x :> int) < 0 then
      LInt_result.mfail ~name ~param x
    else
      Lwt.return_unit

let qsu1 ~f ~name a =
  let sleeper,waker = Lwt.task () in
  let (x: Int_result.unit) = f a waker in
  qsu_common ~name sleeper x

let qsu2 ~f ~name a b =
  let sleeper,waker = Lwt.task () in
  let (x: Int_result.unit) = f a b waker in
  qsu_common ~name sleeper x

(*let qsu3 ~f ~name a b c =
  let sleeper,waker = Lwt.task () in
  let (x: Int_result.unit) = f a b c waker in
  qsu_common ~name sleeper x *)

let qsu4 ~f ~name a b c d =
  let sleeper,waker = Lwt.task () in
  let (x: Int_result.unit) = f a b c d waker in
  qsu_common ~name sleeper x

let qsu5 ~f ~name a b c d e =
  let sleeper,waker = Lwt.task () in
  let (x: Int_result.unit) = f a b c d e waker in
  qsu_common ~name sleeper x

let to_exn n = function
| Ok x -> x
| Error x -> raise (Uwt_error(x,n,param))

let to_exni name (n: Int_result.int) =
  if Int_result.is_error n then
    Int_result.raise_exn ~name ~param n
  else
    let n : int = (n :> int) in
    (n :> int)

let to_exnu name (n: Int_result.unit) =
  if Int_result.is_error n then
    Int_result.raise_exn ~name ~param n
  else
    ()

type u

module Handle = struct
  type t = u

  external close_wait: t -> unit_cb -> Int_result.unit = "uwt_close_wait"
  let close_wait t = qsu1 ~f:close_wait ~name:"uv_close" t

  external close: t -> Int_result.unit = "uwt_close_nowait"
  let close_noerr t = let _ = close t  in ()

  external is_active: t -> bool = "uwt_is_active_na" "noalloc"
end

external get_buffer_size_common:
  u -> bool -> Int_result.int = "uwt_get_buffer_size_common_na" "noalloc"

external set_buffer_size_common:
  u -> int -> bool -> Int_result.unit =
  "uwt_set_buffer_size_common_na" "noalloc"

module Handle_ext = struct
  type t = u
  let get_send_buffer_size (s:t) = get_buffer_size_common s false
  let get_send_buffer_size_exn s =
    get_send_buffer_size s |> to_exni "uv_send_buffer_size"

  let get_recv_buffer_size s = get_buffer_size_common s true
  let get_recv_buffer_size_exn s =
    get_recv_buffer_size s |> to_exni "uv_recv_buffer_size"

  let set_send_buffer_size s l = set_buffer_size_common s l false
  let set_send_buffer_size_exn s l =
    set_send_buffer_size s l |> to_exnu "uv_send_buffer_size"

  let set_recv_buffer_size s l = set_buffer_size_common s l true
  let set_recv_buffer_size_exn s l =
    set_send_buffer_size s l |> to_exnu "uv_recv_buffer_size"
end

module Stream = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  external write_queue_size : t -> int = "uwt_write_queue_size_na" "noalloc"

  external read_start:
    t  -> cb:(Bytes.t result -> unit) -> Int_result.unit = "uwt_read_start"
  let read_start_exn a ~cb = read_start a ~cb |> to_exnu "uv_read_start"

  external read_stop: t -> Int_result.unit = "uwt_read_stop"
  let read_stop_exn a = read_stop a |> to_exnu "uv_read_stop"

  external write:
    t -> 'a -> int -> int -> unit_cb -> Int_result.unit =
    "uwt_write"

  let write_raw ?(pos=0) ?len s ~buf ~dim =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    let name = "uv_write" in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Stream.write_raw")
    else
      qsu4 ~name ~f:write s buf pos len

  let write_raw_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    write_raw ~dim ?pos ?len t ~buf

  let write_raw_string ?pos ?len t ~buf =
    let dim = String.length buf in
    write_raw ~dim ?pos ?len t ~buf

  let write_raw ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    write_raw ~dim ?pos ?len t ~buf

  external try_write:
    t -> 'a -> int -> int -> Int_result.int = "uwt_try_write_na" "noalloc"

  let try_write ?(pos=0) ?len s ~buf ~dim =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Int_result.uwt_einval
    else
      try_write s buf pos len

  let write ?(pos=0) ?len s ~buf ~dim =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    let name = "uv_write" in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Stream.write")
    else
      (* always us try_write first, perhaps we don't need to create
         a sleeping thread at all. It's faster for small write requests *)
      let x' = try_write ~pos ~len s ~buf ~dim in
      let x = ( x' :> int ) in
      if x < 0 then
        if x' = Int_result.eagain then
          qsu4 ~name ~f:write s buf pos len
        else
          LInt_result.mfail ~name ~param x'
      else if x = len then
        Lwt.return_unit
      else if x > len then
        Lwt.fail(Uwt_error(UWT_EFATAL,name,""))
      else
        let pos = pos + x
        and len = len - x in
        qsu4 ~name ~f:write s buf pos len

  let write_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    write ~dim ?pos ?len t ~buf

  let write_string ?pos ?len t ~buf =
    let dim = String.length buf in
    write ~dim ?pos ?len t ~buf

  let write ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    write ~dim ?pos ?len t ~buf

  let try_write_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    try_write ~dim ?pos ?len t ~buf

  let try_write_string ?pos ?len t ~buf =
    let dim = String.length buf in
    try_write ~dim ?pos ?len t ~buf

  let try_write ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    try_write ~dim ?pos ?len t ~buf

  external read:
    t -> 'a -> int -> int -> int_cb -> Int_result.unit = "uwt_read_own"

  let read ?(pos=0) ?len t ~buf ~dim =
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Stream.read")
    else
      let sleeper,waker = Lwt.task () in
      let (x: Int_result.unit) = read t buf pos len waker in
      if Int_result.is_error x then
        LInt_result.fail ~name:"uwt_read" ~param x
      else
        let () = Lwt.on_cancel sleeper ( fun () -> read_stop t |> ignore ) in
        sleeper >>= fun ( x: Int_result.int ) ->
        if Int_result.is_error x then
          LInt_result.fail ~name:"uwt_read" ~param x
        else
          let x : int = (x :> int) in
          Lwt.return ( x :> int )

  let read_ba ?pos ?len t ~(buf:buf) =
    let dim = Bigarray.Array1.dim buf in
    read ?pos ?len ~dim ~buf t

  let read ?pos ?len t ~buf =
    let dim = Bytes.length buf in
    read ?pos ?len ~dim ~buf t

  external write2:
    t -> t -> Bytes.t -> int -> int -> unit_cb -> Int_result.unit =
    "uwt_write2_byte" "uwt_write2_native"

  let write2 ?(pos=0) ?len ~buf ~send s =
    let dim = Bytes.length buf in
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Stream.write2")
    else
      qsu5 ~name:"uv_write2" ~f:write2 s send buf pos len

  external is_readable : t -> bool = "uwt_is_readable_na" "noalloc"
  external is_writable : t -> bool = "uwt_is_writable_na" "noalloc"

  external listen:
    t -> max:int -> cb:( t -> Int_result.unit -> unit ) -> Int_result.unit =
    "uwt_listen"
  let listen_exn a ~max ~cb = listen a ~max ~cb |> to_exnu "uv_listen"

  external shutdown: t -> unit_cb -> Int_result.unit = "uwt_shutdown"
  let shutdown s = qsu1 ~name:"uv_shutdown" ~f:shutdown s

  external accept_raw:
    server:t -> client:t -> Int_result.unit = "uwt_accept_raw_na" "noalloc"

  let accept_raw_exn ~server ~client =
    accept_raw ~server ~client |> to_exnu "uv_accept_raw"

end

module Pipe = struct
  type t = u
  include (Stream: (module type of Stream) with type t := t )
  external to_stream : t -> Stream.t = "%identity"

  include (Handle_ext: (module type of Handle_ext) with type t := t)

  external e_openpipe : loop -> file -> bool -> t result = "uwt_pipe_open"
  let openpipe ?(ipc=false) f = e_openpipe loop f ipc
  let openpipe_exn ?(ipc=false) f =
    e_openpipe loop f ipc |> to_exn "uv_pipe_open"

  external e_init : loop -> bool -> t result = "uwt_pipe_init"
  let init ?(ipc=false) () =
    match e_init loop ipc with
    | Ok x -> x
    | Error ENOMEM -> raise Out_of_memory
    | Error x ->
      (* this can currently not happen. loop is initialized at program
         start - an will never be closed (UWT_EFATAL not possible).
         And uv_tcp_init always returns zero. But the libuv internals
         might change in future versions,... *)
      raise (Uwt_error(x,"uv_pipe_init",""))

  external bind:
    t -> path:string -> Int_result.unit = "uwt_pipe_bind_na" "noalloc"
  let bind_exn a ~path = bind a ~path |> to_exnu "uv_pipe_bind"

  external getsockname: t -> string result = "uwt_pipe_getsockname"
  let getsockname_exn a = getsockname a |> to_exn "uv_pipe_getsockname"

  external pending_instances:
    t -> int -> Int_result.unit = "uwt_pipe_pending_instances_na" "noalloc"
  let pending_instances_exn a b =
    pending_instances a b |> to_exnu "uv_pipe_pending_instances"

  external connect:
    t -> string -> unit_cb -> Int_result.unit = "uwt_pipe_connect"
  let connect p ~path:s = qsu2 ~name:"uv_pipe_connect" ~f:connect p s

  external pending_count:
    t -> Int_result.int = "uwt_pipe_pending_count_na" "noalloc"
  let pending_count_exn a = pending_count a |> to_exni "uv_pipe_pending_count"

  type pending_type =
    | Unknown
    | Tcp
    | Udp
    | Pipe

  external pending_type:
    t -> pending_type = "uwt_pipe_pending_type_na" "noalloc"
end

module Tty = struct
  type t = u
  include (Stream: (module type of Stream) with type t := t )
  external to_stream : t -> Stream.t = "%identity"

  external init: loop -> file -> bool -> t result = "uwt_tty_init"
  let init_exn f ~read = init loop f read |> to_exn "uv_tty_init"
  let init f ~read = init loop f read

  type mode =
    | Normal
    | Raw
    | Io

  external set_mode:
    t -> mode -> Int_result.unit = "uwt_tty_set_mode_na" "noalloc"
  let set_mode_exn t ~mode = set_mode t mode |> to_exnu "uv_tty_set_mode"
  let set_mode t ~mode =  set_mode t mode

  external reset_mode:
    unit -> Int_result.unit = "uwt_tty_reset_mode_na" "noalloc"
  let reset_mode_exn x = reset_mode x |> to_exnu "uv_tty_reset_mode"

  type winsize = {
    width: int;
    height: int;
  }
  external get_winsize:  t -> winsize result = "uwt_tty_get_winsize"
  let get_winsize_exn t = get_winsize t |> to_exn "uv_tty_get_winsize"
end

module Tcp = struct
  type t = u
  include (Stream: (module type of Stream) with type t := t )
  include (Handle_ext: (module type of Handle_ext) with type t := t)
  external to_stream : t -> Stream.t = "%identity"

  type mode =
    | Ipv6_only

  external init_raw: loop -> t result = "uwt_tcp_init"
  let init () =
    match init_raw loop with
    | Ok x -> x
    | Error ENOMEM -> raise Out_of_memory
    | Error x -> raise (Uwt_error(x,"uv_tcp_init",""))

  external opentcp:
    t -> socket -> Int_result.unit = "uwt_tcp_open_na" "noalloc"

  let opentcp s =
    let x = init_raw loop in
    match x with
    | Error _ -> x
    | Ok t ->
      let r = opentcp t s in
      if Int_result.is_ok r then
        x
      else
        Error(Int_result.to_error r)

  let opentcp_exn s = opentcp s |> to_exn "uv_tcp_open"

  external bind:
    t -> sockaddr -> mode list -> Int_result.unit = "uwt_tcp_bind_na" "noalloc"
  let bind_exn ?(mode=[]) t ~addr () = bind t addr mode |> to_exnu "uv_tcp_bind"
  let bind ?(mode=[]) t ~addr () = bind t addr mode

  external nodelay: t -> bool -> Int_result.unit = "uwt_tcp_nodelay_na" "noalloc"
  let nodelay_exn t x = nodelay t x |> to_exnu "uv_tcp_nodelay"

  external keepalive:
    t -> bool -> Int_result.unit = "uwt_tcp_keepalive_na" "noalloc"
  let keepalive_exn t x = keepalive t x |> to_exnu "uv_tcp_keepalive"

  external simultaneous_accepts: t -> bool -> Int_result.unit =
    "uwt_tcp_simultaneous_accepts_na"
  let simultaneous_accepts_exn t x =
    simultaneous_accepts t x |> to_exnu "uv_tcp_simultaneous_accepts"

  external getsockname: t -> sockaddr result = "uwt_tcp_getsockname"
  let getsockname_exn t = getsockname t |> to_exn "tcp_getsockname"

  external getpeername: t -> sockaddr result = "uwt_tcp_getpeername"
  let getpeername_exn t = getpeername t |> to_exn "tcp_getpeername"

  external connect:
    t -> sockaddr -> unit_cb -> Int_result.unit = "uwt_tcp_connect"
  let connect p ~addr = qsu2 ~name:"uv_tcp_connect" ~f:connect p addr

  let accept server =
    let x = init_raw loop in
    match x with
    | Error _ -> x
    | Ok client  ->
      let p = accept_raw ~server ~client in
      if Int_result.is_ok p then
        x
      else
        let () = close_noerr client in
        Error(Int_result.to_error p)

  let accept_exn server =
    accept server |> to_exn "uv_accept"

end

module Udp = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  include (Handle_ext: (module type of Handle_ext) with type t := t)

  external to_handle : t -> Handle.t = "%identity"

  external send_queue_size: t -> int = "uwt_udp_send_queue_size_na" "noalloc"
  external send_queue_count: t -> int = "uwt_udp_send_queue_count_na" "noalloc"

  external init_raw: loop -> t result = "uwt_udp_init"
  let init () =
    match init_raw loop with
    | Ok x -> x
    | Error ENOMEM -> raise Out_of_memory
    | Error x -> raise (Uwt_error(x,"uv_init_tcp",""))

  external openudp: t -> socket -> Int_result.unit = "uwt_udp_open_na" "noalloc"
  let openudp s =
    let x = init_raw loop in
    match x with
    | Error _ -> x
    | Ok t ->
      let r = openudp t s in
      if Int_result.is_ok r then
        x
      else
        Error(Int_result.to_error r)

  let openudp_exn s = openudp s |> to_exn "uv_udp_open"

  type mode =
    | Ipv6_only
    | Reuse_addr

  external bind:
    t -> sockaddr -> mode list -> Int_result.unit = "uwt_udp_bind_na" "noalloc"
  let bind_exn ?(mode=[]) t ~addr () = bind t addr mode |> to_exnu "uv_udp_bind"
  let bind ?(mode=[]) t ~addr () = bind t addr mode

  external getsockname: t -> sockaddr result = "uwt_udp_getsockname"
  let getsockname_exn t = getsockname t |> to_exn "udp_getsockname"

  type membership =
    | Leave_group
    | Join_group

  external set_membership:
    t -> multicast:string -> interface:string -> membership -> Int_result.unit =
    "uwt_udp_set_membership_na" "noalloc"

  let set_membership_exn t ~multicast ~interface m =
    set_membership t ~multicast ~interface m |> to_exnu "uv_udp_set_membership"

  external set_multicast_loop:
    t -> bool -> Int_result.unit = "uwt_udp_set_multicast_loop_na" "noalloc"
  let set_multicast_loop_exn a b =
    set_multicast_loop a b |> to_exnu "uv_udp_set_multicast_loop"

  external set_multicast_ttl:
    t -> int -> Int_result.unit = "uwt_udp_set_multicast_ttl_na" "noalloc"
  let set_multicast_ttl_exn a b =
    set_multicast_ttl a b |> to_exnu "uv_udp_set_multicast_ttl"

  external set_multicast_interface:
    t -> string -> Int_result.unit =
    "uwt_udp_set_multicast_interface_na" "noalloc"
  let set_multicast_interface_exn a b =
    set_multicast_interface a b |> to_exnu "uv_udp_set_multicast_interface"

  external set_broadcast:
    t -> bool -> Int_result.unit =
    "uwt_udp_set_broadcast_na" "noalloc"
  let set_broadcast_exn a b =
    set_broadcast a b |> to_exnu "uv_udp_set_broadcast"

  external set_ttl:
    t -> int -> Int_result.unit = "uwt_udp_set_ttl_na" "noalloc"
  let set_ttl_exn a b =
    set_ttl a b |> to_exnu "uv_udp_set_ttl"

  external try_send:
    t -> 'a -> int -> int -> sockaddr -> Int_result.int =
    "uwt_udp_try_send_na" "noalloc"

  let try_send ?(pos=0) ?len ~buf ~dim t s =
    let len = match len with
    | None -> dim - pos
    | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Int_result.uwt_einval
    else
      try_send t buf pos len s

  external send:
    t -> 'a -> int -> int -> sockaddr -> unit_cb -> Int_result.unit =
    "uwt_udp_send_byte" "uwt_udp_send_native"

  let send_raw ?(pos=0) ?len ~buf ~dim s addr =
    let name = "uv_udp_send" in
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Udp.send_raw")
    else
      qsu5 ~name ~f:send s buf pos len addr

  let send_raw_ba ?pos ?len ~(buf:buf) t addr =
    let dim = Bigarray.Array1.dim buf in
    send_raw ~dim ?pos ?len ~buf t addr

  let send_raw_string ?pos ?len ~buf t addr =
    let dim = String.length buf in
    send_raw ~dim ?pos ?len ~buf t addr

  let send_raw ?pos ?len ~buf t addr =
    let dim = Bytes.length buf in
    send_raw ~dim ?pos ?len ~buf t addr

  let send ?(pos=0) ?len ~buf ~dim s addr =
    let name = "uv_udp_send" in
    let len =
      match len with
      | None -> dim - pos
      | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Udp.send")
    else if Sys.win32 then (* windows doesn't support try_send *)
      qsu5 ~name ~f:send s buf pos len addr
    else
      let x' = try_send ~pos ~len ~buf ~dim s addr in
      let x = ( x' :> int ) in
      if x < 0 then
        if x' = Int_result.eagain || x' = Int_result.enosys then
          qsu5 ~name ~f:send s buf pos len addr
        else
          LInt_result.mfail ~name ~param x'
      else if x = len then
        Lwt.return_unit
      else if x > len then
        Lwt.fail(Uwt_error(UWT_EFATAL,name,""))
      else
        let pos = pos + x
        and len = len - x in
        qsu5 ~name ~f:send s buf pos len addr

  let send_ba ?pos ?len ~(buf:buf) t addr =
    let dim = Bigarray.Array1.dim buf in
    send ~dim ?pos ?len ~buf t addr

  let send_string ?pos ?len ~buf t addr =
    let dim = String.length buf in
    send ~dim ?pos ?len ~buf t addr

  let send ?pos ?len ~buf t addr =
    let dim = Bytes.length buf in
    send ~dim ?pos ?len ~buf t addr

  let try_send_string ?pos ?len ~buf t s =
    let dim = String.length buf in
    try_send ?pos ?len ~buf t s ~dim

  let try_send_ba ?pos ?len ~buf t s =
    let dim = Bigarray.Array1.dim buf in
    try_send ?pos ?len ~buf t s ~dim

  let try_send ?pos ?len ~buf t s =
    let dim = Bytes.length buf in
    try_send ?pos ?len ~buf t s ~dim

  type recv_result =
    | Data of Bytes.t * sockaddr option
    | Partial_data of Bytes.t * sockaddr option
    | Empty_from of sockaddr
    | Transmission_error of error

  external recv_start:
    t -> cb:(recv_result -> unit) -> Int_result.unit = "uwt_udp_recv_start"
  let recv_start_exn a ~cb = recv_start a ~cb |> to_exnu "udp_recv_start"

  external recv_stop: t -> Int_result.unit = "uwt_udp_recv_stop"
  let recv_stop_exn a = recv_stop a |> to_exnu "uv_udp_recv_stop"

  type recv = {
    recv_len: int;
    is_partial: bool;
    sockaddr: sockaddr option;
  }

  external recv:
    t -> 'a -> int -> int -> recv cb
    -> Int_result.unit = "uwt_udp_recv_own"

  let recv ?(pos=0) ?len ~buf ~dim t =
    let name = "uwt_udp_recv" in
    let len = match len with
    | None -> dim - pos
    | Some x -> x
    in
    if pos < 0 || len < 0 || pos > dim - len then
      Lwt.fail (Invalid_argument "Uwt.Udp.recv")
    else
      let sleeper,waker = Lwt.task () in
      let x = recv t buf pos len waker in
      if Int_result.is_error x then
        LInt_result.fail ~name ~param x
      else
        let () = Lwt.on_cancel sleeper ( fun () -> recv_stop t |> ignore ) in
        sleeper >>= function
        | Ok x -> Lwt.return x
        | Error x -> Lwt.fail (Uwt_error(x,name,param))

  let recv_ba ?pos ?len ~(buf:buf) t =
    let dim = Bigarray.Array1.dim buf in
    recv ~dim ?pos ?len ~buf t

  let recv ?pos ?len ~buf t =
    let dim = Bytes.length buf in
    recv ~dim ?pos ?len ~buf t

end

module Timer = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  (* external stop: t -> Int_result.unit = "uwt_timer_stop"
  let stop = stop
  let stop_exn t = stop t |> to_exnu "uv_timver_stop" *)

  external start:
    loop -> ( t -> unit ) -> int -> int -> t result = "uwt_timer_start"

  let start_exn ~repeat ~timeout ~cb =
    start loop cb timeout repeat |> to_exn "uv_timer_start"

  let start ~repeat ~timeout ~cb =
    start loop cb timeout repeat

  let sleep s =
    let sleeper,waker = Lwt.task () in
    let cb (_:t) = Lwt.wakeup waker () in
    match start ~repeat:0 ~timeout:s ~cb with
    | Error x -> Lwt.fail (Uwt_error(x,"uv_timer_start",param))
    | Ok t ->
      Lwt.on_cancel sleeper ( fun () -> close_noerr t );
      sleeper
end

module Signal = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  external start:
    loop -> int -> (t -> int -> unit) -> t result = "uwt_signal_start"

  let start_exn i ~cb = start loop i cb |> to_exn "uv_signal_start"
  let start i ~cb = start loop i cb

  (* external stop: t -> Int_result.unit = "uwt_signal_stop"
  let stop_exn t = stop t |> to_exnu "uv_signal_stop" *)
end

module Poll = struct
  type t = u
  include ( Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  type event =
    | Readable
    | Writable
    | Readable_writable

  external start:
    loop -> file -> event -> ( t -> event result -> unit ) -> t result
    = "uwt_poll_start"

  let start_exn f e ~cb = start loop f e cb |> to_exn "uv_poll_start"
  let start f e ~cb = start loop f e cb

  external start_socket:
    loop -> socket -> event -> ( t -> event result -> unit ) -> t result
    = "uwt_poll_start_socket"

  let start_socket_exn f e ~cb =
    start_socket loop f e cb |> to_exn "uv_poll_start"
  let start_socket f e ~cb = start_socket loop f e cb

  (* external stop: t -> Int_result.unit = "uwt_poll_stop"
  let stop_exn t = stop t |> to_exnu "uv_poll_stop" *)
end

module Fs_event = struct
  type t = u
  include ( Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  type event =
    | Rename
    | Change

  type flags =
    | Entry
    | Stat
    | Recursive

  type cb = t -> (string * event list) result -> unit

  external start:
    loop -> string -> flags list -> cb -> t result =
    "uwt_fs_event_start"

  let start_exn s fl ~cb = start loop s fl cb |> to_exn "uv_fs_event_start"
  let start s fl ~cb = start loop s fl cb

(*  external stop: t -> Int_result.unit = "uwt_fs_event_stop"
  let stop_exn t = stop t |> to_exnu "uv_fs_event_stop" *)
end

module Fs_poll = struct
  type t = u
  include ( Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  type report = {
    prev: Fs.stats;
    curr: Fs.stats
  }

  external start:
    loop -> string -> int -> (t -> report result -> unit) -> t result =
    "uwt_fs_poll_start"

  let start_exn s fl ~cb = start loop s fl cb |> to_exn "uv_fs_poll_start"
  let start s fl ~cb = start loop s fl cb

(*  external stop: t -> Int_result.unit = "uwt_fs_poll_stop"
  let stop_exn t = stop t |> to_exnu "uv_fs_poll_stop" *)
end

module Dns = struct

  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type getaddrinfo_option = Unix.getaddrinfo_option

  type addr_info = {
    ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string;
  }

  external getaddrinfo:
    string -> string -> getaddrinfo_option list ->
    loop -> Req.t -> addr_info list cb -> Int_result.unit
    = "uwt_getaddrinfo_byte" "uwt_getaddrinfo_native"

  let getaddrinfo ~host ~service options =
    Req.ql
      ~typ:Req.Getaddr
      ~f:(getaddrinfo host service options)
      ~name:"uv_getaddrinfo"
      ~param

  type name_info = {
    hostname : string;
    service : string ;
  }

  type getnameinfo_option = Unix.getnameinfo_option

  external getnameinfo :
    sockaddr -> getnameinfo_option list ->
    loop -> Req.t -> name_info cb -> Int_result.unit
    = "uwt_getnameinfo"

  let getnameinfo sock options =
    Req.ql
      ~typ:Req.Getname
      ~f:(getnameinfo sock options)
      ~name:"uv_getnameinfo"
      ~param

end

module Process = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  type stdio =
    | Inherit_file of file
    | Inherit_stream of Stream.t
    | Pipe of Pipe.t

  type stdio_args = stdio option * stdio option * stdio option
  type uid_gid = int * int
  type exit_cb = t -> exit_status:int -> term_signal:int -> unit

  external spawn:
    loop * stdio_args * uid_gid * int ->
    string array * string option ->
    exit_cb option ->
    string * string array ->
    t result = "uwt_spawn"

  let spawn ?stdin ?stdout ?stderr ?uid ?gid ?(verbatim_arguments=false)
      ?(detach=false) ?(hide=true) ?(env=[]) ?cwd ?exit_cb exe args
    =
    let stdio = (stdin,stdout,stderr) in
    let flags =
      (match uid with
       | None -> 0
       | Some _ -> 1 ) lor
      (match gid with
       | None -> 0
       | Some _ -> 2 ) lor
      (match verbatim_arguments with
       | false -> 0
       | true -> 4 ) lor
      (match detach with
       | false -> 0
       | true -> 8 ) lor
      (match hide with
       | false -> 0
       |  true -> 16)
    in
    let uid_gid = ((match uid with
      | None -> 0
      | Some x -> x ),(match gid with
      | None -> 0
      | Some x -> x))
    in
    let p1 = loop,stdio,uid_gid,flags in
    spawn p1 (Array.of_list env,cwd) exit_cb (exe,Array.of_list args)

  let spawn_exn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
      ?detach ?hide ?env ?cwd ?exit_cb exe args =
    spawn ?stdin ?stdout ?stderr ?uid ?gid ?verbatim_arguments
      ?detach ?hide ?env ?cwd ?exit_cb exe args |> to_exn "uv_spawn"

  external disable_stdio_inheritance: unit -> unit =
    "uwt_disable_stdio_inheritance_na" "noalloc"

  external pid: t -> Int_result.int = "uwt_pid_na" "noalloc"
  let pid_exn t = pid t |> to_exni "uwt_pid" (* yes uwt, it's not a function
                                                of libuv *)
  external process_kill: t -> int -> Int_result.unit = "uwt_process_kill_na"
  let process_kill_exn t s = process_kill t s |> to_exnu "uv_process_kill"

  external kill:
    pid:int -> signum:int -> Int_result.unit = "uwt_kill_na" "noalloc"
  let kill_exn ~pid ~signum = kill ~pid ~signum |> to_exnu "uv_kill"
end

module Async = struct
  type t = u
  include (Handle: (module type of Handle) with type t := t )
  external to_handle : t -> Handle.t = "%identity"

  external create: loop -> ( t -> unit ) -> t result = "uwt_async_create"
  let create cb = create loop cb

  external start: t -> Int_result.unit = "uwt_async_start_na" "noalloc"
  external stop: t -> Int_result.unit = "uwt_async_stop_na" "noalloc"

  external send: t -> Int_result.unit = "uwt_async_send_na" "noalloc"
end

module Main = struct

  let exceptions = ref []
  let fatal_found = ref false (* information for exit_hook and run *)

  let add_exception (e:exn) =
    let bt = Printexc.get_raw_backtrace () in
    exceptions := (e,bt)::!exceptions

  let () = Callback.register "uwt.add_exception" add_exception

  exception Main_error of error * string
  exception Deferred of (exn * Printexc.raw_backtrace) list
  exception Fatal of exn * Printexc.raw_backtrace

  let enter_iter_hooks = Lwt_sequence.create ()
  let leave_iter_hooks = Lwt_sequence.create ()
  let yielded = Lwt_sequence.create ()
  let yield () = Lwt.add_task_r yielded

  let rec run ~nothing_cnt task =
    Lwt.wakeup_paused ();
    match Lwt.poll task with
    | Some x -> x
    | None ->
      (* 255 is certainly too high. But I will perhaps postpone certain tasks
         to the next loop iteration *)
      if nothing_cnt > 255 then
        raise (Main_error(EOF,"nothing to do in run"))
      else (
        (* Call enter hooks. *)
        Lwt_sequence.iter_l (fun f -> f ()) enter_iter_hooks;
        (* Do the main loop call. *)
        let mode =
          if Lwt.paused_count () = 0 && Lwt_sequence.is_empty yielded then
            Run_once
          else
            Run_nowait
        in
        let lr = match uv_run_loop loop mode with
        | lr -> lr
        | exception e ->
          fatal_found := true;
          let bt = Printexc.get_raw_backtrace () in
          raise (Fatal(e,bt))
        in
        (match !exceptions with
         | [] -> ()
         | l ->
           exceptions:= [];
           let l = List.rev l in
           let l =
             if Int_result.is_error lr then
               (Main_error(Int_result.to_error lr,"run"),
                Printexc.get_callstack 0)::l
             else
               l
           in
           raise (Deferred l));
        if Int_result.is_error lr then
          raise (Main_error(Int_result.to_error lr,"run"));
        let nothing_cnt =
          let lr : int = (lr :> int) in
          if lr = 0 && mode = Run_once then
            nothing_cnt + 1
          else
            0
        in
        Lwt.wakeup_paused ();
        (* Wakeup yielded threads now. *)
        if not (Lwt_sequence.is_empty yielded) then begin
          let tmp = Lwt_sequence.create () in
          Lwt_sequence.transfer_r yielded tmp;
          Lwt_sequence.iter_l (fun wakener -> Lwt.wakeup wakener ()) tmp
        end;
        (* Call leave hooks. *)
        Lwt_sequence.iter_l (fun f -> f ()) leave_iter_hooks;
        run ~nothing_cnt task
      )

  let run (t:'a Lwt.t) : 'a =
    if !fatal_found then
      failwith "uwt loop unusuable";
    run ~nothing_cnt:0 t

  let exit_hooks = Lwt_sequence.create ()

  let rec call_hooks () =
    match Lwt_sequence.take_opt_l exit_hooks with
    | None ->
      Lwt.return_unit
    | Some f ->
      Lwt.catch
        (fun () -> f ())
        (fun _  -> Lwt.return_unit) >>= fun () ->
      call_hooks ()

  let () = at_exit (fun () -> if !fatal_found then () else run (call_hooks ()))
  let at_exit f = ignore (Lwt_sequence.add_l f exit_hooks)

end

module Unix = struct

  external gethostname:
    loop -> Req.t -> string cb -> Int_result.unit
    = "uwt_gethostname"

  let gethostname () =
    Req.ql
      ~typ:Req.Work
      ~f:gethostname
      ~name:"uwt_gethostname"
      ~param

  type host_entry = Unix.host_entry
  external gethostbyname:
    string -> loop -> Req.t -> host_entry cb -> Int_result.unit
    = "uwt_gethostbyname"

  let gethostbyname p =
    Req.ql
      ~typ:Req.Work
      ~f:(gethostbyname p)
      ~name:"uwt_gethostbyname"
      ~param

  external gethostbyaddr:
    string -> loop -> Req.t -> host_entry cb -> Int_result.unit
    = "uwt_gethostbyaddr"

  let gethostbyaddr p =
    let p = Unix.string_of_inet_addr p in
    Req.ql
      ~typ:Req.Work
      ~f:(gethostbyaddr p)
      ~name:"uwt_gethostbyaddr"
      ~param

  type service_entry = Unix.service_entry

  external getservbyname:
    string -> string -> loop -> Req.t ->  service_entry cb -> Int_result.unit =
    "uwt_getservbyname"

  let getservbyname ~name ~protocol =
    Req.ql
      ~typ:Req.Work
      ~f:(getservbyname name protocol)
      ~name:"uwt_getservbyname"
      ~param

  external getservbyport:
    int -> string -> loop -> Req.t ->  service_entry cb -> Int_result.unit =
    "uwt_getservbyport"

  let getservbyport port protocol =
    Req.ql
      ~typ:Req.Work
      ~f:(getservbyport port protocol)
      ~name:"uwt_getservbyport"
      ~param

  let getaddrinfo host service (options:Unix.getaddrinfo_option list) =
    Dns.getaddrinfo ~host ~service options >>=fun l ->
    let f a =
      let open Unix in
      { ai_family = a.Dns.ai_family;
        ai_socktype = a.Dns.ai_socktype;
        ai_protocol = a.Dns.ai_protocol;
        ai_canonname = a.Dns.ai_canonname;
        ai_addr = Conv.unix_sockaddr_of_sockaddr a.Dns.ai_addr }
    in
    Lwt.return (List.map f l)

  external getprotobyname:
    string -> loop -> Req.t -> Unix.protocol_entry cb -> Int_result.unit
    = "uwt_getprotobyname"

  let getprotobyname p =
    Req.ql
      ~typ:Req.Work
      ~f:(getprotobyname p)
      ~name:"uwt_getprotobyname"
      ~param

  external getprotobynumber:
    int -> loop -> Req.t -> Unix.protocol_entry cb -> Int_result.unit
    = "uwt_getprotobynumber"

  let getprotobynumber p =
    Req.ql
      ~typ:Req.Work
      ~f:(getprotobynumber p)
      ~name:"uwt_getprotobynumber"
      ~param

  external lseek:
    file -> int64 -> Unix.seek_command -> loop -> Req.t -> int64 cb ->
    Int_result.unit = "uwt_lseek_byte" "uwt_lseek_native"

  let lseek f o m  =
    Req.ql
      ~typ:Req.Work
      ~f:(lseek f o m)
      ~name:"uwt_lseek"
      ~param
end

module Valgrind = struct
  let help () =
    let len = int_of_float (2. ** 18.) in
    let _t = Array.init len ( fun _x ->
        Random.int 1024 |> Bytes.create
      )
    in
    ()
  let lwt_cleanup () =
    let rec iter n =
      if n <= 0 then
        Lwt.return_unit
      else
        Main.yield () >>= fun () ->
        iter (pred n)
    in
    Main.run (iter 99)

  external global_cleanup:
    unit -> unit = "uwt_free_all_memory"

  let valgrind_happy () =
    Gc.compact ();
    help ();
    Gc.compact ();
    let () = Main.run (Main.call_hooks ()) in
    lwt_cleanup ();
    global_cleanup ()

end

module C_worker = struct

  type t = unit Int_result.t
  type 'a u = loop * Req.t * 'a result Lwt.u

  let call (f: 'a -> 'b u -> t) (a:'a) : 'b Lwt.t =
    let sleeper,waker = Lwt.task () in
    let req = Req.create loop Req.Work in
    match f a (loop,req,waker) with
    | exception x ->
      Req.finalize req;
      Lwt.fail x
    | x ->
      if Int_result.is_error x then
        LInt_result.mfail ~name:"C_worker.call" ~param:"" x
      else
        let () = Lwt.on_cancel sleeper ( fun () -> Req.cancel_noerr req ) in
        sleeper >>= fun x ->
        Req.finalize req;
        match x with
        | Ok x -> Lwt.return x
        | Error x -> Lwt.fail (Uwt_error(x,"",""))
end

let valgrind_happy = Valgrind.valgrind_happy

let () =
  Printexc.register_printer
    (function
    | Uwt_error (e, s, s') ->
      let msg = err_name e in
      Some (Printf.sprintf "Uwt.Uwt_error(Uwt.%s, %S, %S)" msg s s')
    | Main.Main_error(e,s) ->
      let msg = err_name e in
      Some (Printf.sprintf "Uwt.Main.Main_error(Uwt.%s, %S)" msg s)
    | Main.Deferred(l) ->
      let l =
        List.map ( fun (exn,bt) ->
            "(" ^ (Printexc.to_string exn) ^ ",\n" ^
            (Printexc.raw_backtrace_to_string bt) ^ ");")
          l
      in
      let s = "Uwt.Main.Deferred([\n" ^ String.concat "\n" l ^ "\n])" in
      Some s
    | Main.Fatal(e,bt) ->
      let s =
        "Uwt.Main.Fatal(" ^
        (Printexc.to_string e) ^ ",\n" ^
        (Printexc.raw_backtrace_to_string bt) ^ ")"
      in
      Some s
    | _ -> None)
