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

(*
type loop_mode =
  | Sync
  | Lwt
  | Cb
*)

open Uwt_base
include Fs_types

type loop
external uv_default_loop: int -> loop result = "uwt_default_loop"
let loop =
  match uv_default_loop 0 with (* Lwt of disabled loop_mode *)
  | Error _ ->
    prerr_endline "Can't init default loop";
    exit(3)
  | Ok x -> x

module Req = struct
  type t
  type type' =
    | Fs
  (*  | Getaddr
    | Getname
    | Work *)

  external create: loop -> type' -> t = "uwt_req_create"
  external free: t -> unit = "uwt_fs_free"

  let qlu ~typ ~f =
    let req = create loop typ in
    let (x: Int_result.int) = f loop req () in
    free req;
    if Int_result.is_error x then
      Error (Int_result.to_error x)
    else
      Ok ()
end

let typ = Req.Fs

external mkdir:
  string -> int -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_mkdir"
let mkdir ?(perm=0o777) param = Req.qlu ~typ ~f:(mkdir param perm)

external rmdir:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_rmdir"
let rmdir param = Req.qlu ~typ ~f:(rmdir param)

external close:
  file -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_close"
let close fd = Req.qlu ~typ ~f:(close fd)

external unlink:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_unlink"
let unlink param = Req.qlu ~typ ~f:(unlink param)

external rename:
  string -> string -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_rename"
let rename ~src ~dst = Req.qlu ~typ ~f:(rename src dst)

external link:
  string -> string -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_link"
let link ~target ~link_name = Req.qlu ~typ ~f:(link target link_name)

external fsync: file -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_fsync"
let fsync file = Req.qlu ~typ ~f:(fsync file)

external fdatasync:
  file -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_fsync"
let fdatasync file = Req.qlu ~typ ~f:(fdatasync file)

external ftruncate:
  file -> int64 -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_ftruncate"
let ftruncate file ~len = Req.qlu ~typ ~f:(ftruncate file len)

external symlink:
  string -> string -> symlink_mode -> loop -> Req.t -> unit ->
  Int_result.int = "uwt_fs_symlink_byte" "uwt_fs_symlink_native"
let symlink ?(mode=S_Default) ~src ~dst () =
  Req.qlu ~typ ~f:(symlink src dst mode)

external utime:
  string -> float -> float -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_utime_byte" "uwt_fs_utime_native"
let utime s ~access ~modif =
  Req.qlu ~typ ~f:(utime s access modif)

external futime:
  file -> float -> float -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_futime_byte" "uwt_fs_futime_native"
let futime fd ~access ~modif =
  Req.qlu ~typ ~f:(futime fd access modif)

external access:
  string -> access_permission list -> loop -> Req.t -> unit ->
  Int_result.int = "uwt_fs_access"
let access s al = Req.qlu ~typ ~f:(access s al)

external chmod:
  string -> int -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_chmod"
let chmod param ~perm = Req.qlu ~typ ~f:(chmod param perm)

external fchmod:
  file -> int -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_fchmod"
let fchmod fd ~perm = Req.qlu ~typ ~f:(fchmod fd perm)

external chown:
  string -> int -> int -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_chown_byte" "uwt_fs_chown_native"
let chown s ~uid ~gid = Req.qlu ~typ ~f:(chown s uid gid)

external fchown:
  file -> int -> int -> loop -> Req.t -> unit -> Int_result.int =
  "uwt_fs_fchown_byte" "uwt_fs_fchown_native"
let fchown fd ~uid ~gid = Req.qlu ~typ ~f:(fchown fd uid gid)

external get_result: Req.t -> 'a = "uwt_get_fs_result"
let cu f : 'a =
  let req = Req.create loop typ in
  let (x: Int_result.int) = f loop req () in
  if Int_result.is_error x then
    Error (Int_result.to_error x)
  else
    get_result req

external openfile:
  string -> uv_open_flag list -> int ->
  loop -> Req.t -> unit ->
  Int_result.int =
  "uwt_fs_open_byte" "uwt_fs_open_native"

let openfile ?(perm=0o644) ~mode fln : file result =
  cu (openfile fln mode perm)


external read:
  file -> 'a -> int -> int ->
  loop -> Req.t -> unit ->
  Int_result.int =
  "uwt_fs_read_byte" "uwt_fs_read_native"

let read_write_common ~req (x: Int_result.int) =
  let x =
    if Int_result.is_ok x then
      get_result req
    else
      let () = Req.free req in
      x
  in
  let x' : int =  ( x:> int ) in
  if x' < 0 then
    Error (Int_result.to_error x)
  else
    Ok x'

let read ?(pos=0) ?len t ~buf ~dim =
  let len =  match len with
  | None -> dim - pos
  | Some x -> x
  in
  if pos < 0 || len < 0 || pos > dim - len then
    invalid_arg "Uwt_sync.Fs.read"
  else
    let req = Req.create loop typ in
    read t buf pos len loop req () |>
    read_write_common ~req

let read_ba ?pos ?len t ~(buf:buf) =
  let dim = Bigarray.Array1.dim buf in
  read ?pos ?len ~dim ~buf t

let read ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  read ?pos ?len ~dim ~buf t

external write:
  file -> 'a -> int -> int ->
  loop -> Req.t -> unit ->
  Int_result.int =
  "uwt_fs_write_byte" "uwt_fs_write_native"

let write ?(pos=0) ?len ~dim t ~buf =
  let len =  match len with
  | None -> dim - pos
  | Some x -> x
  in
  if pos < 0 || len < 0 || pos > dim - len then
    invalid_arg "Uwt_sync.Fs.write"
  else
    let req = Req.create loop typ in
    write t buf pos len loop req () |>
    read_write_common ~req

let write_ba ?pos ?len t ~(buf:buf) =
  let dim = Bigarray.Array1.dim buf in
  write ~dim ?pos ?len t ~buf

let write_string ?pos ?len t ~buf =
  let dim = String.length buf in
  write ~dim ?pos ?len t ~buf

let write ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  write ~dim ?pos ?len t ~buf

external sendfile:
  file -> file -> int64 -> nativeint -> loop -> Req.t -> unit ->
  Int_result.int = "uwt_fs_sendfile_byte" "uwt_fs_sendfile_native"

let sendfile ?(pos=0L) ?(len=Nativeint.max_int)  ~dst ~src () : nativeint result =
  cu (sendfile dst src pos len)

external stat:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_stat"

let stat param : stats result = cu (stat param)

external lstat:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_lstat"
let lstat param : stats result = cu (lstat param)

external fstat:
  file -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_fstat"
let fstat fd : stats result = cu (fstat fd)

external mkdtemp:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_mkdtemp"
let mkdtemp param : string result = cu (mkdtemp param)

external readlink:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_readlink"
let readlink param : string result = cu (readlink param)

external scandir:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_scandir"
let scandir param : (file_kind * string) array result =
  cu (scandir param)
