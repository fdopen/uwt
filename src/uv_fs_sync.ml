(* Libuv bindings for OCaml
 * http://github.com/fdopen/uwt
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
 *)

(*
type loop_mode =
  | Sync
  | Lwt
  | Cb
*)
#include "config.inc"

open Uwt_base
include Fs_types

type loop
external uv_default_loop: int -> loop uv_result = "uwt_default_loop"
let loop =
  match uv_default_loop 0 with (* sync of disabled loop_mode *)
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

let openfile ?(perm=0o644) ~mode fln : file uv_result =
  cu (openfile fln mode perm)


external iread:
  file -> 'a -> int -> int -> int64 ->
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

let iread ~fd_offset ?(pos=0) ?len t ~buf ~dim =
  let len =  match len with
  | None -> dim - pos
  | Some x -> x
  in
  if pos < 0 || len < 0 || pos > dim - len then
    invalid_arg "Uwt_sync.Fs.read"
  else
    let req = Req.create loop typ in
    iread t buf pos len fd_offset loop req () |>
    read_write_common ~req

let read_ba ?pos ?len t ~(buf:buf) =
  let dim = Bigarray.Array1.dim buf in
  iread ~fd_offset:Int64.minus_one ?pos ?len ~dim ~buf t

let read ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  iread ~fd_offset:Int64.minus_one ?pos ?len ~dim ~buf t

let pread_ba ?pos ?len t ~fd_offset ~(buf:buf) =
  if Int64.compare fd_offset Int64.zero < 0 then
    Error EINVAL
  else
    let dim = Bigarray.Array1.dim buf in
    iread ~fd_offset ?pos ?len ~dim ~buf t

let pread ?pos ?len t ~fd_offset ~buf =
  if Int64.compare fd_offset Int64.zero < 0 then
    Error EINVAL
  else
    let dim = Bytes.length buf in
    iread ~fd_offset ?pos ?len ~dim ~buf t

external iwrite:
  file -> 'a -> int -> int -> int64 ->
  loop -> Req.t -> unit ->
  Int_result.int =
  "uwt_fs_write_byte" "uwt_fs_write_native"

let iwrite ~fd_offset ?(pos=0) ?len ~dim t ~buf =
  let len =  match len with
  | None -> dim - pos
  | Some x -> x
  in
  if pos < 0 || len < 0 || pos > dim - len then
    invalid_arg "Uwt_sync.Fs.write"
  else
    let req = Req.create loop typ in
    iwrite t buf pos len fd_offset loop req () |>
    read_write_common ~req

let write_ba ?pos ?len t ~(buf:buf) =
  let dim = Bigarray.Array1.dim buf in
  iwrite ~fd_offset:Int64.minus_one ~dim ?pos ?len t ~buf

let write_string ?pos ?len t ~buf =
  let dim = String.length buf in
  iwrite ~fd_offset:Int64.minus_one ~dim ?pos ?len t ~buf

let write ?pos ?len t ~buf =
  let dim = Bytes.length buf in
  iwrite ~fd_offset:Int64.minus_one ~dim ?pos ?len t ~buf

let iwrite ~fd_offset ?pos ?len ~dim t ~buf =
  if Int64.compare fd_offset Int64.zero < 0 then
    Error EINVAL
  else
    iwrite ~fd_offset ~dim ?pos ?len t ~buf

let pwrite_ba ?pos ?len t ~fd_offset ~(buf:buf) =
  let dim = Bigarray.Array1.dim buf in
  iwrite ~fd_offset ~dim ?pos ?len t ~buf

let pwrite_string ?pos ?len t ~fd_offset ~buf =
  let dim = String.length buf in
  iwrite ~fd_offset ~dim ?pos ?len t ~buf

let pwrite ?pos ?len t ~fd_offset ~buf =
  let dim = Bytes.length buf in
  iwrite ~fd_offset ~dim ?pos ?len t ~buf

external iwritev:
  file -> Iovec_write.t array -> Iovec_write.t list -> int64 ->
  loop -> Req.t -> unit ->
  Int_result.int =
  "uwt_fs_writev_byte" "uwt_fs_writev_native"

let iwritev ~fd_offset t iol =
  let open Iovec_write in
  match prep_for_cstub iol with
  | Invalid -> invalid_arg "Uwt_sync.Fs.writev"
  | Empty -> write ~len:0 t ~buf:(Bytes.create 1)
  | All_ba(ar,bl) ->
    let req = Req.create loop typ in
    iwritev t ar bl fd_offset loop req () |>
    read_write_common ~req

let writev a b = iwritev ~fd_offset:Int64.minus_one a b
let pwritev a b fd_offset =
  if Int64.compare fd_offset Int64.zero < 0 then
    Error EINVAL
  else
    iwritev ~fd_offset a b

external sendfile:
  file -> file -> int64 -> nativeint -> loop -> Req.t -> unit ->
  Int_result.int = "uwt_fs_sendfile_byte" "uwt_fs_sendfile_native"

let sendfile ?(pos=0L) ?(len=Nativeint.max_int)  ~dst ~src () : nativeint uv_result =
  cu (sendfile dst src pos len)

external stat:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_stat"

let stat param : stats uv_result = cu (stat param)

external lstat:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_lstat"
let lstat param : stats uv_result = cu (lstat param)

external fstat:
  file -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_fstat"
let fstat fd : stats uv_result = cu (fstat fd)

external mkdtemp:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_mkdtemp"
let mkdtemp param : string uv_result = cu (mkdtemp param)

external readlink:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_readlink"
let readlink param : string uv_result = cu (readlink param)

external scandir:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_scandir"
let scandir param : (file_kind * string) array uv_result =
  cu (scandir param)

#if HAVE_UV_REALPATH = 0
external realpath: string -> string Uwt_base.uv_result = "uwt_realpath_sync"
#else
external realpath:
  string -> loop -> Req.t -> unit -> Int_result.int = "uwt_fs_realpath"
#if  HAVE_WINDOWS = 0
let realpath param : string uv_result = cu (realpath param)
#else
external realpath_o: string -> string Uwt_base.uv_result = "uwt_realpath_sync"
let use_own_realpath = ref (
    match Uwt_base.Sys_info.win_version () with
    | Ok(x) when x.Uwt_base.Sys_info.major_version < 6 -> true
    | _ -> false )
let realpath param  : string uv_result =
  match !use_own_realpath with
  | true -> realpath_o param
  | false ->
    match cu (realpath param) with
    | Error Uwt_base.ENOSYS ->
      use_own_realpath:= true;
      realpath_o param
    | x -> x
#endif
#endif
