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

#if HAVE_WINDOWS <> 0
external init_uwt : unit -> unit = "uwt_init_sync_na" NOALLOC
let () = init_uwt ()
#endif

let wrap x =
  if Int_result.is_error x then
    Error (Int_result.to_error x)
  else
    Ok ()

external mkdir: string -> int -> Int_result.int = "uwt_fs_mkdir_sync"
let mkdir ?(perm=0o777) param = mkdir param perm |> wrap

external rmdir: string -> Int_result.int = "uwt_fs_rmdir_sync"
let rmdir param = rmdir param |> wrap

external close: file -> Int_result.int = "uwt_fs_close_sync"
let close fd = close fd |> wrap

external unlink: string -> Int_result.int = "uwt_fs_unlink_sync"
let unlink param = unlink param |> wrap

external rename: string -> string -> Int_result.int = "uwt_fs_rename_sync"
let rename ~src ~dst = rename src dst |> wrap

external link: string -> string -> Int_result.int = "uwt_fs_link_sync"
let link ~target ~link_name = link target link_name |> wrap

external fsync: file -> Int_result.int = "uwt_fs_fsync_sync"
let fsync file = fsync file |> wrap

external fdatasync: file -> Int_result.int = "uwt_fs_fdatasync_sync"
let fdatasync file = fdatasync file |> wrap

external ftruncate: file -> int64 -> Int_result.int = "uwt_fs_ftruncate_sync"
let ftruncate file ~len = ftruncate file len |> wrap

external symlink: string -> string -> symlink_mode -> Int_result.int =
  "uwt_fs_symlink_sync"
let symlink ?(mode=S_Default) ~src ~dst () =
#if HAVE_WINDOWS <> 0
  (* FIXME: where to put this function to avoid code duplication? *)
  let src =
    if String.length src >= 4 && src.[0] = '\\' && src.[1] = '\\' &&
       src.[2] = '?' && src.[3] = '\\' then src
    else String.map (function '/' -> '\\' | c -> c) src in
#endif
  symlink src dst mode |> wrap

external utime: string -> float -> float -> Int_result.int = "uwt_fs_utime_sync"
let utime s ~access ~modif = utime s access modif |> wrap

external futime: file -> float -> float -> Int_result.int = "uwt_fs_futime_sync"
let futime fd ~access ~modif = futime fd access modif |> wrap

external access: string -> access_permission list -> Int_result.int =
  "uwt_fs_access_sync"
let access s al = access s al |> wrap

external chmod: string -> int -> Int_result.int = "uwt_fs_chmod_sync"
let chmod param ~perm = chmod param perm |> wrap

external fchmod: file -> int -> Int_result.int = "uwt_fs_fchmod_sync"
let fchmod fd ~perm = fchmod fd perm |> wrap

external chown: string -> int -> int -> Int_result.int =
  "uwt_fs_chown_sync"
let chown s ~uid ~gid = chown s uid gid |> wrap

external fchown: file -> int -> int -> Int_result.int = "uwt_fs_fchown_sync"
let fchown fd ~uid ~gid = fchown fd uid gid |> wrap

external lchown: string -> int -> int -> Int_result.int =
  "uwt_fs_lchown_sync"
let lchown s ~uid ~gid = lchown s uid gid |> wrap

external openfile: string -> uv_open_flag list -> int ->
  file uv_result = "uwt_fs_open_sync"

let openfile ?(perm=0o644) ~mode fln : file uv_result = openfile fln mode perm

external iread: file -> 'a -> int -> int -> int64 ->
  Int_result.int = "uwt_fs_read_sync"

let read_write_common (x:Int_result.int) =
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
    iread t buf pos len fd_offset |>
    read_write_common

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
  file -> 'a -> int -> int -> int64 -> Int_result.int = "uwt_fs_write_sync"

let iwrite ~fd_offset ?(pos=0) ?len ~dim t ~buf =
  let len =  match len with
  | None -> dim - pos
  | Some x -> x
  in
  if pos < 0 || len < 0 || pos > dim - len then
    invalid_arg "Uwt_sync.Fs.write"
  else
    iwrite t buf pos len fd_offset |>
    read_write_common

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
  Iovec_write.t array -> Iovec_write.t list -> file -> int64 ->
  Int_result.int = "uwt_fs_writev_sync"

let iwritev ~fd_offset t iol =
  let open Iovec_write in
  match prep_for_cstub iol with
  | Invalid -> invalid_arg "Uwt_sync.Fs.writev"
  | Empty -> write ~len:0 t ~buf:(Bytes.create 1)
  | All_ba(ar,bl) ->
    iwritev ar bl t fd_offset |>
    read_write_common

let writev a b = iwritev ~fd_offset:Int64.minus_one a b
let pwritev a b fd_offset =
  if Int64.compare fd_offset Int64.zero < 0 then
    Error EINVAL
  else
    iwritev ~fd_offset a b

external sendfile:
  file -> file -> int64 -> nativeint ->
  nativeint uv_result = "uwt_fs_sendfile_sync"

let sendfile ?(pos=0L) ?(len=Nativeint.max_int)  ~dst ~src () =
  sendfile dst src pos len

external stat: string -> stats uv_result = "uwt_fs_stat_sync"
external lstat:  string -> stats uv_result = "uwt_fs_lstat_sync"
external fstat: file -> stats uv_result = "uwt_fs_fstat_sync"
external mkdtemp: string -> string uv_result = "uwt_fs_mkdtemp_sync"
external readlink: string -> string uv_result = "uwt_fs_readlink_sync"

external scandir:
  string -> (file_kind * string) array uv_result = "uwt_fs_scandir_sync"

external realpath: string -> string Uwt_base.uv_result = "uwt_fs_realpath_sync"
#if  HAVE_WINDOWS <> 0
external realpath_o: string -> string Uwt_base.uv_result = "uwt_realpath_sync"
let use_own_realpath = ref (
    match Uwt_base.Sys_info.win_version () with
    | Ok(x) when x.Uwt_base.Sys_info.major_version < 6 -> true
    | _ -> false )
let realpath param  : string uv_result =
  match !use_own_realpath with
  | true -> realpath_o param
  | false ->
    match realpath param with
    | Error Uwt_base.ENOSYS ->
      use_own_realpath := true;
      realpath_o param
    | x -> x
#endif

external copyfile: string -> string -> int -> Int_result.int =
  "uwt_fs_copyfile_sync"
let copyfile ?(excl=false) ?(clone=No_clone) ~src ~dst () =
  let flags =
    (if excl then 1 else 0) lor
      (match clone with
       | No_clone -> 0
       | Try_clone -> 2
       | Force_clone -> 4) in
  copyfile src dst flags |> wrap
