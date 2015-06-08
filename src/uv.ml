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

#include "error.ml"

type 'a result =
| Ok of 'a
| Error of error

external strerror: error -> string = "uwt_strerror"
exception Uv_error of error * string * string

module Int_result = struct
  type 'a t = int

  type real_int = int
  type real_unit = unit
  type int = real_int t
  type unit = real_unit t

  let is_ok (x: 'a t) = x >= 0
  let is_error (x: 'a t) = x < 0

  let to_int (x: int) : real_int =
    if x < 0 then
      invalid_arg "Uwt.Int_result.to_int";
    x

  let transform x =
    let y = (x * (-1)) - 1 in
    Obj.magic y

  let to_error (x: 'a t) : error =
    if x >= 0 then
      invalid_arg "Uwt.Int_result.to_error";
    transform x

  let raise_exn ?(name="") ?(param="") (x: 'a t) =
    if x >= 0 then
      invalid_arg "Uwt.Int_result.raise_exn"
    else
      raise (Uv_error(transform x,name,param))

  let to_exn ?(name="") ?(param="") (x: 'a t) =
    if x >= 0 then
      Invalid_argument "Uwt.Int_result.to_exn"
    else
      Uv_error(transform x,name,param)

#include "error_val.ml"
end

type file = int

type socket

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type sockaddr

let stdin : file = Obj.magic 0
let stdout : file = Obj.magic 1
let stderr : file = Obj.magic 2

module Fs = struct
  type open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_CREAT
    | O_EXCL
    | O_TRUNC
    | O_APPEND
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_TEMPORARY
    | O_SHORT_LIVED
    | O_SEQUENTIAL
    | O_RANDOM

  type file_kind =
    |	S_REG
    |	S_DIR
    |	S_CHR
    |	S_BLK
    |	S_LNK
    |	S_FIFO
    |	S_SOCK
    | S_UNKNOWN

  type access_permission =
    | Read
    | Write
    | Exec
    | Exists

  type symlink_mode =
    | S_Default
    | S_Dir
    | S_Junction

  type stats = {
    st_dev: int;
    st_kind: file_kind;
    st_perm: int;
    st_nlink: int;
    st_uid: int;
    st_gid: int;
    st_rdev: int;
    st_ino: int;
    st_size: int64;
    st_blksize: int;
    st_blocks: int;
    st_flags: int;
    st_gen: int;
    st_atime: int64;
    st_atime_nsec: int;
    st_mtime: int64;
    st_mtime_nsec: int;
    st_ctime: int64;
    st_ctime_nsec: int;
    st_birthtime: int64;
    st_birthtime_nsec: int;
  }
end

module Conv = struct
  external sockaddr_of_unix_sockaddr : Unix.sockaddr -> sockaddr = "uwt_of_sockaddr"
  external unix_sockaddr_of_sockaddr : sockaddr -> Unix.sockaddr = "uwt_to_sockaddr"
  (* the following functions always succeeds on *nix - but not on windows *)
  external file_of_file_descr: Unix.file_descr -> file option = "uwt_get_fd"
  external socket_of_file_descr:
    Unix.file_descr -> socket option = "uwt_get_socket"
  external file_descr_of_file :
    file -> Unix.file_descr option = "uwt_get_file_descriptor"
end
