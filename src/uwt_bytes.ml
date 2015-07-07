(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
 *               2010 Pierre Chambart
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

(* I/O parts removed, external functions renamed, otherwise unmodified
   import from lwt, A.H. *)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create size = Array1.create char c_layout size
let length bytes = Array1.dim bytes

external unsafe_fill : t -> int -> int -> char -> unit = "uwt_unix_fill_bytes" "noalloc"

(*
custom noalloc wrappers seem to be faster than the more general solution
of the caml runtime. But this might change in the future.
Benchmark it again with further releases of OCaml,...

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"
*)
external unsafe_get: t -> int -> char = "uwt_unix_unsafe_getbuf" "noalloc"
external unsafe_set: t -> int -> char -> unit = "uwt_unix_unsafe_setbuf" "noalloc"

let get b i =
  if i < 0 || i >= Array1.dim b then
    raise (Invalid_argument "index out of bounds");
  unsafe_get b i

let set b i c =
  if i < 0 || i >= Array1.dim b then
    raise (Invalid_argument "index out of bounds");
  unsafe_set b i c

let fill bytes ofs len ch =
  if ofs < 0 || len < 0 || ofs > length bytes - len then
    invalid_arg "Uwt_bytes.fill"
  else
    unsafe_fill bytes ofs len ch

(* +-----------------------------------------------------------------+
   | Blitting                                                        |
   +-----------------------------------------------------------------+ *)

external unsafe_blit_from_bytes : Bytes.t -> int -> t -> int -> int -> unit = "uwt_unix_blit_from_bytes" "noalloc"
external unsafe_blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit = "uwt_unix_blit_to_bytes" "noalloc"
external unsafe_blit : t -> int -> t -> int -> int -> unit = "uwt_unix_blit" "noalloc"

let blit_from_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > Bytes.length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit_from_bytes src_buf src_ofs dst_buf dst_ofs len

let blit_to_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > Bytes.length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit_to_bytes src_buf src_ofs dst_buf dst_ofs len

let blit src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit src_buf src_ofs dst_buf dst_ofs len

let of_bytes buf =
  let len = Bytes.length buf in
  let bytes = create len in
  unsafe_blit_from_bytes buf 0 bytes 0 len;
  bytes

let of_string str = of_bytes (Bytes.unsafe_of_string str)

let to_bytes bytes =
  let len = length bytes in
  let str = Bytes.create len in
  unsafe_blit_to_bytes bytes 0 str 0 len;
  str

let to_string bytes = Bytes.unsafe_to_string (to_bytes bytes)

let proxy = Array1.sub

let extract buf ofs len =
  if ofs < 0 || len < 0 || ofs > length buf - len then
    invalid_arg "Uwt_bytes.extract"
  else begin
    let buf' = create len in
    blit buf ofs buf' 0 len;
    buf'
  end

let copy buf =
  let len = length buf in
  let buf' = create len in
  blit buf 0 buf' 0 len;
  buf'
