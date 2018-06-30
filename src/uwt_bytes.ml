(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

#include "config.inc"
open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create size = Array1.create char c_layout size
let length bytes = Array1.dim bytes

external unsafe_fill : t -> int -> int -> char -> unit = "uwt_unix_fill_bytes" NOALLOC

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"
external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

let fill bytes ofs len ch =
  if ofs < 0 || len < 0 || ofs > length bytes - len then
    invalid_arg "Uwt_bytes.fill"
  else
    unsafe_fill bytes ofs len ch

(* +-----------------------------------------------------------------+
   | Blitting                                                        |
   +-----------------------------------------------------------------+ *)

external unsafe_blit_from_bytes : Bytes.t -> int -> t -> int -> int -> unit = "uwt_unix_blit_from_bytes" NOALLOC
external unsafe_blit_to_bytes : t -> int -> Bytes.t -> int -> int -> unit = "uwt_unix_blit_to_bytes" NOALLOC
external unsafe_blit : t -> int -> t -> int -> int -> unit = "uwt_unix_blit" NOALLOC

let blit_from_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > Bytes.length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Uwt_bytes.blit_from_bytes"
  else
    unsafe_blit_from_bytes src_buf src_ofs dst_buf dst_ofs len

let blit_to_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > Bytes.length dst_buf - len) then
    invalid_arg "Uwt_bytes.blit_to_bytes"
  else
    unsafe_blit_to_bytes src_buf src_ofs dst_buf dst_ofs len

let blit src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "Uwt_bytes.blit"
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
