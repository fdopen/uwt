(** Synchronous filesystem operations *)

(**
   This module is independent of lwt. It might be useful, if you target windows:

   - additional options and functions are available, that are missing
   inside the standard Unix module.

   - filenames and similar parameters and return values are always utf-8
   encoded.
*)

include Uwt_base.Fs_functions
  with type 'a t := 'a Uwt_base.uv_result
