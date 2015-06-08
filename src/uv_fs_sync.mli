(** synchronous system calls *)

(**
   These synchronous might be useful, if you also target windows users:
   - filenames must be utf8 encoded (see comment to ECHARSET)
   - sometimes additional options are available, e.g. [Fs.openfile]
*)

open Uv
open Uv.Fs

(** @param perm defaults are 0o644 *)
val openfile : ?perm:int -> mode:Uv.Fs.open_flag list -> string -> file result

(** @param pos default is always zero
    @param len default is always the length of the string / array / Bytes.t *)
val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int result

(** _ba function are unsafe. Bigarrays are passed directly to libuv (no
    copy to c heap or stack). *)
val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int result

val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int result
val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int result
val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int result

val close : file -> unit result

val unlink : string -> unit result

(** @param perm defaults are 0o777 *)
val mkdir : ?perm:int -> string -> unit result

val rmdir : string -> unit result

val fsync : file -> unit result
val fdatasync : file -> unit result
val ftruncate: file -> len:int64 -> unit result

val stat : string -> stats result
val lstat : string -> stats result
val fstat : file -> stats result
val rename : src:string -> dst:string -> unit result
val link : target:string -> link_name:string -> unit result

(** @param mode default [S_Default] *)
val symlink :
  ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit result
val mkdtemp : string -> string result

(** @param pos default 0
    @param len [Int64.max_int] *)
val sendfile :
  ?pos:int64 -> ?len:int64 -> dst:file -> src:file -> unit -> int64 result
val utime : string -> access:float -> modif:float -> unit result
val futime : file -> access:float -> modif:float -> unit result
val readlink : string -> string result

val access : string -> access_permission list -> unit result
val chmod : string -> perm:int -> unit result
val fchmod : file -> perm:int -> unit result
val chown : string -> uid:int -> gid:int -> unit result
val fchown : file -> uid:int -> gid:int -> unit result
val scandir : string -> (file_kind * string) array result
