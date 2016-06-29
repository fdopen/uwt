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
#include "config.inc"

type 'a uv_result = ('a , error) result

external strerror: error -> string = "uwt_strerror"
exception Uwt_error of error * string * string

let to_unix_error x =
  match x with
  | E2BIG -> Unix.E2BIG
  | EACCES -> Unix.EACCES
  | EADDRINUSE -> Unix.EADDRINUSE
  | EADDRNOTAVAIL -> Unix.EADDRNOTAVAIL
  | EAFNOSUPPORT -> Unix.EAFNOSUPPORT
  | EAGAIN -> Unix.EAGAIN
  | EALREADY -> Unix.EALREADY
  | EBADF -> Unix.EBADF
  | EBUSY -> Unix.EBUSY
  | ECONNABORTED -> Unix.ECONNABORTED
  | ECONNREFUSED -> Unix.ECONNREFUSED
  | ECONNRESET -> Unix.ECONNRESET
  | EDESTADDRREQ -> Unix.EDESTADDRREQ
  | EEXIST -> Unix.EEXIST
  | EFAULT -> Unix.EFAULT
  | EFBIG -> Unix.EFBIG
  | EHOSTUNREACH -> Unix.EHOSTUNREACH
  | EINTR -> Unix.EINTR
  | EINVAL -> Unix.EINVAL
  | EIO -> Unix.EIO
  | EISCONN -> Unix.EISCONN
  | EISDIR -> Unix.EISDIR
  | ELOOP -> Unix.ELOOP
  | EMFILE -> Unix.EMFILE
  | EMLINK -> Unix.EMLINK
  | EMSGSIZE -> Unix.EMSGSIZE
  | ENAMETOOLONG -> Unix.ENAMETOOLONG
  | ENETDOWN -> Unix.ENETDOWN
  | ENETUNREACH -> Unix.ENETUNREACH
  | ENFILE -> Unix.ENFILE
  | ENOBUFS -> Unix.ENOBUFS
  | ENODEV -> Unix.ENODEV
  | ENOENT -> Unix.ENOENT
  | ENOMEM -> Unix.ENOMEM
  | ENOPROTOOPT -> Unix.ENOPROTOOPT
  | ENOSPC -> Unix.ENOSPC
  | ENOSYS -> Unix.ENOSYS
  | ENOTCONN -> Unix.ENOTCONN
  | ENOTDIR -> Unix.ENOTDIR
  | ENOTEMPTY -> Unix.ENOTEMPTY
  | ENOTSOCK -> Unix.ENOTSOCK
  | ENXIO -> Unix.ENXIO
  | EPERM -> Unix.EPERM
  | EPIPE -> Unix.EPIPE
  | EPROTONOSUPPORT -> Unix.EPROTONOSUPPORT
  | EPROTOTYPE -> Unix.EPROTOTYPE
  | ERANGE -> Unix.ERANGE
  | EROFS -> Unix.EROFS
  | ESHUTDOWN -> Unix.ESHUTDOWN
  | ESPIPE -> Unix.ESPIPE
  | ESRCH -> Unix.ESRCH
  | ETIMEDOUT -> Unix.ETIMEDOUT
  | EXDEV -> Unix.EXDEV
  | UWT_EBADF -> Unix.EBADF
  | UWT_EBUSY -> Unix.EBUSY
  | UWT_EINVAL -> Unix.EINVAL
  | UWT_ENOENT -> Unix.ENOENT
  | ETXTBSY
  | UNKNOWN
  | UWT_EFATAL
  | UWT_ENOTACTIVE
  | UWT_UNKNOWN
  | UWT_EUNAVAIL
  | EAI_ADDRFAMILY
  | EAI_AGAIN
  | EAI_BADFLAGS
  | EAI_BADHINTS
  | EAI_CANCELED
  | EAI_FAIL
  | EAI_FAMILY
  | EAI_MEMORY
  | EAI_NODATA
  | EAI_NONAME
  | EAI_OVERFLOW
  | EAI_PROTOCOL
  | EAI_SERVICE
  | ECANCELED
  | ECHARSET
  | ENONET
  | ENOTSUP
  | EOF
  | EPROTO
  | EAI_SOCKTYPE -> Unix.EUNKNOWNERR (Obj.magic x)

let to_unix_exn = function
| Uwt_error(x,y,z) -> Unix.Unix_error(to_unix_error x,y,z)
| x -> x

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

  external plain : 'a t -> real_int = "%identity"

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
      raise (Uwt_error(transform x,name,param))

  let to_exn ?(name="") ?(param="") (x: 'a t) =
    if x >= 0 then
      Invalid_argument "Uwt.Int_result.to_exn"
    else
      Uwt_error(transform x,name,param)

#include "error_val.ml"
end

type file = Unix.file_descr

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type sockaddr

let stdin : file = Unix.stdin
let stdout : file = Unix.stdout
let stderr : file = Unix.stderr

module Fs_types = struct
  type uv_open_flag =
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

module type Fs_functions = sig
  include module type of Fs_types
  with type uv_open_flag = Fs_types.uv_open_flag
  with type file_kind = Fs_types.file_kind
  with type symlink_mode = Fs_types.symlink_mode
  with type access_permission = Fs_types.access_permission
  with type stats = Fs_types.stats
  type 'a t
  val openfile : ?perm:int -> mode:uv_open_flag list -> string -> file t
  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  val close : file -> unit t
  val unlink : string -> unit t
  val mkdir : ?perm:int -> string -> unit t
  val rmdir : string -> unit t
  val fsync : file -> unit t
  val fdatasync : file -> unit t
  val ftruncate: file -> len:int64 -> unit t
  val stat : string -> stats t
  val lstat : string -> stats t
  val fstat : file -> stats t
  val rename : src:string -> dst:string -> unit t
  val link : target:string -> link_name:string -> unit t
  val symlink :
    ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit t
  val mkdtemp : string -> string t
  val sendfile :
    ?pos:int64 -> ?len:nativeint -> dst:file -> src:file -> unit -> nativeint t
  val utime : string -> access:float -> modif:float -> unit t
  val futime : file -> access:float -> modif:float -> unit t
  val readlink : string -> string t
  val access : string -> access_permission list -> unit t
  val chmod : string -> perm:int -> unit t
  val fchmod : file -> perm:int -> unit t
  val chown : string -> uid:int -> gid:int -> unit t
  val fchown : file -> uid:int -> gid:int -> unit t
  val scandir : string -> (file_kind * string) array t
  val realpath: string -> string t
end

module Conv = struct
  type usockaddr = Unix.sockaddr =
    | ADDR_UNIX of string
    | ADDR_INET of Unix.inet_addr * int

  external of_unix_sockaddr_exn :
    usockaddr -> sockaddr = "uwt_of_sockaddr"
  external to_unix_sockaddr_exn :
    sockaddr -> usockaddr = "uwt_to_sockaddr"

#if HAVE_WINDOWS = 0
  let file_of_file_descr s = Some s
#else
  external set_crtfd : Unix.file_descr -> bool = "uwt_set_crtfd_na"
  let file_of_file_descr s = if set_crtfd s then Some s else None
#endif
  (* it might change in the future , therefore I continue to wrap it in
     an option *)
  let file_descr_of_file f = Some f
end

module Misc = struct

  let to_exn n = function
  | Ok x -> x
  | Error x -> raise (Uwt_error(x,n,""))

  type timeval = {
    sec: int; usec: int;
  }

  type rusage = {
    utime: timeval;
    stime: timeval;
    maxrss: int64;
    ixrss: int64;
    idrss: int64;
    isrss: int64;
    minflt: int64;
    majflt: int64;
    nswap: int64;
    inblock: int64;
    outblock: int64;
    msgsnd: int64;
    msgrcv: int64;
    nsignals: int64;
    nvcsw: int64;
    nivcsw: int64;
  }

  type cpu_times = {
    user: int64;
    nice: int64;
    sys: int64;
    idle: int64;
    irq: int64;
  }

  type cpu_info = {
    model: string;
    speed: int;
    cpu_times: cpu_times;
  }

  type interface_address = {
    name: string;
    phys_addr: string;
    is_internal: bool;
    address: sockaddr;
    netmask: sockaddr;
  }

  type handle_type =
    | File
    | Tty
    | Pipe
    | Tcp
    | Udp
    | Unknown

  external guess_handle:
    file -> handle_type = "uwt_guess_handle_na" "noalloc"

  external resident_set_memory:
    unit -> int64 uv_result = "uwt_resident_set_memory"
  let resident_set_memory_exn () =
    resident_set_memory () |> to_exn "uv_resident_set_memory"

  external uptime: unit -> float uv_result = "uwt_uptime"
  let uptime_exn () = uptime () |> to_exn "uv_uptime"

  external getrusage : unit -> rusage uv_result = "uwt_getrusage"
  let getrusage_exn () = getrusage () |> to_exn "uv_getrusage"

  external cpu_info: unit -> cpu_info array uv_result = "uwt_cpu_info"
  let cpu_info_exn () = cpu_info () |> to_exn "uv_cpu_info"

  external interface_addresses:
    unit -> interface_address array uv_result = "uwt_interface_addresses"
  let interface_addresses_exn () =
    interface_addresses () |> to_exn "uv_interface_addresses"

  external load_avg: unit -> float * float * float = "uwt_load_avg"

  external ip4_addr: string -> int -> sockaddr uv_result = "uwt_ip4_addr"
  let ip4_addr_exn s i = ip4_addr s i |> to_exn "uv_ip4_addr"
  external ip4_name: sockaddr -> string uv_result = "uwt_ip4_name"
  let ip4_name_exn s = ip4_name s |> to_exn "uv_ip4_name"

  external ip6_addr: string -> int -> sockaddr uv_result = "uwt_ip6_addr"
  let ip6_addr_exn s i = ip6_addr s i |> to_exn "uv_ip6_addr"
  external ip6_name: sockaddr -> string uv_result = "uwt_ip6_name"
  let ip6_name_exn s = ip6_name s |> to_exn "uv_ip6_name"

  external get_total_memory: unit -> int64 = "uwt_get_total_memory"
  external hrtime: unit -> int64 = "uwt_hrtime"

  type version = {
    major: int;
    minor: int;
    patch: int;
  }

  external version_raw: unit -> int = "uwt_version_na" "noalloc"
  let version () =
    let n = version_raw () in
    {
      patch = n land 0xff;
      minor = (n lsr 8 ) land 0xff;
      major = (n lsr 16) land 0xff
    }

  external version_string: unit -> string = "uwt_version_string"
  external os_homedir: unit -> string uv_result = "uwt_os_homedir"
  external exepath: unit -> string uv_result = "uwt_exepath"
  external os_tmpdir: unit -> string uv_result = "uwt_os_tmpdir"
  external get_passwd: unit -> Unix.passwd_entry uv_result = "uwt_get_passwd"

  external cwd: unit -> string uv_result = "uwt_cwd"
  external chdir: string -> Int_result.unit = "uwt_chdir"

  external get_process_title:
    string array -> string uv_result = "uwt_get_process_title"
  let get_process_title () = get_process_title Sys.argv
  external set_process_title:
    string array -> string -> Int_result.unit = "uwt_set_process_title_na" "noalloc"
  let set_process_title s = set_process_title Sys.argv s

end

module Sys_info = struct
  type os =
    | Windows
    | Android
    | Linux
    | Mac
    | Freebsd
    | Openbsd
    | Cygwin
    | Netbsd
    | Sun
    | Hp
    | Dragonfly
    | Aix
    | Minix
    | Bsd
    | Unknown
  let os : os = OS_MACRO

  type win_version = {
    major_version: int;
    minor_version: int;
    build_number: int;
    platform_id: int;
    csd_version: string;
  }
#if HAVE_WINDOWS = 0
  let win_version () = Error UWT_EUNAVAIL
#else
  external win_version: unit -> win_version uv_result = "uwt_win_version"
#endif
end
