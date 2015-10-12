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

(** UWT_- error codes are introduced by uwt. *)
type error =
  | E2BIG
  | EACCES
  | EADDRINUSE
  | EADDRNOTAVAIL
  | EAFNOSUPPORT
  | EAGAIN
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
  | EAI_SOCKTYPE
  | EALREADY
  | EBADF
  | EBUSY
  | ECANCELED
  | ECHARSET (** Windows filenames (and similar parameters) are
                 expected to be in utf8. ECHARSET is returned, if one
                 parameter contains invalid unicode *)
  | ECONNABORTED
  | ECONNREFUSED
  | ECONNRESET
  | EDESTADDRREQ
  | EEXIST
  | EFAULT
  | EFBIG
  | EHOSTUNREACH
  | EINTR
  | EINVAL
  | EIO
  | EISCONN
  | EISDIR
  | ELOOP
  | EMFILE
  | EMSGSIZE
  | ENAMETOOLONG
  | ENETDOWN
  | ENETUNREACH
  | ENFILE
  | ENOBUFS
  | ENODEV
  | ENOENT
  | ENOMEM
  | ENONET
  | ENOPROTOOPT
  | ENOSPC
  | ENOSYS
  | ENOTCONN
  | ENOTDIR
  | ENOTEMPTY
  | ENOTSOCK
  | ENOTSUP
  | EPERM
  | EPIPE
  | EPROTO
  | EPROTONOSUPPORT
  | EPROTOTYPE
  | ERANGE
  | EROFS
  | ESHUTDOWN
  | ESPIPE
  | ESRCH
  | ETIMEDOUT
  | ETXTBSY
  | EXDEV
  | UNKNOWN
  | EOF
  | ENXIO
  | EMLINK
  | UWT_UNKNOWN (** Can't translate the error code. Perhaps your libuv version
                    is too new or too old *)
  | UWT_EFATAL (** something happened that the author of uwt didn't expect.
                   Probably a bug or a the api of libuv has changed in the
                   meanwhile *)
  | UWT_EBADF  (** you've already closed this handle/request *)
  | UWT_EINVAL (** one of your parameters doesn't look valid, e.g. negative
                   integer when only positive integers are expected *)
  | UWT_ENOTACTIVE (** e.g. you've tried to stop a timer, that wasn't active *)
  | UWT_EBUSY (** e.g. reported, if you try to use Uwt.Stream.read_start,
                  while you've already registered another callback for this
                  event *)
  | UWT_ENOENT (** entry not found, [Not_found] message for callbacks *)
  | UWT_EUNAVAIL (** you've tried to call a function, that is not supported,
                     either by your os or your libuv installation *)

exception Uwt_error of error * string * string

(** error message for the given error code *)
val strerror: error -> string

(** error name for the given error code *)
val err_name: error -> string

type ('a , 'b) o_result = ('a, 'b) Result.result =
  | Ok of 'a
  | Error of 'b
type 'a result = ('a , error) o_result

module Int_result : sig
  (** [Int_result.t] is used instead of ['a result], if a function returns either an
      error or a non-negative integer (including unit/bool).
      This way, we can avoid an extra allocation. *)
  type 'a t = private int

  type real_int = int
  type real_unit = unit
  type int = real_int t
  type unit = real_unit t

  val is_ok : 'a t -> bool
  val is_error : 'a t -> bool

  (** will raise [Invalid_argument], if {!is_ok} is false *)
  val to_int : int -> real_int

  (** will raise [Invalid_argument], if {!is_error} is false *)
  val to_error: 'a t -> error

  val plain : 'a t -> real_int
  (*
  (** create a thread that fails with an [Uv_error] - or [Invalid_argument],
      if {!is_error} is false *)
  val fail: ?name:string -> ?param:string -> 'a t -> 'b Lwt.t
  *)
  val to_exn: ?name:string -> ?param:string -> 'a t -> exn
  val raise_exn: ?name:string -> ?param:string -> 'a t -> 'b

  (** You can use the following values to compare a Int_result.t value with an
      assumed error, e.g.

      {[
      let p = Uwt.Stream.try_write t ~buf in
      if (p :> int) = Uwt.Int_result.eagain then
        ...
      else
        ...
      ]}

      All values are negative integers, but don't make any further
      assumption about them. Their concrete values can change with any
      uwt release.  *)

  val eagain : int
  val e2big : int
  val eacces : int
  val eaddrinuse : int
  val eaddrnotavail : int
  val eafnosupport : int
  val eai_addrfamily : int
  val eai_again : int
  val eai_badflags : int
  val eai_badhints : int
  val eai_canceled : int
  val eai_fail : int
  val eai_family : int
  val eai_memory : int
  val eai_nodata : int
  val eai_noname : int
  val eai_overflow : int
  val eai_protocol : int
  val eai_service : int
  val eai_socktype : int
  val ealready : int
  val ebadf : int
  val ebusy : int
  val ecanceled : int
  val echarset : int
  val econnaborted : int
  val econnrefused : int
  val econnreset : int
  val edestaddrreq : int
  val eexist : int
  val efault : int
  val efbig : int
  val ehostunreach : int
  val eintr : int
  val einval : int
  val eio : int
  val eisconn : int
  val eisdir : int
  val eloop : int
  val emfile : int
  val emsgsize : int
  val enametoolong : int
  val enetdown : int
  val enetunreach : int
  val enfile : int
  val enobufs : int
  val enodev : int
  val enoent : int
  val enomem : int
  val enonet : int
  val enoprotoopt : int
  val enospc : int
  val enosys : int
  val enotconn : int
  val enotdir : int
  val enotempty : int
  val enotsock : int
  val enotsup : int
  val eperm : int
  val epipe : int
  val eproto : int
  val eprotonosupport : int
  val eprototype : int
  val erange : int
  val erofs : int
  val eshutdown : int
  val espipe : int
  val esrch : int
  val etimedout : int
  val etxtbsy : int
  val exdev : int
  val unknown : int
  val eof : int
  val enxio : int
  val emlink : int
  val uwt_unknown : int
  val uwt_efatal : int
  val uwt_ebadf : int
  val uwt_einval : int
  val uwt_enotactive : int
  val uwt_ebusy : int
  val uwt_enoent: int
  val uwt_eunavail: int
end

(** abstract type for a file descriptor *)
type file

(** similar to [Unix.sockaddr], but abstract *)
type sockaddr

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val stdin : file
val stdout : file
val stderr : file

module Fs_types : sig
  type uv_open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK (** Unix only, ignored otherwise *)
    | O_CREAT
    | O_EXCL
    | O_TRUNC
    | O_APPEND
    | O_NOCTTY (** Unix only, ignored otherwise *)
    | O_DSYNC (** only supported on some Unix platforms, ignored otherwise *)
    | O_SYNC (** only supported on some Unix platforms, ignored otherwise *)
    | O_RSYNC (** only supported on some Unix platforms, ignored otherwise *)
    | O_TEMPORARY (** windows only, ignored on Unix *)
    | O_SHORT_LIVED (** windows only, ignored on Unix *)
    | O_SEQUENTIAL (** windows only, ignored on Unix *)
    | O_RANDOM (** windows only, ignored on Unix *)
  (** [O_CLOEXEC] and [O_SHARE_DELETE], [O_SHARE_WRITE],
      [O_SHARE_READ] don't exist, because these flags are
      unconditionally added by libuv, if the platform supports
      them. *)

  type file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
    | S_UNKNOWN

  type symlink_mode =
    | S_Default
    | S_Dir
    | S_Junction

  type access_permission =
    | Read
    | Write
    | Exec
    | Exists

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

  (** @param perm defaults are 0o644 *)
  val openfile : ?perm:int -> mode:uv_open_flag list -> string -> file t

  (** @param pos default is always zero
      @param len default is always the length of the string / array / Bytes.t *)
  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int t

  (** _ba function are unsafe. Bigarrays are passed directly to libuv (no
      copy to c heap or stack). *)
  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t

  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t

  val close : file -> unit t

  val unlink : string -> unit t

  (** @param perm defaults are 0o777 *)
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

  (** @param mode default [S_Default] *)
  val symlink :
    ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit t
  val mkdtemp : string -> string t

  (** @param pos default 0
      @param len [Nativeint.max_int] *)
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
end

module Conv : sig
  (** be careful in case of [Unix.ADDR_UNIX path]. If path is very
      long, {!of_unix_sockaddr} will raise an exception
      ([Unix.Unix_error]) and {!to_unix_sockaddr} might return a
      truncated string.  [Unix.ADDR_UNIX] is not supported on windows,
      {!of_unix_sockaddr} will also raise an exception in this
      case. *)
  val of_unix_sockaddr_exn : Unix.sockaddr -> sockaddr
  val to_unix_sockaddr_exn : sockaddr -> Unix.sockaddr

  (** the following functions always succeed on Unix - but not on windows *)
  val file_of_file_descr : Unix.file_descr -> file option
  val file_descr_of_file : file -> Unix.file_descr option
end

module Misc : sig
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

  val guess_handle: file -> handle_type

  val resident_set_memory : unit -> int64 result
  val resident_set_memory_exn : unit -> int64

  val uptime : unit -> float result
  val uptime_exn : unit -> float

  val getrusage : unit -> rusage result
  val getrusage_exn : unit -> rusage

  val cpu_info : unit -> cpu_info array result
  val cpu_info_exn : unit -> cpu_info array

  val interface_addresses: unit -> interface_address array result
  val interface_addresses_exn: unit -> interface_address array

  val load_avg: unit -> float * float * float

  val ip4_addr: string -> int -> sockaddr result
  val ip4_addr_exn: string -> int -> sockaddr

  val ip4_name: sockaddr -> string result
  val ip4_name_exn: sockaddr -> string

  val ip6_addr: string -> int -> sockaddr result
  val ip6_addr_exn: string -> int -> sockaddr

  val ip6_name: sockaddr -> string result
  val ip6_name_exn: sockaddr -> string

  val get_total_memory: unit -> int64
  val hrtime: unit -> int64

  type version = {
    major: int;
    minor: int;
    patch: int;
  }
  val version: unit -> version
  val version_raw: unit -> int
  val version_string: unit -> string
  val os_homedir: unit -> string result

  (** like Sys.executable_name , but utf-8 encoded under windows
      and perhaps more reliable under niche operating systems *)
  val exepath: unit -> string result
end

module Sys_info : sig
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

  (** {!os} is determined at compile time (by the c compiler) and
      might differ from the later OS *)
  val os : os

  type win_version = {
    major_version: int;
    minor_version: int;
    build_number: int;
    platform_id: int;
    csd_version: string;
  }

  (** Wrapper around GetVersionEx. It will always return [Error
      UWT_EUNAVAIL] on non windows systems.
      Your application must be manifested, otherwise you will get
      wrong informations on windows 8.1 and newer *)
  val win_version: unit -> win_version result
end
