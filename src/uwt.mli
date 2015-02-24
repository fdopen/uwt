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

(** Uwt main module *)

(**
 Basic overview:

 - naming conventions mirror the conventions of libuv, so you can easily
   consult the official libuv manual. Only the differences are explained
   here.

 - Requests are translated to lwt-threads, therefore [uv_req_t] is kept
   internal.

 - Callbacks that are called continually are most of the time not
   translated to the usual lwt semantic.

 - Uwt is {b not} {i compatible} with [lwt.unix]. It's not a further
   [Lwt_engine] in addition to [select] and [libev].

 - Uwt is {b not} {i thread safe}. All uwt functions should be called from your
   main thread.

 - Uwt is in an early alpha stage. The interface is likely to
   change. Feel free to open an issue an make suggestions about it :)


   Please notice, that there are subtile differences compared to
   [lwt.unix]. Because all requests are accomplished by libuv
   (sometimes in parallel in different threads), you don't have that
   kind of low level control, that you have with 'lwt.unix'.  For
   example it's not guaranteed that in the following example only one
   operation is executed:

   {[
     Lwt.pick [ op_timeout ; op_read ; op_write; op_xyz ]
   ]}

 **)


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
  | ECHARSET
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
  | UWT_EFATAL (** something happend that the author of uwt didn't expect.
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

exception Uwt_error of error * string * string

(** error message for the given error code *)
val strerror: error -> string

(** error name for the given error code *)
val err_name: error -> string

type 'a ret =
  | Ok of 'a
  | Error of error

module Result : sig
  (** [Result.t] is used instead of ['a ret], if a function returns either an
      error or a non-negative integer (including unit/bool).
      This way, we can avoid an extra allocation. *)
  type 'a t = private int
  val is_ok : 'a t -> bool
  val is_error : 'a t -> bool

  (** will raise [Invalid_argument], if {!is_ok} is false *)
  val to_int : int t -> int

  (** will raise [Invalid_argument], if {!is_error} is false *)
  val to_error: 'a t -> error

  (** create a thread that fails with an [Uwt_error] - or [Invalid_argument],
      if {!is_error} is false *)
  val fail: ?name:string -> ?param:string -> 'a t -> 'b Lwt.t

  val raise_exn: ?name:string -> ?param:string -> 'a t -> 'b

  (** You can use the following values to compare a Result.t value with an
      assumed error, e.g.

      {[
      let p = Uwt.Stream.try_write t ~buf in
      if (p :> int) = Uwt.Result.eagain then
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
end

type file (** abstract type for a file descriptor *)
type sockaddr (** similar to [Unix.sockaddr], but abstract *)
type socket

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val stdin : file
val stdout : file
val stderr : file

module Main : sig
  (** Analogous to [Lwt_main] *)

  (** Main_error is thrown, when uv_run returns an error - or if lwt
      doesn't report any result and libuv reports, that there are no
      pending tasks.  *)
  exception Main_error of error * string
  val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  val yield : unit -> unit Lwt.t
  val run : 'a Lwt.t -> 'a
  val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  val at_exit : (unit -> unit Lwt.t) -> unit
end

module Fs : sig
  type open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_CREAT
    | O_EXCL
    | O_TRUNC
    | O_APPEND

  (** @param perm defaults are 0o644 *)
  val openfile : ?perm:int -> mode:open_flag list -> string -> file Lwt.t

  (** @param pos default is always zero
      @param len default is always the length ofthe string / array, etc. *)
  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int Lwt.t

  (** _ba function are unsafe. Bigarrays are passed directly to libuv (no
      copy to c heap or stack). *)
  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int Lwt.t

  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int Lwt.t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int Lwt.t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int Lwt.t

  val close : file -> unit Lwt.t

  val unlink : string -> unit Lwt.t

  (** @param perm defaults are 0o755 *)
  val mkdir : ?perm:int -> string -> unit Lwt.t

  val rmdir : string -> unit Lwt.t

  val fsync : file -> unit Lwt.t
  val fdatasync : file -> unit Lwt.t
  val ftruncate: file -> int64 -> unit Lwt.t

  type file_kind =
    | S_REG
    | S_DIR
    | S_CHR
    | S_BLK
    | S_LNK
    | S_FIFO
    | S_SOCK
    | S_UNKNOWN

  type stats = {
    st_dev : int;
    st_kind : file_kind;
    st_perm : int;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_ino : int;
    st_size : int64;
    st_blksize : int;
    st_blocks : int;
    st_flags : int;
    st_gen : int;
    st_atime : float;
    st_mtime : float;
    st_ctime : float;
    st_birthtime : float;
  }

  val stat : string -> stats Lwt.t
  val lstat : string -> stats Lwt.t
  val fstat : file -> stats Lwt.t
  val rename : src:string -> dst:string -> unit Lwt.t
  val link : target:string -> string -> unit Lwt.t

  type symlink_mode =
    | S_Default
    | S_Dir
    | S_Junction

  (** @param mode default [S_Default] *)
  val symlink :
    ?mode:symlink_mode -> target:string -> string -> unit Lwt.t
  val mkdtemp : string -> string Lwt.t

  (** @param pos default 0
      @param len [Int64.max_int] *)
  val sendfile :
    ?pos:int64 -> ?len:int64 -> dst:file -> src:file -> unit -> int64 Lwt.t
  val utime : string -> access:float -> modif:float -> unit Lwt.t
  val futime : file -> access:float -> modif:float -> unit Lwt.t
  val readlink : string -> string Lwt.t
  type access_permission = Read | Write | Exec | Exists
  val access : string -> access_permission list -> unit Lwt.t
  val chmod : string -> perm:int -> unit Lwt.t
  val fchmod : file -> perm:int -> unit Lwt.t
  val chown : string -> uid:int -> gid:int -> unit Lwt.t
  val fchown : file -> uid:int -> gid:int -> unit Lwt.t
  val scandir : string -> (file_kind * string) array Lwt.t
end

module Handle : sig
  type t
  val close : t -> unit Lwt.t
  val close_noerr : t -> unit
  val is_active : t -> bool
end

module Handle_ext : sig
  type t
  val get_send_buffer_size : t -> int Result.t
  val get_send_buffer_size_exn : t -> int

  val get_recv_buffer_size : t -> int Result.t
  val get_recv_buffer_size_exn : t -> int

  val set_send_buffer_size : t -> int -> unit Result.t
  val set_send_buffer_size_exn : t -> int -> unit

  val set_recv_buffer_size : t -> int -> unit Result.t
  val set_recv_buffer_size_exn : t -> int -> unit
end

module Stream : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  val is_readable : t -> bool
  val is_writable : t -> bool

  val read_start : t -> cb:(Bytes.t ret -> unit) -> unit Result.t
  val read_start_exn : t -> cb:(Bytes.t ret -> unit) -> unit

  val read_stop : t -> unit Result.t
  val read_stop_exn : t -> unit

  (** There is currently no uv_read function in libuv, just
   *  read_start and read_stop.
   *  This is a wrapper for your convenience. It calls read_stop internally, if
   *  you don't continue with reading immediately. Zero result indicates EOF  *)
  val read : ?pos:int -> ?len:int -> t -> buf:bytes -> int Lwt.t
  val read_ba : ?pos:int -> ?len:int -> t -> buf:buf -> int Lwt.t

  val write_queue_size : t -> int

  val write : ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t
  val write_string : ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t
  val write_ba : ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  val write2 : ?pos:int -> ?len:int -> buf:bytes -> send:t -> t -> unit Lwt.t

  val try_write : ?pos:int -> ?len:int -> t -> buf:bytes -> int Result.t
  val try_write_exn : ?pos:int -> ?len:int -> t -> buf:bytes -> int

  val listen:
    t -> back:int -> cb:( t -> unit Result.t -> unit ) -> unit Result.t
  val listen_exn :
    t -> back:int -> cb:( t -> unit Result.t -> unit ) -> unit

  val accept_raw: server:t -> client:t -> unit Result.t
  val accept_raw_exn: server:t -> client:t -> unit

  val shutdown: t -> unit Lwt.t
end

module Pipe : sig
  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  val to_stream: t -> Stream.t

  (** @param ipc is false by default *)
  val init : ?ipc:bool -> unit -> t ret
  val init_exn : ?ipc:bool -> unit -> t

  (** @param ipc is false by default *)
  val openpipe : ?ipc:bool -> file -> t ret
  val openpipe_exn : ?ipc:bool -> file -> t

  val bind: t -> string -> unit Result.t
  val bind_exn : t -> string -> unit

  val getsockname: t -> string ret
  val getsockname_exn : t -> string

  val pending_instances: t -> int -> unit Result.t
  val pending_instances_exn : t -> int -> unit

  val pending_count: t -> int Result.t
  val pending_count_exn : t -> int

  type pending_type =
    | Unknown
    | Tcp
    | Udp
    | Pipe
  val pending_type: t -> pending_type

  val connect: t -> string -> unit Lwt.t
end

module Tcp : sig
  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  val to_stream : t -> Stream.t

  val init : unit -> t ret
  val init_exn : unit -> t

  type mode =
    | Ipv6_only

  val opentcp : socket -> t ret
  val opentcp_exn : socket -> t

  (** @param mode: default is the empty list *)
  val bind : ?mode:mode list -> t -> sockaddr -> unit Result.t
  val bind_exn : ?mode:mode list -> t -> sockaddr -> unit

  val nodelay : t -> bool -> unit Result.t
  val nodelay_exn : t -> bool -> unit

  val keepalive : t -> bool -> unit Result.t
  val keepalive_exn : t -> bool -> unit

  val simultaneous_accepts : t -> bool -> unit Result.t
  val simultaneous_accepts_exn : t -> bool -> unit

  val getsockname : t -> sockaddr ret
  val getsockname_exn : t -> sockaddr

  val getpeername : t -> sockaddr ret
  val getpeername_exn : t -> sockaddr

  val connect : t -> sockaddr -> unit Lwt.t

  (** initializes a new client and calls accept_raw with it *)
  val accept: t -> t ret
  val accept_exn: t -> t
end

module Udp : sig
  type t
  include module type of Handle with type t := t
  include module type of Handle_ext with type t := t
  val to_handle : t -> Handle.t

  val send_queue_size: t -> int
  val send_queue_count: t -> int

  val init : unit -> t ret
  val init_exn : unit -> t

  val openudp : socket -> t ret
  val openudp_exn : socket -> t

  type mode =
    | Ipv6_only
    | Reuse_addr

  (** @param mode default mode is the empty list *)
  val bind : ?mode:mode list -> t -> sockaddr -> unit Result.t
  val bind_exn : ?mode:mode list -> t -> sockaddr -> unit

  val getsockname : t -> sockaddr ret
  val getsockname_exn : t -> sockaddr

  type membership =
    | Leave_group
    | Join_group

  val set_membership :
    t -> multicast:string -> interface:string -> membership -> unit Result.t
  val set_membership_exn :
    t -> multicast:string -> interface:string -> membership -> unit

  val set_multicast_loop : t -> bool -> unit Result.t
  val set_multicast_loop_exn : t -> bool -> unit

  val set_multicast_ttl : t -> int -> unit Result.t
  val set_multicast_ttl_exn : t -> int -> unit

  val set_multicast_interface : t -> string -> unit Result.t
  val set_multicast_interface_exn : t -> string -> unit

  val set_broadcast : t -> bool -> unit Result.t
  val set_broadcast_exn : t -> bool -> unit

  val set_ttl : t -> int -> unit Result.t
  val set_ttl_exn : t -> int -> unit

  val send : ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> unit Lwt.t
  val send_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> unit Lwt.t

  val try_send :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> int Result.t
  val try_send_exn :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> int

  (** The type definition will likely be changed.
      Don't use fragile pattern matching for it *)
  type recv_result =
    | Data of (Bytes.t * sockaddr option)
    | Partial_data of (Bytes.t * sockaddr option)
    | Empty_from of sockaddr
    | Transmission_error of error

  val recv_start : t -> cb:(recv_result -> unit) -> unit Result.t
  val recv_start_exn : t -> cb:(recv_result -> unit) -> unit

  val recv_stop : t -> unit Result.t
  val recv_stop_exn : t -> unit
end

module Tty : sig
  type t
  include module type of Stream with type t := t

  val to_stream: t -> Stream.t
  val init : file -> read:bool -> t ret
  val init_exn : file -> read:bool -> t

  type mode =
    | Normal
    | Raw
    | Io

  val set_mode : t -> mode:mode -> unit Result.t
  val set_mode_exn : t -> mode:mode -> unit

  val reset_mode : unit -> unit Result.t
  val reset_mode_exn : unit -> unit

  type winsize = {
    width: int;
    height: int;
  }
  val get_winsize : t -> winsize ret
  val get_winsize_exn : t -> winsize
end

module Timer : sig
  type t
  include module type of Handle with type t := t
  val to_handle: t -> Handle.t

  val sleep : int -> unit Lwt.t

  (** Timers, that are executed only once (repeat=0), are automatically closed.
      After their callback have been executed, their handles are invalid.  *)
  val start : repeat:int -> timeout:int -> cb:(t -> unit) -> t ret
  val start_exn : repeat:int -> timeout:int -> cb:(t -> unit) -> t

  (** a successful stop call will also close the handles *)
  val stop : t -> unit Result.t
  val stop_exn : t -> unit
end

module Signal : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  (** use Sys.sigterm, Sys.sigstop, etc *)
  val start : int -> cb:(t -> int -> unit) -> t ret
  val start_exn : int -> cb:(t -> int -> unit) -> t

  (** a successful stop call will also close the handles *)
  val stop : t -> unit Result.t
  val stop_exn : t -> unit
end

module Poll : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type event =
    | Readable
    | Writeable
    | Readable_writeable

  (** start and start_exn don't support windows *)
  val start : file -> event -> cb:(t -> event ret -> unit) -> t ret
  val start_exn : file -> event -> cb:(t -> event ret -> unit) -> t

  val start_socket : socket -> event -> cb:(t -> event ret -> unit) -> t ret
  val start_socket_exn : socket -> event -> cb:(t -> event ret -> unit) -> t

  (** a successful stop call will also close the handles *)
  val stop : t -> unit Result.t
  val stop_exn : t -> unit
end

module Fs_event : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type event =
    | Rename
    | Change

  type flags =
    | Entry
    | Stat
    | Recursive

  type cb = t -> (string * event list) ret -> unit

  val start : string -> flags list -> cb:cb -> t ret
  val start_exn : string -> flags list -> cb:cb -> t

  (** a successful stop call will also close the handles *)
  val stop : t -> unit Result.t
  val stop_exn : t -> unit
end

module Fs_poll : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type report = {
    prev : Fs.stats;
    curr : Fs.stats;
  }

  val start : string -> int -> cb:(t -> report ret -> unit) -> t ret
  val start_exn : string -> int -> cb:(t -> report ret -> unit) -> t

  (** a successful stop call will also close the handles *)
  val stop : t -> unit Result.t
  val stop_exn : t -> unit
end

module Process : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type stdio =
    | Inherit_file of file
    | Inherit_stream of Stream.t
    | Pipe of Pipe.t

  type exit_cb = t -> exit_status:int -> term_signal:int -> unit

  (** @param verbatim_arguments default false
      @param detach default false
      @param hide default true
   **)
  val spawn :
    ?stdin:stdio ->
    ?stdout:stdio ->
    ?stderr:stdio ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?hide:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list ->
    t ret

  val spawn_exn :
    ?stdin:stdio ->
    ?stdout:stdio ->
    ?stderr:stdio ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?hide:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list ->
    t

  val disable_stdio_inheritance : unit -> unit

  val pid : t -> int Result.t
  val pid_exn : t -> int

  val process_kill : t -> unit Result.t
  val process_kill_exn : t -> unit

  val kill : pid:int -> signum:int -> unit Result.t
  val kill_exn : pid:int -> signum:int -> unit
end

module Dns : sig
  (* these lines are only included because of ppx_import.
     Remove them later *)
  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type getaddrinfo_option = Unix.getaddrinfo_option

  type addr_info = {
    ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string;
  }

  val getaddrinfo :
    host:string -> service:string ->
    getaddrinfo_option list -> addr_info list Lwt.t

  type name_info = {
    hostname : string;
    service : string;
  }

  type getnameinfo_option = Unix.getnameinfo_option

  val getnameinfo : sockaddr -> getnameinfo_option list -> name_info Lwt.t
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

  val resident_set_memory : unit -> nativeint ret
  val resident_set_memory_exn : unit -> nativeint

  val uptime : unit -> float ret
  val uptime_exn : unit -> float

  val getrusage : unit -> rusage ret
  val getrusage_exn : unit -> rusage

  val cpu_info : unit -> cpu_info array ret
  val cpu_info_exn : unit -> cpu_info array

  val interface_addresses: unit -> interface_address array ret
  val interface_addresses_exn: unit -> interface_address array

  val load_avg: unit -> float * float * float

  val ip4_addr: string -> int -> sockaddr ret
  val ip4_addr_exn: string -> int -> sockaddr

  val ip4_name: sockaddr -> string ret
  val ip4_name_exn: sockaddr -> string

  val ip6_addr: string -> int -> sockaddr ret
  val ip6_addr_exn: string -> int -> sockaddr

  val ip6_name: sockaddr -> string ret
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
end

module Compat : sig
  val of_unix_sockaddr: Unix.sockaddr -> sockaddr
  val to_unix_sockaddr: sockaddr -> Unix.sockaddr

  (** the following functions always succeeds on *nix - but not on windows **)
  val file_of_file_descr : Unix.file_descr -> file option
  val socket_of_file_descr : Unix.file_descr -> socket option
end

module Unix : sig
  val gethostname : unit -> string Lwt.t

  type host_entry = Unix.host_entry
  val gethostbyname: string -> host_entry Lwt.t
  val gethostbyaddr: Unix.inet_addr -> host_entry Lwt.t

  type service_entry = Unix.service_entry

  val getservbyname: name:string -> protocol:string -> service_entry Lwt.t
  val getservbyport: int -> string -> service_entry Lwt.t
  val lseek: file -> int64 -> Unix.seek_command -> int64 Lwt.t
  val getaddrinfo:
    string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list Lwt.t

  val getprotobyname: string -> Unix.protocol_entry Lwt.t
  val getprotobynumber: int -> Unix.protocol_entry Lwt.t
end

(**/**)
(* Only for debugging.
   - Don't call it, while Main.run is active.
   - After you've called it, you can't run any uwt related functions *)
val valgrind_happy : unit -> unit
