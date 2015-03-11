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


   Please notice, that there are subtle differences compared to
   [lwt.unix]. Because all requests are accomplished by libuv
   (sometimes in parallel in different threads), you don't have that
   kind of low level control, that you have with [lwt.unix].  For
   example it's not guaranteed that in the following example only one
   operation is executed:

   {[
     Lwt.pick [ op_timeout ; op_read ; op_write; op_xyz ]
   ]}

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

exception Uwt_error of error * string * string

(** error message for the given error code *)
val strerror: error -> string

(** error name for the given error code *)
val err_name: error -> string

(** The official [result] type will be used in the future *)
type 'a result =
  | Ok of 'a
  | Error of error

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

  (** create a thread that fails with an [Uwt_error] - or [Invalid_argument],
      if {!is_error} is false *)
  val fail: ?name:string -> ?param:string -> 'a t -> 'b Lwt.t

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
  (** Analogue to [Lwt_main] *)

  (** Main_error is thrown, when uv_run returns an error - or if lwt
      doesn't report any result and libuv reports, that there are no
      pending tasks.  *)
  exception Main_error of error * string


  (** You shouldn't raise exceptions, if you are using lwt. Always use
      {!Lwt.fail}. If you throw exceptions nevertheless, uwt can
      usually not propagate the exceptions to the OCaml runtime
      immediately. The exceptions are stored internally an are
      re-thrown as soon as possible ([Deferred]). You can catch these
      exceptions below {!run}.

      However, uwt cannot catch all exceptions at the right
      moment. These exceptions are wrapped inside [Fatal]. Don't call
      any uwt function (especially {!run}) again, if you catch such an
      exception below {!run}. A workaround is currently not
      implemented, because only rare exceptions like [Out_of_memory]
      are 'fatal' under certain circumstances - and they usually mean
      you are in unrecoverable trouble anyway.

      {[
        let rec main t1  =
          match Uwt.Main.run t1 with
          | result -> let y = ... (* no error *)
          | exception Uwt.Main.Deferred(l) ->
            log_deferred l ; main t2 (* safe *)
          | exception Uwt.Main.Fatal(e,p) -> (* fatal, restart your process *)
            log_fatal e p ; cleanup () ; exit 2
          | exception x -> log_normal x ; main t3 (* safe *)
      ]}
  *)
  exception Deferred of (exn * Printexc.raw_backtrace) list
  exception Fatal of exn * Printexc.raw_backtrace

  val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  val yield : unit -> unit Lwt.t

  (** Unlike {!Lwt_main.run}, it's not allowed to nest calls to Uwt.Main.run.
      The following code is invalid, an exception [Main_error] will be thrown:
      {[
        let help () =
           let () = Uwt.Main.run foo in
           Lwt.return_unit
        in
        Uwt.Main.run (help ())
      ]}

      And {!Uwt.Main.run} will complain about missing work ([Main_error] again):
      {|
        let s,t = Lwt.task () in
        Uwt.Main.run s
      |}

      With [lwt.unix] the code above could lead to a busy loop (wasting your cpu
      time - but it's dependend on the selected  Lwt_engine).
      If you really want your process to run forever, without waiting for any
      i/o, you can create a [Uwt.Timer.t] that gets called repeatedly, but does
      nothing. *)
  val run : 'a Lwt.t -> 'a
  val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  val at_exit : (unit -> unit Lwt.t) -> unit
end

module Fs : sig
  type open_flag =
    | O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK (** *nix only, ignored otherwise *)
    | O_CREAT
    | O_EXCL
    | O_TRUNC
    | O_APPEND
    | O_NOCTTY (** *nix only, ignored otherwise *)
    | O_DSYNC (** only supported on some *nix platforms, ignored otherwise *)
    | O_SYNC (** only supported on some *nix platforms, ignored otherwise *)
    | O_RSYNC (** only supported on some *nix platforms, ignored otherwise *)
    | O_TEMPORARY (** windows only, ignored on *nix *)
    | O_SHORT_LIVED (** windows only, ignored on *nix *)
    | O_SEQUENTIAL (** windows only, ignored on *nix *)
    | O_RANDOM (** windows only, ignored on *nix *)
  (** [O_CLOEXEC] and [O_SHARE_DELETE], [O_SHARE_WRITE],
      [O_SHARE_READ] don't exist, because these flags are
      unconditionally added by libuv, if the platform supports
      them. *)

  (** @param perm defaults are 0o644 *)
  val openfile : ?perm:int -> mode:open_flag list -> string -> file Lwt.t

  (** @param pos default is always zero
      @param len default is always the length of the string / array / Bytes.t *)
  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int Lwt.t

  (** _ba function are unsafe. Bigarrays are passed directly to libuv (no
      copy to c heap or stack). *)
  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int Lwt.t

  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int Lwt.t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int Lwt.t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int Lwt.t

  val close : file -> unit Lwt.t

  val unlink : string -> unit Lwt.t

  (** @param perm defaults are 0o777 *)
  val mkdir : ?perm:int -> string -> unit Lwt.t

  val rmdir : string -> unit Lwt.t

  val fsync : file -> unit Lwt.t
  val fdatasync : file -> unit Lwt.t
  val ftruncate: file -> len:int64 -> unit Lwt.t

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

  val stat : string -> stats Lwt.t
  val lstat : string -> stats Lwt.t
  val fstat : file -> stats Lwt.t
  val rename : src:string -> dst:string -> unit Lwt.t
  val link : target:string -> link_name:string -> unit Lwt.t

  type symlink_mode =
    | S_Default
    | S_Dir
    | S_Junction

  (** @param mode default [S_Default] *)
  val symlink :
    ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit Lwt.t
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

  (** Handles are closed automatically, if they are not longer referenced from
      the OCaml heap. Nevertheless, you should nearly always close them with
      {!close}, because:

      - if they wrap a file descriptor, you will sooner or later run
        out of file descriptors. The OCaml garbage collector doesn't give any
        guarantee, when orphaned memory blocks are removed.

      - you might have registered some repeatedly called action (e.g. timeout,
        read_start,...), that prevent that all references get removed from the
        OCaml heap.

      However, it's safe to write code in this manner:
      {[
        let s = Uwt.Tcp.init () in
        let c = Uwt.Tcp.init () in
        Uwt.Tcp.nodelay s false;
        Uwt.Tcp.simultaneous_accepts true;
        if foobar () then (* no file descriptor yet assigned, no need to worry
                             about exceptions inside foobar,... *)
          Lwt.return_unit (* no need to close *)
        else
          ...
      ]}

      If you want - for whatever reason - keep a file descriptor open
      for the whole lifetime of your process, remember to keep a
      reference to its handle.  *)
  val close : t -> Int_result.unit
  val close_noerr : t -> unit

  (** Prefer {!close} or {!close_noerr} to {!close_wait}. {!close} or
      {!close_noerr} return immediately (there are no useful error
      messages, beside perhaps a notice, that you've already closed that
      handle).

      {!close_wait} is only useful, if you intend to wait until all
      concurrent write and read threads related to this handle are
      canceled. *)
  val close_wait : t -> unit Lwt.t
  val is_active : t -> bool
end

module Handle_ext : sig
  type t
  val get_send_buffer_size : t -> Int_result.int
  val get_send_buffer_size_exn : t -> int

  val get_recv_buffer_size : t -> Int_result.int
  val get_recv_buffer_size_exn : t -> int

  val set_send_buffer_size : t -> int -> Int_result.unit
  val set_send_buffer_size_exn : t -> int -> unit

  val set_recv_buffer_size : t -> int -> Int_result.unit
  val set_recv_buffer_size_exn : t -> int -> unit
end

module Stream : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  val is_readable : t -> bool
  val is_writable : t -> bool

  val read_start : t -> cb:(Bytes.t result -> unit) -> Int_result.unit
  val read_start_exn : t -> cb:(Bytes.t result -> unit) -> unit

  val read_stop : t -> Int_result.unit
  val read_stop_exn : t -> unit

  (** There is currently no uv_read function in libuv, just read_start and
   *  read_stop.
   *  This is a wrapper for your convenience. It calls read_stop internally, if
   *  you don't continue with reading immediately. Zero result indicates EOF.
   *
   *  There are currently plans to add [uv_read] and [uv_try_read] to libuv
   *  itself. If these changes got merged, {!Stream.read} will wrap them -
   *  even if there will be small semantic differences.
   *)
  val read : ?pos:int -> ?len:int -> t -> buf:bytes -> int Lwt.t
  val read_ba : ?pos:int -> ?len:int -> t -> buf:buf -> int Lwt.t

  val write_queue_size : t -> int

  val try_write : ?pos:int -> ?len:int -> t -> buf:bytes -> Int_result.int
  val try_write_ba: ?pos:int -> ?len:int -> t -> buf:buf -> Int_result.int
  val try_write_string: ?pos:int -> ?len:int -> t -> buf:string -> Int_result.int

  val write : ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t
  val write_string : ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t
  val write_ba : ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  (** {!write} is always eager. It first calls try_write internally to
      check if it can return immediately (without the overhead of
      creating a sleeping thread and waking it up later). If it can't
      write everything instantly, it will call write_raw
      internally. {!write_raw} is exposed here mainly in order to write
      unit tests for it. But you can also use it, if you your [buf] is
      very large or you know for another reason, that try_write will
      fail.  *)
  val write_raw : ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t
  val write_raw_string : ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t
  val write_raw_ba : ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  val write2 : ?pos:int -> ?len:int -> buf:bytes -> send:t -> t -> unit Lwt.t

  val listen:
    t -> max:int -> cb:( t -> Int_result.unit -> unit ) -> Int_result.unit
  val listen_exn :
    t -> max:int -> cb:( t -> Int_result.unit -> unit ) -> unit

  val accept_raw: server:t -> client:t -> Int_result.unit
  val accept_raw_exn: server:t -> client:t -> unit

  val shutdown: t -> unit Lwt.t
end

module Pipe : sig
  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  val to_stream: t -> Stream.t

  (** The only thing that can go wrong, is memory allocation.
      In this case the ordinary exception [Out_of_memory] is thrown.
      The function is not called init_exn, because this exception can
      be thrown by nearly all functions. *)
  (** @param ipc is false by default *)
  val init : ?ipc:bool -> unit -> t

  (**
     Be careful with open* functions. They exists, so you can re-use
     system dependent libraries. But if you pass a file descriptor
     to openpipe (or opentcp,...), that is not really a file descriptor of a
     pipe (or tcp socket,...) you can trigger assert failures inside libuv.
     @param ipc is false by default
  *)
  val openpipe : ?ipc:bool -> file -> t result
  val openpipe_exn : ?ipc:bool -> file -> t

  val bind: t -> path:string -> Int_result.unit
  val bind_exn : t -> path:string -> unit

  val getsockname: t -> string result
  val getsockname_exn : t -> string

  val pending_instances: t -> int -> Int_result.unit
  val pending_instances_exn : t -> int -> unit

  val pending_count: t -> Int_result.int
  val pending_count_exn : t -> int

  type pending_type =
    | Unknown
    | Tcp
    | Udp
    | Pipe
  val pending_type: t -> pending_type

  val connect: t -> path:string -> unit Lwt.t
end

module Tcp : sig
  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  val to_stream : t -> Stream.t

  (** See comment to {!Pipe.init} *)
  val init : unit -> t

  type mode =
    | Ipv6_only

  (** See comment to {!Pipe.openpipe} *)
  val opentcp : socket -> t result
  val opentcp_exn : socket -> t

  (** @param mode: default is the empty list *)
  val bind : ?mode:mode list -> t -> addr:sockaddr -> unit -> Int_result.unit
  val bind_exn : ?mode:mode list -> t -> addr:sockaddr -> unit -> unit

  val nodelay : t -> bool -> Int_result.unit
  val nodelay_exn : t -> bool -> unit

  val keepalive : t -> bool -> Int_result.unit
  val keepalive_exn : t -> bool -> unit

  val simultaneous_accepts : t -> bool -> Int_result.unit
  val simultaneous_accepts_exn : t -> bool -> unit

  val getsockname : t -> sockaddr result
  val getsockname_exn : t -> sockaddr

  val getpeername : t -> sockaddr result
  val getpeername_exn : t -> sockaddr

  val connect : t -> addr:sockaddr -> unit Lwt.t

  (** initializes a new client and calls accept_raw with it *)
  val accept: t -> t result
  val accept_exn: t -> t
end

module Udp : sig
  type t
  include module type of Handle with type t := t
  include module type of Handle_ext with type t := t
  val to_handle : t -> Handle.t

  val send_queue_size: t -> int
  val send_queue_count: t -> int

  (** See comment to {!Pipe.init} *)
  val init : unit -> t

  (** See comment to {!Pipe.openpipe} *)
  val openudp : socket -> t result
  val openudp_exn : socket -> t

  type mode =
    | Ipv6_only
    | Reuse_addr

  (** @param mode default mode is the empty list *)
  val bind : ?mode:mode list -> t -> addr:sockaddr -> unit -> Int_result.unit
  val bind_exn : ?mode:mode list -> t -> addr:sockaddr -> unit -> unit

  val getsockname : t -> sockaddr result
  val getsockname_exn : t -> sockaddr

  type membership =
    | Leave_group
    | Join_group

  val set_membership :
    t -> multicast:string -> interface:string -> membership -> Int_result.unit
  val set_membership_exn :
    t -> multicast:string -> interface:string -> membership -> unit

  val set_multicast_loop : t -> bool -> Int_result.unit
  val set_multicast_loop_exn : t -> bool -> unit

  val set_multicast_ttl : t -> int -> Int_result.unit
  val set_multicast_ttl_exn : t -> int -> unit

  val set_multicast_interface : t -> string -> Int_result.unit
  val set_multicast_interface_exn : t -> string -> unit

  val set_broadcast : t -> bool -> Int_result.unit
  val set_broadcast_exn : t -> bool -> unit

  val set_ttl : t -> int -> Int_result.unit
  val set_ttl_exn : t -> int -> unit

  val send : ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> unit Lwt.t
  val send_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> unit Lwt.t
  val send_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> unit Lwt.t

  (** See comment to {!Stream.write_raw} *)
  val send_raw :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> unit Lwt.t
  val send_raw_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> unit Lwt.t
  val send_raw_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> unit Lwt.t

  val try_send :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> Int_result.int
  val try_send_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> Int_result.int
  val try_send_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> Int_result.int

  (** The type definition will likely be changed.
      Don't use fragile pattern matching for it *)
  type recv_result =
    | Data of Bytes.t * sockaddr option
    | Partial_data of Bytes.t * sockaddr option
    | Empty_from of sockaddr
    | Transmission_error of error

  val recv_start : t -> cb:(recv_result -> unit) -> Int_result.unit
  val recv_start_exn : t -> cb:(recv_result -> unit) -> unit

  val recv_stop : t -> Int_result.unit
  val recv_stop_exn : t -> unit

  type recv = {
    recv_len: int;
    is_partial: bool;
    sockaddr: sockaddr option;
  }
  (** Wrappers around {!recv_start} and {!recv_stop} for you convenience,
      no callback soup. *)
  val recv : ?pos:int -> ?len:int -> buf:bytes -> t -> recv Lwt.t
  val recv_ba : ?pos:int -> ?len:int -> buf:buf -> t -> recv Lwt.t

end

module Tty : sig
  type t
  include module type of Stream with type t := t

  val to_stream: t -> Stream.t
  val init : file -> read:bool -> t result
  val init_exn : file -> read:bool -> t

  type mode =
    | Normal
    | Raw
    | Io

  val set_mode : t -> mode:mode -> Int_result.unit
  val set_mode_exn : t -> mode:mode -> unit

  val reset_mode : unit -> Int_result.unit
  val reset_mode_exn : unit -> unit

  type winsize = {
    width: int;
    height: int;
  }
  val get_winsize : t -> winsize result
  val get_winsize_exn : t -> winsize
end

module Timer : sig
  type t
  include module type of Handle with type t := t
  val to_handle: t -> Handle.t

  val sleep : int -> unit Lwt.t

  (** Timers, that are executed only once (repeat=0), are automatically closed.
      After their callback have been executed, their handles are invalid.
      Call {!close} to stop a repeating timer *)
  val start : repeat:int -> timeout:int -> cb:(t -> unit) -> t result
  val start_exn : repeat:int -> timeout:int -> cb:(t -> unit) -> t
end

module Signal : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  (** use Sys.sigterm, Sys.sigstop, etc *)
  val start : int -> cb:(t -> int -> unit) -> t result
  val start_exn : int -> cb:(t -> int -> unit) -> t
end

module Poll : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type event =
    | Readable
    | Writable
    | Readable_writable

  (** start and start_exn don't support windows *)
  val start : file -> event -> cb:(t -> event result -> unit) -> t result
  val start_exn : file -> event -> cb:(t -> event result -> unit) -> t

  val start_socket : socket -> event -> cb:(t -> event result -> unit) -> t result
  val start_socket_exn : socket -> event -> cb:(t -> event result -> unit) -> t
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

  type cb = t -> (string * event list) result -> unit

  val start : string -> flags list -> cb:cb -> t result
  val start_exn : string -> flags list -> cb:cb -> t
end

module Fs_poll : sig
  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type report = {
    prev : Fs.stats;
    curr : Fs.stats;
  }

  val start : string -> int -> cb:(t -> report result -> unit) -> t result
  val start_exn : string -> int -> cb:(t -> report result -> unit) -> t
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
   *)
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
    t result

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

  val pid : t -> Int_result.int
  val pid_exn : t -> int

  val process_kill : t -> int -> Int_result.unit
  val process_kill_exn : t -> int -> unit

  val kill : pid:int -> signum:int -> Int_result.unit
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

  type handle_type =
    | File
    | Tty
    | Pipe
    | Tcp
    | Udp
    | Unknown

  val guess_handle: file -> handle_type

  val resident_set_memory : unit -> nativeint result
  val resident_set_memory_exn : unit -> nativeint

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
end

module Compat : sig
  (** be careful in case of [Unix.ADDR_UNIX path]. If path is very long,
      {!of_unix_sockaddr} will raise an exception ([Unix.Unix_error]) and
      {!to_unix_sockaddr} might return a truncated string.
      [Unix.ADDR_UNIX] is not supported on windows, {!of_unix_sockaddr} will
      also raise an exception in this case. *)
  val of_unix_sockaddr: Unix.sockaddr -> sockaddr
  val to_unix_sockaddr: sockaddr -> Unix.sockaddr

  (** the following functions always succeeds on *nix - but not on windows *)
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

(* Don't use it. It's currently only intended for Uwt_preemptive. *)
module Async : sig
  type t
  include module type of Handle with type t := t
  val to_handle: t -> Handle.t
  val create: ( t -> unit ) -> t result
  val start: t -> Int_result.unit
  val stop: t -> Int_result.unit
  val send: t -> Int_result.unit
end
