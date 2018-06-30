(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(** Lwt independent core *)

(**
   Uwt_base contains type definitions and functions that are shared by
   {!Uwt} and {!Uv_fs_sync}
*)


type error =
  | E2BIG (** argument list too long *)
  | EACCES (** permission denied *)
  | EADDRINUSE (** address already in use *)
  | EADDRNOTAVAIL (** address not available *)
  | EAFNOSUPPORT (** address family not supported *)
  | EAGAIN (** resource temporarily unavailable *)
  | EAI_ADDRFAMILY (** address family not supported *)
  | EAI_AGAIN (** temporary failure *)
  | EAI_BADFLAGS (** bad ai_flags value *)
  | EAI_BADHINTS (** invalid value for hints *)
  | EAI_CANCELED (** request canceled *)
  | EAI_FAIL (** permanent failure *)
  | EAI_FAMILY (** ai_family not supported *)
  | EAI_MEMORY (** out of memory *)
  | EAI_NODATA (** no address *)
  | EAI_NONAME (** unknown node or service *)
  | EAI_OVERFLOW (** argument buffer overflow *)
  | EAI_PROTOCOL (** resolved protocol is unknown *)
  | EAI_SERVICE (** service not available for socket type *)
  | EAI_SOCKTYPE (** socket type not supported *)
  | EALREADY (** connection already in progress *)
  | EBADF (** bad file descriptor *)
  | EBUSY (** resource busy or locked *)
  | ECANCELED (** operation canceled *)
  | ECHARSET (** Windows filenames (and similar parameters) are
                 expected to be utf-8 encoded. [ECHARSET] is returned, if one
                 parameter contains invalid byte sequences.
                 Furthermore, [ECHARSET] will be returned, if your strings
                 contain null bytes (on *nix, too). *)
  | ECONNABORTED (** software caused connection abort *)
  | ECONNREFUSED (** connection refused *)
  | ECONNRESET (** connection reset by peer *)
  | EDESTADDRREQ (** destination address required *)
  | EEXIST (** file already exists *)
  | EFAULT (** bad address in system call argument *)
  | EFBIG (** file too large *)
  | EHOSTUNREACH (** host is unreachable *)
  | EINTR (** interrupted system call *)
  | EINVAL (** invalid argument *)
  | EIO (** i/o error *)
  | EISCONN (** socket is already connected *)
  | EISDIR (** illegal operation on a directory *)
  | ELOOP (** too many symbolic links encountered *)
  | EMFILE (** too many open files *)
  | EMSGSIZE (** message too long *)
  | ENAMETOOLONG (** name too long *)
  | ENETDOWN (** network is down *)
  | ENETUNREACH (** network is unreachable *)
  | ENFILE (** file table overflow *)
  | ENOBUFS (** no buffer space available *)
  | ENODEV (** no such device *)
  | ENOENT (** no such file or directory *)
  | ENOMEM (** not enough memory *)
  | ENONET (** machine is not on the network *)
  | ENOPROTOOPT (** protocol not available *)
  | ENOSPC (** no space left on device *)
  | ENOSYS (** function not implemented *)
  | ENOTCONN (** socket is not connected *)
  | ENOTDIR (** not a directory *)
  | ENOTEMPTY (** directory not empty *)
  | ENOTSOCK (** socket operation on non-socket *)
  | ENOTSUP (** operation not supported on socket *)
  | EPERM (** operation not permitted *)
  | EPIPE (** broken pipe *)
  | EPROTO (** protocol error *)
  | EPROTONOSUPPORT (** protocol not supported *)
  | EPROTOTYPE (** protocol wrong type for socket *)
  | ERANGE (** result too large *)
  | EROFS (** read-only file system *)
  | ESHUTDOWN (** cannot send after transport endpoint shutdown *)
  | ESPIPE (** invalid seek *)
  | ESRCH (** no such process *)
  | ETIMEDOUT (** connection timed out *)
  | ETXTBSY (** text file is busy *)
  | EXDEV (** cross-device link not permitted *)
  | UNKNOWN (** unknown error *)
  | EOF (** end of file *)
  | ENXIO (** no such device or address *)
  | EMLINK (** too many links *)
  | UWT_EFATAL (** something happened that the author of uwt didn't expect.
                   Probably a bug or the api of libuv has changed in the
                   meanwhile *)

val strerror: error -> string
(** error message for the given error code *)

val err_name: error -> string
(** error name for the given error code *)

val to_unix_error: error -> Unix.error
(** map error to [Unix.error] , [Unix.EUNKNOWNERR] is
    used, if the mapping is not possible *)

val of_unix_error : Unix.error -> error
(** get back the original Uwt.error from an exception
    raised by a Uwt function *)

type 'a uv_result = ('a , error) result

(**/**)
type ('a , 'b) o_result = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b [@@ocaml.deprecated "please use Result.result instead"]

[@@@ocaml.warning "-3"]

type 'a result = ('a , error) o_result
[@@ocaml.deprecated "please use Result.result or uv_result instead"]

[@@@ocaml.warning "+3"]

(**/**)

module Int_result : sig
  (** [Int_result.t] is used instead of ['a result], if a function
      returns either an error or a non-negative integer (including
      unit/bool). *)

  type 'a t = private int

  type real_int = int
  type real_unit = unit
  type int = real_int t
  type unit = real_unit t

  val is_ok : 'a t -> bool
  val is_error : 'a t -> bool

  val to_int : int -> real_int
  (** will raise [Invalid_argument], if {!is_ok} is false *)

  val to_error: 'a t -> error
  (** will raise [Invalid_argument], if {!is_error} is false *)

  val plain : 'a t -> real_int
  (** return a plain integer number, positive or negative *)

  val to_exn: ?name:string -> ?param:string -> 'a t -> exn
  (** will raise [Invalid_argument], if {!is_error} is false *)

  val raise_exn: ?name:string -> ?param:string -> 'a t -> 'b
  (** [Invalid_argument], if {!is_error} is false *)

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
      uwt release. *)

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
  val uwt_efatal : int
end

type file
(** Abstract type for a file descriptor.
    Unlike {!Unix.file_descr} it is not intended to wrap a SOCKET. *)

type sockaddr = Unix.sockaddr

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val stdin : file
(** File descriptor for standard input. *)

val stdout : file
(** File descriptor for standard output. *)

val stderr : file
(** File descriptor for standard error. *)

module Iovec_write : sig
  (**
     pass multiple buffers at once to a libuv write function.

     - first integer: offset in string / bytes / bigarray
     - second integer: number of bytes to write
  *)

  type t =
    | Bigarray of buf * int * int
    | String of string * int * int
    | Bytes of bytes * int * int

  val drop : t list -> int -> t list
  (** [drop tl n] adjusts the I/O vector list [tl] so that it no longer
      includes its first [n] bytes. *)

  (**/**)
  (* Internal functions. Don't use *)
  type ts =
    | Invalid (* at least one element has an invalid spec *)
    | Empty (* nothing to write / empty list or length is zero *)
    | All_ba of t array *  t list
    (* first element: all objects to write except the empty ones
       second element: all bigarray elements that need GC protection *)
  val prep_for_cstub : t list -> ts
  val length : t -> int
  (**/**)
end

module Fs_types : sig
  type uv_open_flag =
    | O_RDONLY (** Open for reading *)
    | O_WRONLY (** Open for writing *)
    | O_RDWR   (** Open for reading and writing *)
    | O_NONBLOCK (** Open in non-blocking mode, ignored on Windows *)
    | O_CREAT  (** Create if nonexistent *)
    | O_EXCL   (** Fail if existing *)
    | O_TRUNC  (** Truncate to 0 length if existing *)
    | O_APPEND (** Open for append *)
    | O_NOCTTY (** Don't make this dev a controlling tty, ignored on Windows *)
    | O_DSYNC  (** Writes complete as `Synchronised I/O data integrity
                   completion', ignored by some platforms *)
    | O_SYNC   (** Writes complete as `Synchronised I/O file integrity
                   completion', ignored by some platforms *)
    | O_RSYNC (** Reads complete as writes (depending on
                  O_SYNC/O_DSYNC), only supported on some Unix
                  platforms, ignored otherwise *)
    | O_TEMPORARY (** windows only, ignored on Unix *)
    | O_SHORT_LIVED (** windows only, ignored on Unix *)
    | O_SEQUENTIAL (** windows only, ignored on Unix *)
    | O_RANDOM (** windows only, ignored on Unix *)
    | O_DIRECT (** On Windows supported since libuv 1.16 *)
    | O_EXLOCK (** OS X (and Windows, but with different semantic) *)
    | O_NOATIME (** no windows, some Unix systems, ignored otherwise *)
    | O_SYMLINK  (** no windows, some Unix systems, ignored otherwise *)
    | O_NOFOLLOW (** no windows, some Unix systems, ignored otherwise *)
    | O_DIRECTORY (** no windows, some Unix systems, ignored otherwise *)

  (** Flags for {!Fs_functions.openfile}

      [O_CLOEXEC] doesn't exist, because this flag is unconditionally
      added by libuv. [O_SHARE_DELETE], [O_SHARE_WRITE], [O_SHARE_READ]
      are always added on Windows, unless [O_EXLOCK] is specified. *)

  type file_kind =
    | S_REG  (** Regular file *)
    | S_DIR  (** Directory *)
    | S_CHR  (** Character device *)
    | S_BLK  (** Block device *)
    | S_LNK  (** Symbolic link *)
    | S_FIFO (** Named pipe *)
    | S_SOCK (** Socket *)
    | S_UNKNOWN (** Everything else - possible on some platforms. *)

  type symlink_mode =
    | S_Default
    | S_Dir (** indicates that path points to a directory. *)
    | S_Junction (** request that the symlink is created using junction points.*)
  (**  On Windows it can be specified how to create symlinks. *)

  type access_permission =
    | Read  (** Read permission *)
    | Write (** Write permission *)
    | Exec  (** Execution permission *)
    | Exists (** File exists *)

  type stats = {
    st_dev: int;        (** Device number *)
    st_kind: file_kind; (** Kind of the file *)
    st_perm: int;       (** Access rights *)
    st_nlink: int;      (** Number of links *)
    st_uid: int;        (** User id of the owner *)
    st_gid: int;        (** Group ID of the file's group *)
    st_rdev: int;       (** Device minor number *)
    st_ino: int;        (** Inode number *)
    st_size: int64;     (** Size in bytes *)
    st_blksize: int;    (** "Preferred" block size for efficient filesystem I/O *)
    st_blocks: int;     (** Number of blocks allocated to the file, in 512-byte units *)
    st_flags: int;      (** User defined flags for file *)
    st_gen: int;        (** File generation number *)
    st_atime: int64;    (** Last access time *)
    st_atime_nsec: int; (** Nanosecond components of last access time *)
    st_mtime: int64;    (** Last modification time *)
    st_mtime_nsec: int; (** Nanosecond components of last modification time *)
    st_ctime: int64;    (** Last status change time *)
    st_ctime_nsec: int; (** Nanosecond components of lastt status change time *)
    st_birthtime: int64; (** File creation time *)
    st_birthtime_nsec: int; (** Nanosecond components of File creation time *)
  }
  (** File status information. Support for the various fields differs
      depending on the OS and filesystem. *)

  type clone_mode =
    | No_clone (** Create a normal copy *)
    | Try_clone (** Try to clone the file, but create a normal copy, if it fails *)
    | Force_clone (** Try tlone the file, don't create a normal copy, if it fails *)
  (** Clone mode for {!Fs_functions.copyfile} *)
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
  (** Equivalent to open(2). @param perm defaults are 0o644 *)

  val read : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  (** [read ~pos ~len fd ~buf] reads [~len] bytes from descriptor [fd],
      storing them in byte sequence [buf], starting at position [~pos] in
      [~buf]. Return the number of bytes actually read.

      @param pos default is always zero
      @param len default is always the still available length of the string /
      bigarray / Bytes.t *)

  val read_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  (** like {!read}, buf for bigarrays. Bigarrays are passed directly
      to libuv (no copy to c heap or stack). It's faster, but you must manually
      ensure, that the bigarray is not accessed from another thread. *)

  val pread : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:bytes ->
    int t
  (** {!pread} is equivalent to {!read}, except that it reads from a given
      position [~fd_offset] in the file without changing the file offset. *)

  val pread_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:buf ->
    int t

  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  (** [write fd ~pos ~len fd ~buf] writes [~len] bytes to descriptor [fd],
      taking them from byte sequence [buf], starting at position [~pos]
      in [~buf]. Return the number of bytes actually written. *)

  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t

  val pwrite : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:bytes -> int t
  (** {!pwrite} is equivalent to {!write}, except that it writes into
      a given position [~fd_offset] and does not change the file offset *)

  val pwrite_string : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:string -> int t
  val pwrite_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:buf -> int t

  val writev : file -> Iovec_write.t list -> int t
  (** write multiple buffers at once. If the number of buffers is
      greater than IOV_MAX, newer libuv versions already contains code
      to circumvent this issue *)

  val pwritev : file -> Iovec_write.t list -> int64 -> int t
  (** {!pwritev} is equivalent to {!pwrite}, except that it writes into
      a given position and does not change the file offset *)

  val close : file -> unit t
  (** Close a file descriptor. *)

  val unlink : string -> unit t
  (** delete a name and possibly the file it refers to *)

  val mkdir : ?perm:int -> string -> unit t
  (** Create a directory. @param perm defaults are 0o777 *)

  val rmdir : string -> unit t
  (** Delete an empty directory *)

  val fsync : file -> unit t
  (** synchronize a file's in-core state with storage device. *)

  val fdatasync : file -> unit t
  (** {!fdatasync} is similar to {!fsync}, but does not flush modified metadata
       unless that metadata is needed in order to allow a subsequent data
       retrieval to be correctly handled. *)

  val ftruncate: file -> len:int64 -> unit t
  (** truncate a file to a specified length *)

  val stat : string -> stats t
  (** Return the information for the named file. *)

  val lstat : string -> stats t
  (** {!lstat} is identical to {!stat}, except that if pathname is a symbolic
      link. In this case it returns information about the link itself. *)

  val fstat : file -> stats t
  (** {!fstat} is identical to {!stat}, except that the file about which
      information is to be retrieved is specified by the file descriptor *)

  val rename : src:string -> dst:string -> unit t
  (** change the name or location of a file *)

  val link : target:string -> link_name:string -> unit t
  (** {!link} creates a new link (also known as a hard link) to an existing
      file *)

  val symlink :
    ?mode:symlink_mode -> src:string -> dst:string -> unit -> unit t
  (** {!symlink} creates a symbolic link named [~dst] which contains
      the string [~src]

      @param mode default [S_Default] *)

  val mkdtemp : string -> string t
  (** The {!mkdtemp} function generates a uniquely named temporary directory
      from template. The last six characters of template must be XXXXXX and
      these are replaced with a string that makes the directory name unique *)

  val sendfile :
    ?pos:int64 -> ?len:nativeint -> dst:file -> src:file -> unit -> nativeint t
  (** A limited equivalent to [sendfile(2)]. It copies data between one file
      descriptor and another

      @param pos default 0
      @param len [Nativeint.max_int] *)

  val utime : string -> access:float -> modif:float -> unit t
  (** {!utime} changes the access and modification times of a file. If
      both times are [0.0], the access and last modification times are
      both set to the current time. *)

  val futime : file -> access:float -> modif:float -> unit t
  (** {!futime} is identical to {!utime}, except that the file about which
      information is to be retrieved is specified by the file descriptor *)

  val readlink : string -> string t
  (** {!readlink} reads the value of a symbolic link *)

  val access : string -> access_permission list -> unit t
  (** Check user's permissions for a file *)

  val chmod : string -> perm:int -> unit t
  (** Change the permissions of the named file. *)

  val fchmod : file -> perm:int -> unit t
  (** Change the permissions of an opened file. *)

  val chown : string -> uid:int -> gid:int -> unit t
  (** Change the owner [~uid] and owner [~gid] of the named file. *)

  val fchown : file -> uid:int -> gid:int -> unit t
  (** Change the owner [~uid] and owner [~gid] of the opened file. *)

  val lchown : string -> uid:int -> gid:int -> unit t
  (** like chown, but do not dereference symbolic links *)

  val scandir : string -> (file_kind * string) array t
  (** On Linux, getting the type of an entry is only supported by some
      filesystems (btrfs, ext2, ext3 and ext4 at the time of this
      writing), check the getdents(2) man page. *)

  val realpath: string -> string t
  (** Equivalent to realpath(3) on Unix. Windows uses
      GetFinalPathNameByHandle.

      Warning This function has certain platform specific caveats that
      were discovered when used in Node.

      macOS and other BSDs: this function will fail with UV_ELOOP if
      more than 32 symlinks are found while resolving the given
      path. This limit is hardcoded and cannot be sidestepped.

      Windows: while this function works in the common case, there are
      a number of corner cases where it doesn't:

      - Paths in ramdisk
      - volumes created by tools which sidestep the Volume Manager (such
        as ImDisk) cannot be resolved.
      - Inconsistent casing when using drive letters.
      - Resolved path bypasses subst'd drives.

      While this function can still be used, it's not recommended if scenarios
      such as the above need to be supported.
  *)

  val copyfile : ?excl:bool -> ?clone:clone_mode -> src:string -> dst:string -> unit -> unit t
  (** Copies a file from [~src] to [~dst].

      If [?excl] is true, copyfile will fail with [EEXIST] if the
      destination path already exists. The default behavior is to
      overwrite the destination if it exists.

      If [?clone] is [Try_clone], copyfile will attempt to create a
      copy-on-write reflink. If the underlying platform (or your installed
      libuv version) does not support copy-on-write, then a fallback copy
      mechanism is used.
      If [?clone] is [Force_clone], copyfile will attempt to create a
      copy-on-write reflink. If the underlying platform does not support
      copy-on-write, then an error is returned.
      The default behaviour is a normal copy ([No_clone]).

      Warning: If the destination path is created, but an error occurs
      while copying the data, then the destination path is
      removed. There is a brief window of time between closing and
      removing the file where another process could access the file. *)
end

module Conv : sig
  (**/**)
  val of_unix_sockaddr_exn :
    Unix.sockaddr -> sockaddr [@@ocaml.deprecated "useless now"]
  val to_unix_sockaddr_exn :
    sockaddr -> Unix.sockaddr [@@ocaml.deprecated "useless now"]
  (**/**)

  val file_of_file_descr : Unix.file_descr -> file option
  (** the function always succeed on Unix - but not on Windows.  It will
      fail, if you try to convert a `Unix.file_descr` that wraps a
      SOCKET or if you run out of crt file descriptors. *)

  val file_descr_of_file : file -> Unix.file_descr option
end

module Misc : sig
  (**
     This section contains miscellaneous functions that don't really
     belong in any other section. *)


  type timeval = {
    sec: int; usec: int;
  }

  type rusage = {
    utime: timeval; (** user CPU time used *)
    stime: timeval; (** system CPU time used *)
    maxrss: int64; (** maximum resident set size *)
    ixrss: int64; (** integral shared memory size (X) *)
    idrss: int64; (** integral unshared data size (X) *)
    isrss: int64; (** integral unshared stack size (X) *)
    minflt: int64; (** page reclaims (soft page faults) (X) *)
    majflt: int64; (** page faults (hard page faults) *)
    nswap: int64; (** swaps (X) *)
    inblock: int64; (** block input operations *)
    outblock: int64;  (** block output operations *)
    msgsnd: int64; (** IPC messages sent (X) *)
    msgrcv: int64; (** IPC messages received (X) *)
    nsignals: int64; (** signals received (X) *)
    nvcsw: int64; (** voluntary context switches (X) *)
    nivcsw: int64; (** involuntary context switches (X) *)
  }
  (** Data type for resource usage results. Members marked with (X)
      are unsupported on Windows. *)

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
  (** Data type for CPU information *)

  type interface_address = {
    name: string;
    phys_addr: string;
    is_internal: bool;
    address: sockaddr option;
    netmask: sockaddr option;
  }
  (** Data type for interface addresses. *)

  type handle_type =
    | File
    | Tty
    | Pipe
    | Tcp
    | Udp
    | Unknown

  val guess_handle: Unix.file_descr -> handle_type
  (** Used to detect what type of stream should be used with a given
      file descriptor. Usually this will be used during initialization
      to guess the type of the stdio streams.

      For isatty(3) equivalent functionality use this function and
      test for UV_TTY. *)

  val resident_set_memory : unit -> int64 uv_result
  (** Gets the resident set size (RSS) for the current process. *)

  val resident_set_memory_exn : unit -> int64

  val uptime : unit -> float uv_result
  (** Gets the current system uptime. *)

  val uptime_exn : unit -> float

  val getrusage : unit -> rusage uv_result
  (** Gets the resource usage measures for the current process. On
      Windows not all fields are set *)

  val getrusage_exn : unit -> rusage

  val cpu_info : unit -> cpu_info array uv_result
  (** Gets information about the CPUs on the system. *)

  val cpu_info_exn : unit -> cpu_info array

  val interface_addresses: unit -> interface_address array uv_result
  (** Gets address information about the network interfaces on the system. *)

  val interface_addresses_exn: unit -> interface_address array

  val load_avg: unit -> float * float * float
  (** Gets the load average. Returns [0,0,0] on Windows *)

  val ip4_addr: string -> int -> sockaddr uv_result
  (** Convert a string containing an IPv4 addresses to a binary structure. *)

  val ip4_addr_exn: string -> int -> sockaddr

  val ip4_name: sockaddr -> string uv_result
  (** Convert a binary structure containing an IPv4 address to a string. *)

  val ip4_name_exn: sockaddr -> string

  val ip6_addr: string -> int -> sockaddr uv_result
  (** Convert a string containing an IPv6 addresses to a binary structure. *)

  val ip6_addr_exn: string -> int -> sockaddr

  val ip6_name: sockaddr -> string uv_result
  (** Convert a binary structure containing an IPv6 address to a string. *)

  val ip6_name_exn: sockaddr -> string

  val get_total_memory: unit -> int64
  (** Gets memory information (in bytes). *)

  val hrtime: unit -> int64
  (**
     Returns the current high-resolution real time. This is expressed
     in nanoseconds. It is relative to an arbitrary time in the
     past. It is not related to the time of day and therefore not subject
     to clock drift. The primary use is for measuring performance
     between intervals.

     Note: Not every platform can support nanosecond resolution;
     however, this value will always be in nanoseconds. *)

  type version = {
    major: int;
    minor: int;
    patch: int;
  }

  val version: unit -> version
  (** libuv version used *)

  val version_raw: unit -> int
  val version_string: unit -> string

  (** Many of the functions below are not thread safe
      and might block.
      Functions like [cwd] are useful nevertheless,
      if you target windows. Unlike [Sys.getcwd()] they
      return UTF8-encoded names *)

  val os_homedir: unit -> string uv_result
  (** Gets the current user's home directory. On Windows, {!homedir}
      first checks the [USERPROFILE environment] variable using
      [GetEnvironmentVariableW()]. If [USERPROFILE] is not set,
      [GetUserProfileDirectoryW()] is called.

      On all other operating systems, {!os_homedir} first checks the
      [HOME] environment variable using [getenv(3)]. If [HOME] is not set,
      [getpwuid_r(3)] is called. *)

  val os_tmpdir: unit -> string uv_result
  (** Gets the temp directory. On Windows, uv_os_tmpdir() uses
      GetTempPathW(). On all other operating systems, uv_os_tmpdir()
      uses the first environment variable found in the ordered list
      TMPDIR, TMP, TEMP, and TEMPDIR. If none of these are found, the
      path "/tmp" is used, or, on Android, "/data/local/tmp" is
      used. *)

  val get_passwd: unit -> Unix.passwd_entry uv_result
  (** [pw_passwd] and [pw_gecos] will currently
      always contain an empty string.
      This function does work on Windows (unlike [Unix.getpwnam]
      or [Unix.getwuid] ) *)

  val exepath: unit -> string uv_result
  (** like Sys.executable_name , but utf-8 encoded under windows
      and perhaps more reliable under niche operating systems *)

  val cwd: unit -> string uv_result
  (** Gets the current working directory *)

  val chdir: string -> Int_result.unit
  (** Changes the current working directory. *)

  val getenv: string -> string uv_result
  (** Return the value associated to a variable in the process environment.
      ENOENT is returned, if the variable is unbound. *)

  val putenv: key:string -> data:string -> Int_result.unit
  (** [putenv ~key ~data] sets the value associated to a variable in
      the process environment. [key] is the name of the environment
      variable, and [data] its new associated value. *)

  val unsetenv: string -> Int_result.unit
  (** Deletes the environment variable specified by name. If no such
      environment variable exists, this function returns
      successfully. *)

  val getppid: unit -> Int_result.int
  (** Returns the parent process ID. Similar to [Unix.getppid], but also works
      under Windows *)

  (** The following two functions don't work reliable, especially with
      byte code. *)

  val set_process_title: string -> Int_result.unit
  (** Sets the process title. It won't necessary report an error, if it fails *)

  val get_process_title: unit -> string uv_result

end

module Sys_info : sig
  (**
     Information about your operating system.

     The module was introduced in order to skip certain unit tests
     on some platforms. But it might be useful for other purpose, too.
  *)

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
    | KFreebsd
    | Bsd
    | Unknown

  val os : os
  (** {!os} is determined at compile time (by the c compiler) and
      might differ from the later OS *)

  type win_version = {
    major_version: int;
    minor_version: int;
    build_number: int;
    platform_id: int;
    csd_version: string;
  }

  val win_version: unit -> win_version uv_result
  (** Wrapper around RtlGetVersion. It will always return [Error
      ENOSYS] on non windows systems. *)
end
