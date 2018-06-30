(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

#include "error.ml"
#include "config.inc"

type 'a uv_result = ('a , error) result

type ('a , 'b) o_result = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type 'a result = ('a , error) o_result

external strerror: error -> string = "uwt_strerror"

module Int_result = struct
  module U = struct
    type error = Unix.error =
      | E2BIG
      | EACCES
      | EAGAIN
      | EBADF
      | EBUSY
      | ECHILD
      | EDEADLK
      | EDOM
      | EEXIST
      | EFAULT
      | EFBIG
      | EINTR
      | EINVAL
      | EIO
      | EISDIR
      | EMFILE
      | EMLINK
      | ENAMETOOLONG
      | ENFILE
      | ENODEV
      | ENOENT
      | ENOEXEC
      | ENOLCK
      | ENOMEM
      | ENOSPC
      | ENOSYS
      | ENOTDIR
      | ENOTEMPTY
      | ENOTTY
      | ENXIO
      | EPERM
      | EPIPE
      | ERANGE
      | EROFS
      | ESPIPE
      | ESRCH
      | EXDEV
      | EWOULDBLOCK
      | EINPROGRESS
      | EALREADY
      | ENOTSOCK
      | EDESTADDRREQ
      | EMSGSIZE
      | EPROTOTYPE
      | ENOPROTOOPT
      | EPROTONOSUPPORT
      | ESOCKTNOSUPPORT
      | EOPNOTSUPP
      | EPFNOSUPPORT
      | EAFNOSUPPORT
      | EADDRINUSE
      | EADDRNOTAVAIL
      | ENETDOWN
      | ENETUNREACH
      | ENETRESET
      | ECONNABORTED
      | ECONNRESET
      | ENOBUFS
      | EISCONN
      | ENOTCONN
      | ESHUTDOWN
      | ETOOMANYREFS
      | ETIMEDOUT
      | ECONNREFUSED
      | EHOSTDOWN
      | EHOSTUNREACH
      | ELOOP
      | EOVERFLOW
      | EUNKNOWNERR of int
  end

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

#include "error_val.ml"

  let to_unix_error = function
  | E2BIG -> U.E2BIG
  | EACCES -> U.EACCES
  | EADDRINUSE -> U.EADDRINUSE
  | EADDRNOTAVAIL -> U.EADDRNOTAVAIL
  | EAFNOSUPPORT -> U.EAFNOSUPPORT
  | EAGAIN -> U.EAGAIN
  | EALREADY -> U.EALREADY
  | EBADF -> U.EBADF
  | EBUSY -> U.EBUSY
  | ECONNABORTED -> U.ECONNABORTED
  | ECONNREFUSED -> U.ECONNREFUSED
  | ECONNRESET -> U.ECONNRESET
  | EDESTADDRREQ -> U.EDESTADDRREQ
  | EEXIST -> U.EEXIST
  | EFAULT -> U.EFAULT
  | EFBIG -> U.EFBIG
  | EHOSTUNREACH -> U.EHOSTUNREACH
  | EINTR -> U.EINTR
  | EINVAL -> U.EINVAL
  | EIO -> U.EIO
  | EISCONN -> U.EISCONN
  | EISDIR -> U.EISDIR
  | ELOOP -> U.ELOOP
  | EMFILE -> U.EMFILE
  | EMLINK -> U.EMLINK
  | EMSGSIZE -> U.EMSGSIZE
  | ENAMETOOLONG -> U.ENAMETOOLONG
  | ENETDOWN -> U.ENETDOWN
  | ENETUNREACH -> U.ENETUNREACH
  | ENFILE -> U.ENFILE
  | ENOBUFS -> U.ENOBUFS
  | ENODEV -> U.ENODEV
  | ENOENT -> U.ENOENT
  | ENOMEM -> U.ENOMEM
  | ENOPROTOOPT -> U.ENOPROTOOPT
  | ENOSPC -> U.ENOSPC
  | ENOSYS -> U.ENOSYS
  | ENOTCONN -> U.ENOTCONN
  | ENOTDIR -> U.ENOTDIR
  | ENOTEMPTY -> U.ENOTEMPTY
  | ENOTSOCK -> U.ENOTSOCK
  | ENXIO -> U.ENXIO
  | EPERM -> U.EPERM
  | EPIPE -> U.EPIPE
  | EPROTONOSUPPORT -> U.EPROTONOSUPPORT
  | EPROTOTYPE -> U.EPROTOTYPE
  | ERANGE -> U.ERANGE
  | EROFS -> U.EROFS
  | ESHUTDOWN -> U.ESHUTDOWN
  | ESPIPE -> U.ESPIPE
  | ESRCH -> U.ESRCH
  | ETIMEDOUT -> U.ETIMEDOUT
  | EXDEV -> U.EXDEV
  | ENOTSUP -> U.EOPNOTSUPP
  | (UWT_EFATAL|EAI_ADDRFAMILY|EAI_AGAIN|EAI_BADFLAGS|EAI_BADHINTS|
     EAI_CANCELED|EAI_FAIL|EAI_FAMILY|EAI_MEMORY|EAI_NODATA|EAI_NONAME|
     EAI_OVERFLOW|EAI_PROTOCOL|EAI_SERVICE|EAI_SOCKTYPE|ECANCELED|
     ECHARSET|ENONET|EPROTO|ETXTBSY|UNKNOWN|EOF)
    as x -> U.EUNKNOWNERR (error_to_int x)

  let of_unix_error = function
  | Unix.E2BIG -> E2BIG
  | Unix.EACCES -> EACCES
  | Unix.EADDRINUSE -> EADDRINUSE
  | Unix.EADDRNOTAVAIL -> EADDRNOTAVAIL
  | Unix.EAFNOSUPPORT -> EAFNOSUPPORT
  | Unix.EAGAIN -> EAGAIN
  | Unix.EALREADY -> EALREADY
  | Unix.EBADF -> EBADF
  | Unix.EBUSY -> EBUSY
  | Unix.ECONNABORTED -> ECONNABORTED
  | Unix.ECONNREFUSED -> ECONNREFUSED
  | Unix.ECONNRESET -> ECONNRESET
  | Unix.EDESTADDRREQ -> EDESTADDRREQ
  | Unix.EEXIST -> EEXIST
  | Unix.EFAULT -> EFAULT
  | Unix.EFBIG -> EFBIG
  | Unix.EHOSTUNREACH -> EHOSTUNREACH
  | Unix.EINTR -> EINTR
  | Unix.EINVAL -> EINVAL
  | Unix.EIO -> EIO
  | Unix.EISCONN -> EISCONN
  | Unix.EISDIR -> EISDIR
  | Unix.ELOOP -> ELOOP
  | Unix.EMFILE -> EMFILE
  | Unix.EMLINK -> EMLINK
  | Unix.EMSGSIZE -> EMSGSIZE
  | Unix.ENAMETOOLONG -> ENAMETOOLONG
  | Unix.ENETDOWN -> ENETDOWN
  | Unix.ENETUNREACH -> ENETUNREACH
  | Unix.ENFILE -> ENFILE
  | Unix.ENOBUFS -> ENOBUFS
  | Unix.ENODEV -> ENODEV
  | Unix.ENOENT -> ENOENT
  | Unix.ENOMEM -> ENOMEM
  | Unix.ENOPROTOOPT -> ENOPROTOOPT
  | Unix.ENOSPC -> ENOSPC
  | Unix.ENOSYS -> ENOSYS
  | Unix.ENOTCONN -> ENOTCONN
  | Unix.ENOTDIR -> ENOTDIR
  | Unix.ENOTEMPTY -> ENOTEMPTY
  | Unix.ENOTSOCK -> ENOTSOCK
  | Unix.ENXIO -> ENXIO
  | Unix.EPERM -> EPERM
  | Unix.EPIPE -> EPIPE
  | Unix.EPROTONOSUPPORT -> EPROTONOSUPPORT
  | Unix.EPROTOTYPE -> EPROTOTYPE
  | Unix.ERANGE -> ERANGE
  | Unix.EROFS -> EROFS
  | Unix.ESHUTDOWN -> ESHUTDOWN
  | Unix.ESPIPE -> ESPIPE
  | Unix.ESRCH -> ESRCH
  | Unix.ETIMEDOUT -> ETIMEDOUT
  | Unix.EXDEV -> EXDEV
  | Unix.EOPNOTSUPP -> ENOTSUP
  | Unix.ECHILD
  | Unix.EDEADLK
  | Unix.EDOM
  | Unix.ENOEXEC
  | Unix.ENOLCK
  | Unix.ENOTTY
  | Unix.EWOULDBLOCK
  | Unix.EINPROGRESS
  | Unix.ESOCKTNOSUPPORT
  | Unix.EPFNOSUPPORT
  | Unix.ENETRESET
  | Unix.ETOOMANYREFS
  | Unix.EHOSTDOWN
  | Unix.EOVERFLOW -> UNKNOWN
  | Unix.EUNKNOWNERR (i) ->
    match int_to_error i with
    | EINTR -> UNKNOWN
    | x -> x

  let to_error (x: 'a t) : error =
    if x >= 0 then
      invalid_arg "Uwt.Int_result.to_error";
    int_to_error x

  let h x = int_to_error x |> to_unix_error

  let raise_exn ?(name="") ?(param="") (x: 'a t) =
    if x >= 0 then
      invalid_arg "Uwt.Int_result.raise_exn"
    else
      raise (Unix.Unix_error(h x,name,param))

  let to_exn ?(name="") ?(param="") (x: 'a t) =
    if x >= 0 then
      Invalid_argument "Uwt.Int_result.to_exn"
    else
      Unix.Unix_error(h x,name,param)
end

let to_unix_error = Int_result.to_unix_error
let of_unix_error = Int_result.of_unix_error

type file = Unix.file_descr

type buf =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type sockaddr = Unix.sockaddr =
  | ADDR_UNIX of string
  | ADDR_INET of Unix.inet_addr * int

let stdin : file = Unix.stdin
let stdout : file = Unix.stdout
let stderr : file = Unix.stderr

module Iovec_write = struct
  type t =
    | Bigarray of buf * int * int
    | String of string * int * int
    | Bytes of bytes * int * int

  let length = function
  | Bigarray(_,_,l)
  | String(_,_,l)
  | Bytes(_,_,l) -> l

  let normalize ios =
    List.filter (fun a -> length a <> 0 ) ios

  let drop tl count =
    let rec loop count = function
    | [] -> []
    | hd::tl ->
      let l = length hd in
      if l < count then
        loop (count - l) tl
      else if l = count then
        normalize tl
      else
        let nhd = match hd with
        | Bigarray(b,o,l) -> Bigarray(b, o+count, l-count)
        | String(b,o,l) -> String(b, o+count, l-count)
        | Bytes(b,o,l) -> Bytes(b, o+count, l-count)
        in
        nhd::(normalize tl)
    in
    if count < 0 then
      invalid_arg "Iovec_write.drop"
    else if count = 0 then
      normalize tl
    else
      loop count tl

  type ts =
    | Invalid
    | Empty
    | All_ba of t array *  t list

  let is_invalid t =
    let dim,pos,len =
      match t with
      | Bigarray(b,o,l) -> Bigarray.Array1.dim b, o, l
      | String(b,o,l) -> String.length b, o, l
      | Bytes(b,o,l) -> Bytes.length b, o, l
    in
    pos < 0 || len < 0 || pos > dim - len

  let bas_only ios =
    let f = function | Bigarray _ -> true | Bytes _  | String _ -> false in
    List.filter f ios

  let prep_for_cstub li =
    if List.exists is_invalid li then Invalid else
    match normalize li with
    | [] -> Empty
    | l -> All_ba(Array.of_list l,bas_only l)
end

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
    | O_DIRECT
    | O_EXLOCK
    | O_NOATIME
    | O_SYMLINK
    | O_NOFOLLOW
    | O_DIRECTORY

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

  type clone_mode =
    | No_clone
    | Try_clone
    | Force_clone
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
  val pread : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:bytes ->
    int t
  val pread_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 -> buf:buf ->
    int t
  val write : ?pos:int -> ?len:int -> file -> buf:bytes -> int t
  val write_string : ?pos:int -> ?len:int -> file -> buf:string -> int t
  val write_ba : ?pos:int -> ?len:int -> file -> buf:buf -> int t
  val pwrite : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:bytes -> int t
  val pwrite_string : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:string -> int t
  val pwrite_ba : ?pos:int -> ?len:int -> file -> fd_offset:int64 ->
    buf:buf -> int t
  val writev : file -> Iovec_write.t list -> int t
  val pwritev : file -> Iovec_write.t list -> int64 -> int t
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
  val lchown : string -> uid:int -> gid:int -> unit t
  val scandir : string -> (file_kind * string) array t
  val realpath: string -> string t
  val copyfile : ?excl:bool -> ?clone:clone_mode -> src:string -> dst:string -> unit -> unit t
end

module Conv = struct

  external of_unix_sockaddr_exn : sockaddr -> sockaddr = "%identity"
  external to_unix_sockaddr_exn : sockaddr -> sockaddr = "%identity"

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
  | Error x -> raise (Unix.Unix_error(to_unix_error x,n,""))

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
    address: sockaddr option;
    netmask: sockaddr option;
  }

  type handle_type =
    | File
    | Tty
    | Pipe
    | Tcp
    | Udp
    | Unknown

  external guess_handle:
    Unix.file_descr -> handle_type = "uwt_guess_handle_na" NOALLOC

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

  external version_raw: unit -> int = "uwt_version_na" NOALLOC
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
    string array -> string -> Int_result.unit = "uwt_set_process_title_na" NOALLOC
  let set_process_title s = set_process_title Sys.argv s

  external getenv: string -> string uv_result = "uwt_os_getenv"
  external putenv: key:string -> data:string -> Int_result.unit =
    "uwt_os_setenv_na" NOALLOC
  external unsetenv: string -> Int_result.unit = "uwt_os_unsetenv_na" NOALLOC

  external getppid: unit -> Int_result.int = "uwt_os_getppid_na" NOALLOC
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
    | KFreebsd
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
  let win_version () = Error ENOSYS
#else
  external win_version: unit -> win_version uv_result = "uwt_win_version"
#endif
end
