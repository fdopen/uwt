module Lwt_main = Uwt.Main
module Lwt_io = Uwt_io
module Lwt_log = Uwt_log
module Lwt_bytes = Uwt_bytes
module Lwt_process = Uwt_process
module Lwt_throttle = Uwt_throttle

(** everything inside Lwt_unix is implemented with funcions from Uwt.
    The sole purpose is to make it easier to test existing code with
    uwt.

    Unlike the modules above, functions from Lwt_unix always fail with
    [Unix.Unix_error], not [Uwt.Uwt_error].
*)
module Lwt_unix :
  sig
    type file_descr
    val to_file_descr:
      [`File of Uwt.file | `Pipe of Uwt.Pipe.t | `Tcp of Uwt.Tcp.t ] -> file_descr
    val from_file_descr:
      file_descr -> [`File of Uwt.file | `Pipe of Uwt.Pipe.t | `Tcp of Uwt.Tcp.t ]

    type process_status = Unix.process_status
    type wait_flag = Unix.wait_flag
    type file_perm = Unix.file_perm
    type open_flag = Unix.open_flag
    type seek_command = Unix.seek_command
    type file_kind = Unix.file_kind
    type stats = Unix.stats
    type access_permission = Unix.access_permission
    type inet_addr = Unix.inet_addr
    type socket_domain = Unix.socket_domain
    type socket_type = Unix.socket_type
    type sockaddr = Unix.sockaddr
    type shutdown_command = Unix.shutdown_command
    type msg_flag = Unix.msg_flag
    type socket_bool_option = Unix.socket_bool_option
    type socket_int_option = Unix.socket_int_option
    type socket_optint_option = Unix.socket_optint_option
    type socket_float_option = Unix.socket_float_option
    type host_entry = Unix.host_entry
    type protocol_entry = Unix.protocol_entry
    type service_entry = Unix.service_entry
    type addr_info = Unix.addr_info
    type getaddrinfo_option = Unix.getaddrinfo_option
    type name_info = Unix.name_info
    type terminal_io = Unix.terminal_io
    type setattr_when = Unix.setattr_when
    type flush_queue = Unix.flush_queue

    type dir_handle

    val sleep : float -> unit Lwt.t
    val yield : unit -> unit Lwt.t
    val auto_yield : float -> unit -> unit Lwt.t
    exception Timeout
    val timeout : float -> 'a Lwt.t
    val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t

    val stdin : file_descr
    val stdout : file_descr
    val stderr : file_descr

    val openfile : string -> Unix.open_flag list -> int -> file_descr Lwt.t
    val close : file_descr -> unit Lwt.t
    val read : file_descr -> bytes -> int -> int -> int Lwt.t
    val write : file_descr -> bytes -> int -> int -> int Lwt.t
    val write_string : file_descr -> string -> int -> int -> int Lwt.t
    val lseek : file_descr -> int -> Unix.seek_command -> int Lwt.t
    val truncate : string -> int -> unit Lwt.t
    val ftruncate : file_descr -> int -> unit Lwt.t
    val fsync : file_descr -> unit Lwt.t
    val fdatasync : file_descr -> unit Lwt.t
    val stat : string -> Unix.stats Lwt.t
    val lstat : string -> Unix.stats Lwt.t
    val fstat : file_descr -> Unix.stats Lwt.t
    val isatty : file_descr -> bool Lwt.t
    module LargeFile :
      sig
        type stats = Unix.LargeFile.stats
        val stat : string -> Unix.LargeFile.stats Lwt.t
        val lstat : string -> Unix.LargeFile.stats Lwt.t
        val fstat : file_descr -> Unix.LargeFile.stats Lwt.t
        val lseek : file_descr -> int64 -> Unix.seek_command -> int Lwt.t
        val truncate : string -> int64 -> unit Lwt.t
        val ftruncate : file_descr -> int64 -> unit Lwt.t
      end
    val unlink : string -> unit Lwt.t
    val rename : string -> string -> unit Lwt.t
    val link : string -> string -> unit Lwt.t
    val chmod : string -> int -> unit Lwt.t
    val fchmod : file_descr -> int -> unit Lwt.t
    val chown : string -> int -> int -> unit Lwt.t
    val fchown : file_descr -> int -> int -> unit Lwt.t
    val access : string -> Unix.access_permission list -> unit Lwt.t
    val mkdir : string -> int -> unit Lwt.t
    val rmdir : string -> unit Lwt.t
    val opendir : string -> dir_handle Lwt.t
    val readdir : dir_handle -> string Lwt.t
    val readdir_n : dir_handle -> int -> string array Lwt.t
    val closedir : dir_handle -> unit Lwt.t
    val files_of_directory : string -> string Lwt_stream.t
    val symlink : string -> string -> unit Lwt.t
    val readlink : string -> string Lwt.t
    val getlogin : unit -> string Lwt.t
    val getpwnam : string -> Unix.passwd_entry Lwt.t
    val getgrnam : string -> Unix.group_entry Lwt.t
    val getpwuid : int -> Unix.passwd_entry Lwt.t
    val getgrgid : int -> Unix.group_entry Lwt.t
    val chdir : string -> unit Lwt.t
    val gethostname : unit -> string Lwt.t
    val gethostbyname : string -> Unix.host_entry Lwt.t
    val gethostbyaddr : Unix.inet_addr -> Unix.host_entry Lwt.t
    val getprotobyname : string -> Unix.protocol_entry Lwt.t
    val getprotobynumber : int -> Unix.protocol_entry Lwt.t
    val getservbyname : string -> string -> Unix.service_entry Lwt.t
    val getservbyport : int -> string -> Unix.service_entry Lwt.t
    val getaddrinfo :
      string ->
      string -> Unix.getaddrinfo_option list -> Unix.addr_info list Lwt.t
    val getnameinfo:
      Unix.sockaddr -> Unix.getnameinfo_option list -> Unix.name_info Lwt.t
    val pipe: unit -> file_descr * file_descr
    val pipe_in : unit -> file_descr * Unix.file_descr
    val pipe_out : unit -> Unix.file_descr * file_descr
    val system : string -> Unix.process_status Lwt.t
  end
