(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(** Lwt_unix compat module *)

module Lwt_main = Uwt.Main
module Lwt_bytes = Uwt_bytes
module Lwt_process = Uwt_process
module Lwt_throttle = Uwt_throttle

(** Lwt_unix compatibility layer

    Everything inside Lwt_unix is implemented with funcions from
    Uwt. The purpose is to make it easier to test or use existing
    code with uwt instead of lwt.unix *)
module Lwt_unix : sig
    type file_descr
    val to_file_descr:
      [`File of Uwt.file | `Pipe of Uwt.Pipe.t | `Tcp of Uwt.Tcp.t | `Udp of Uwt.Udp.t ] -> file_descr
    val from_file_descr:
      file_descr -> [`File of Uwt.file | `Pipe of Uwt.Pipe.t | `Tcp of Uwt.Tcp.t | `Udp of Uwt.Udp.t]
    type dir_handle

    (**/**)
    type process_status = Unix.process_status =
      | WEXITED of int
      | WSIGNALED of int
      | WSTOPPED of int
    type wait_flag = Unix.wait_flag =
      | WNOHANG
      | WUNTRACED
    type file_perm = Unix.file_perm
    type open_flag = Unix.open_flag =
      | O_RDONLY
      | O_WRONLY
      | O_RDWR
      | O_NONBLOCK
      | O_APPEND
      | O_CREAT
      | O_TRUNC
      | O_EXCL
      | O_NOCTTY
      | O_DSYNC
      | O_SYNC
      | O_RSYNC
      | O_SHARE_DELETE
      | O_CLOEXEC
#if OCAML_VERSION >= (4, 05, 0)
      | O_KEEPEXEC
#endif
    type seek_command = Unix.seek_command =
      | SEEK_SET
      | SEEK_CUR
      | SEEK_END
    type file_kind = Unix.file_kind
    type stats = Unix.stats =
      {
        st_dev : int;
        st_ino : int;
        st_kind : file_kind;
        st_perm : file_perm;
        st_nlink : int;
        st_uid : int;
        st_gid : int;
        st_rdev : int;
        st_size : int;
        st_atime : float;
        st_mtime : float;
        st_ctime : float;
      }
    type access_permission = Unix.access_permission =
      | R_OK
      | W_OK
      | X_OK
      | F_OK
    type inet_addr = Unix.inet_addr
    type socket_domain = Unix.socket_domain =
      | PF_UNIX
      | PF_INET
      | PF_INET6
    type socket_type = Unix.socket_type =
      | SOCK_STREAM
      | SOCK_DGRAM
      | SOCK_RAW
      | SOCK_SEQPACKET
    type sockaddr = Unix.sockaddr =
      | ADDR_UNIX of string
      | ADDR_INET of inet_addr * int

    type shutdown_command = Unix.shutdown_command =
      | SHUTDOWN_RECEIVE
      | SHUTDOWN_SEND
      | SHUTDOWN_ALL
    type msg_flag = Unix.msg_flag =
      | MSG_OOB
      | MSG_DONTROUTE
      | MSG_PEEK
    type socket_bool_option = Unix.socket_bool_option =
      | SO_DEBUG
      | SO_BROADCAST
      | SO_REUSEADDR
      | SO_KEEPALIVE
      | SO_DONTROUTE
      | SO_OOBINLINE
      | SO_ACCEPTCONN
      | TCP_NODELAY
      | IPV6_ONLY
    type socket_int_option = Unix.socket_int_option =
      | SO_SNDBUF
      | SO_RCVBUF
      | SO_ERROR
      | SO_TYPE
      | SO_RCVLOWAT
      | SO_SNDLOWAT
    type socket_optint_option = Unix.socket_optint_option =
      | SO_LINGER
    type socket_float_option = Unix.socket_float_option =
      | SO_RCVTIMEO
      | SO_SNDTIMEO
    type host_entry = Unix.host_entry =
      {
        h_name : string;
        h_aliases : string array;
        h_addrtype : socket_domain;
        h_addr_list : inet_addr array
      }
    type protocol_entry = Unix.protocol_entry =
      {
        p_name : string;
        p_aliases : string array;
        p_proto : int
      }
    type service_entry = Unix.service_entry =
      {
        s_name : string;
        s_aliases : string array;
        s_port : int;
        s_proto : string
      }
    type addr_info = Unix.addr_info =
      {
        ai_family : socket_domain;
        ai_socktype : socket_type;
        ai_protocol : int;
        ai_addr : sockaddr;
        ai_canonname : string;
      }
    type getaddrinfo_option = Unix.getaddrinfo_option =
      | AI_FAMILY of socket_domain
      | AI_SOCKTYPE of socket_type
      | AI_PROTOCOL of int
      | AI_NUMERICHOST
      | AI_CANONNAME
      | AI_PASSIVE
    type name_info = Unix.name_info =
    {
      ni_hostname : string;
      ni_service : string;
    }
    type getnameinfo_option = Unix.getnameinfo_option =
      | NI_NOFQDN
      | NI_NUMERICHOST
      | NI_NAMEREQD
      | NI_NUMERICSERV
      | NI_DGRAM
    type terminal_io = Unix.terminal_io =
      {
        mutable c_ignbrk : bool;
        mutable c_brkint : bool;
        mutable c_ignpar : bool;
        mutable c_parmrk : bool;
        mutable c_inpck : bool;
        mutable c_istrip : bool;
        mutable c_inlcr : bool;
        mutable c_igncr : bool;
        mutable c_icrnl : bool;
        mutable c_ixon : bool;
        mutable c_ixoff : bool;
        mutable c_opost : bool;
        mutable c_obaud : int;
        mutable c_ibaud : int;
        mutable c_csize : int;
        mutable c_cstopb : int;
        mutable c_cread : bool;
        mutable c_parenb : bool;
        mutable c_parodd : bool;
        mutable c_hupcl : bool;
        mutable c_clocal : bool;
        mutable c_isig : bool;
        mutable c_icanon : bool;
        mutable c_noflsh : bool;
        mutable c_echo : bool;
        mutable c_echoe : bool;
        mutable c_echok : bool;
        mutable c_echonl : bool;
        mutable c_vintr : char;
        mutable c_vquit : char;
        mutable c_verase : char;
        mutable c_vkill : char;
        mutable c_veof : char;
        mutable c_veol : char;
        mutable c_vmin : int;
        mutable c_vtime : int;
        mutable c_vstart : char;
        mutable c_vstop : char;
      }
    type setattr_when = Unix.setattr_when =
      | TCSANOW
      | TCSADRAIN
      | TCSAFLUSH
    type flush_queue = Unix.flush_queue =
      | TCIFLUSH
      | TCOFLUSH
      | TCIOFLUSH
      (**/**)

    val handle_unix_error : ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
    (** Same as [Unix.handle_unix_error] but catches lwt-level
        exceptions *)

    val sleep : float -> unit Lwt.t
    (** [sleep d] is a thread that remains suspended for [d] seconds
        and then terminates. *)

    val yield : unit -> unit Lwt.t
    (** [yield ()] is a thread that suspends itself and then resumes
        as soon as possible and terminates. *)

    val auto_yield : float -> unit -> unit Lwt.t
    (** [auto_yield timeout] returns a function [f] that will yield
        every [timeout] seconds. *)

    exception Timeout
    (** Exception raised by timeout operations *)

    val timeout : float -> 'a Lwt.t
    (** [timeout d] is a thread that remains suspended for [d] seconds
        and then fails with {!Timeout}. *)

    val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t
    (** [with_timeout d f] is a short-hand for:

        {[
          Lwt.pick [Lwt_unix.timeout d; f ()]
        ]}
    *)

    val stdin : file_descr
    (** The standard file descriptor for input *)

    val stdout : file_descr
    (** The standard file descriptor for output *)

    val stderr : file_descr
    (** The standard file descriptor for printing error messages *)

    val openfile : string -> Unix.open_flag list -> file_perm -> file_descr Lwt.t
    (** Wrapper for [Unix.openfile]. *)


    val close : file_descr -> unit Lwt.t
    (** Close a file descriptor. This close the underlying unix file
        descriptor *)

    val read : file_descr -> bytes -> int -> int -> int Lwt.t
    (** [read fd buff ofs len] reads [len] bytes from descriptor [fd],
        storing them in byte sequence [buff], starting at position [ofs] in
        [buff]. Return the number of bytes actually read. *)

    val write : file_descr -> bytes -> int -> int -> int Lwt.t
    (** [write fd buff ofs len] writes [len] bytes to descriptor [fd],
        taking them from byte sequence [buff], starting at position [ofs]
        in [buff]. Return the number of bytes actually written.  [write]
        repeats the writing operation until all bytes have been written or
        an error occurs (but only for streams) *)

    val write_string : file_descr -> string -> int -> int -> int Lwt.t
    (** See {!write}. *)

    val lseek : file_descr -> int -> Unix.seek_command -> int Lwt.t
    (** Set the current position for a file descriptor, and return the resulting
        offset (from the beginning of the file). *)

    val truncate : string -> int -> unit Lwt.t
    (** Truncates the named file to the given size. *)

    val ftruncate : file_descr -> int -> unit Lwt.t
    (** Truncates the file corresponding to the given descriptor to
       the given size. *)

    val fsync : file_descr -> unit Lwt.t
    (** Synchronise all data and metadata of the file descriptor with
        the disk. *)

    val fdatasync : file_descr -> unit Lwt.t
    (** Synchronise all data (but not metadata) of the file descriptor
        with the disk. *)

    val stat : string -> Unix.stats Lwt.t
    (** Return the information for the named file. *)

    val lstat : string -> Unix.stats Lwt.t
    (** Same as {!stat}, but in case the file is a symbolic link,
        return the information for the link itself. *)

    val fstat : file_descr -> Unix.stats Lwt.t
    (** Return the information for the file associated with the given
        descriptor. *)

    val isatty : file_descr -> bool Lwt.t
    (** Return [true] if the given file descriptor refers to a terminal or
        console window, [false] otherwise. *)

    val file_exists : string -> bool Lwt.t
    (** [file_exists name] tests if a file named [name] exists.

            Note that [file_exists] behaves similarly to
            {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Sys.html#VALfile_exists}
            [Sys.file_exists]}:

            - "file" is interpreted as "directory entry" in this context

            - [file_exists name] will return [false] in
              circumstances that would make {!stat} raise a
              {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#EXCEPTIONUnix_error}
              [Unix.Unix_error]} exception.
    *)

    module LargeFile : sig
      type stats = Unix.LargeFile.stats =
        {
          st_dev : int;
          st_ino : int;
          st_kind : file_kind;
          st_perm : file_perm;
          st_nlink : int;
          st_uid : int;
          st_gid : int;
          st_rdev : int;
          st_size : int64;
          st_atime : float;
          st_mtime : float;
          st_ctime : float;
        }
      val stat : string -> Unix.LargeFile.stats Lwt.t
      val lstat : string -> Unix.LargeFile.stats Lwt.t
      val fstat : file_descr -> Unix.LargeFile.stats Lwt.t
      val lseek : file_descr -> int64 -> Unix.seek_command -> int Lwt.t
      val truncate : string -> int64 -> unit Lwt.t
      val ftruncate : file_descr -> int64 -> unit Lwt.t
      val file_exists : string -> bool Lwt.t
    end

    val unlink : string -> unit Lwt.t
    (** Removes the named file. *)

    val rename : string -> string -> unit Lwt.t
    (** [rename old new] changes the name of a file from [old] to [new]. *)

    val link : string -> string -> unit Lwt.t
    (** [link source dest] creates a hard link named [dest] to the file
        named [source]. *)

    val chmod : string -> file_perm -> unit Lwt.t
    (** Change the permissions of the named file. *)

    val fchmod : file_descr -> file_perm -> unit Lwt.t
    (** Change the permissions of an opened file. *)

    val chown : string -> int -> int -> unit Lwt.t
    (** Change the owner uid and owner gid of the named file.
        On Windows: not implemented (make no sense on a DOS file system). *)

    val fchown : file_descr -> int -> int -> unit Lwt.t
    (** Change the owner uid and owner gid of an opened file.
        On Windows: not implemented (make no sense on a DOS file system). *)

    val access : string -> Unix.access_permission list -> unit Lwt.t
   (** Check that the process has the given permissions over the named file. *)

    val mkdir : string -> file_perm -> unit Lwt.t
    (** Create a directory with the given permissions. *)

    val rmdir : string -> unit Lwt.t
    (** Remove an empty directory. *)

    val opendir : string -> dir_handle Lwt.t
    (** This not really opens a descriptor on a
       directory. {!Uwt.Fs.scandir} is used internally *)

    val readdir : dir_handle -> string Lwt.t
    val readdir_n : dir_handle -> int -> string array Lwt.t
    val closedir : dir_handle -> unit Lwt.t

    val files_of_directory : string -> string Lwt_stream.t
    (** [files_of_directory dir] returns the stream of all files of
        [dir]. *)

    val symlink : string -> string -> unit Lwt.t
    (** [symlink source dest] creates the file [dest] as a symbolic link
        to the file [source]. *)

    val readlink : string -> string Lwt.t
    (** Read the contents of a symbolic link. *)

    val getlogin : unit -> string Lwt.t
    (** Return the login name of the user executing the process. *)

    val getpwnam : string -> Unix.passwd_entry Lwt.t
    (** Find an entry in [passwd] with the given name. *)

    val getgrnam : string -> Unix.group_entry Lwt.t
    (** Find an entry in [group] with the given name. *)

    val getpwuid : int -> Unix.passwd_entry Lwt.t
    (** Find an entry in [passwd] with the given user id.*)

    val getgrgid : int -> Unix.group_entry Lwt.t
    (** Find an entry in [group] with the given group id.*)

    val chdir : string -> unit Lwt.t
    (** Change the process working directory. *)

    val chroot : string -> unit Lwt.t
    (** Wrapper for [Unix.chroot] *)

    val getcwd : unit -> string Lwt.t
    (** Wrapper for [Unix.getcwd] *)

    val gethostname : unit -> string Lwt.t
    (** Return the name of the local host. *)

    val gethostbyname : string -> Unix.host_entry Lwt.t
    (** Find an entry in [hosts] with the given name.*)

    val gethostbyaddr : Unix.inet_addr -> Unix.host_entry Lwt.t
    (** Find an entry in [hosts] with the given address.*)

    val getprotobyname : string -> Unix.protocol_entry Lwt.t
    (** Find an entry in [services] with the given name.*)

    val getprotobynumber : int -> Unix.protocol_entry Lwt.t
    (** Find an entry in [protocols] with the given protocol number.*)

    val getservbyname : string -> string -> Unix.service_entry Lwt.t
    (** Find an entry in [services] with the given name.*)

    val getservbyport : int -> string -> Unix.service_entry Lwt.t
    (** Find an entry in [services] with the given service number.*)

    val getaddrinfo :
      string ->
      string -> Unix.getaddrinfo_option list -> Unix.addr_info list Lwt.t
    (** [getaddrinfo host service opts] returns a list of {!Unix.addr_info}
        records describing socket parameters and addresses suitable for
        communicating with the given host and service.  The empty list is
        returned if the host or service names are unknown, or the constraints
        expressed in [opts] cannot be satisfied. *)

    val getnameinfo:
      Unix.sockaddr -> Unix.getnameinfo_option list -> Unix.name_info Lwt.t
    (** [getnameinfo addr opts] returns the host name and service name
        corresponding to the socket address [addr].  [opts] is a possibly
        empty list of options that governs how these names are obtained.
        Lwt.fails with Not_found if an error occurs. *)

    val pipe: unit -> file_descr * file_descr
    (** [pipe ()] creates pipe using [Unix.pipe] and returns two file
        descriptors created from unix file_descriptor *)

    val pipe_in : unit -> file_descr * Unix.file_descr
    (** [pipe_in ()] is the same as {!pipe} but maps only the unix file
        descriptor for reading into a lwt one. The second is not
        put into non-blocking mode. You usually want to use this before
        forking to receive data from the child process. *)

    val pipe_out : unit -> Unix.file_descr * file_descr
    (** [pipe_out ()] is the inverse of {!pipe_in}. You usually want to
        use this before forking to send data to the child process *)

    val system : string -> Unix.process_status Lwt.t
    (** Executes the given command, waits until it terminates, and
        return its termination status. The string is interpreted by the
        shell [/bin/sh] on Unix and [cmd.exe] on Windows. The result
        [WEXITED 127] indicates that the shell couldn't be executed. *)

    val utimes : string -> float -> float -> unit Lwt.t
    (** [utimes path atime mtime] updates the access and modification
       times of the file at [path]. The access time is set to [atime]
       and the modification time to [mtime]. To set both to the
       current time, call [utimes path 0. 0.]. *)

    type signal_handler_id

    val on_signal : int -> (int -> unit) -> signal_handler_id
    (** [on_signal signum f] calls [f] each time the signal with numnber
        [signum] is received by the process. It returns a signal handler
        identifier that can be used to stop monitoring [signum]. *)

    val on_signal_full : int -> (signal_handler_id -> int -> unit) -> signal_handler_id
    (** [on_signal_full f] is the same as [on_signal f] except that [f]
        also receive the signal handler identifier as argument so it can
        disable it. *)

    val disable_signal_handler : signal_handler_id -> unit
    (** Stops receiving this signal *)

  end

module Lwt_io : sig
  include module type of Uwt_io

  val open_file :
    ?buffer : Lwt_bytes.t ->
    ?flags : Unix.open_flag list ->
    ?perm : Unix.file_perm ->
    mode : 'a mode ->
    file_name -> 'a channel Lwt.t

  val with_file :
    ?buffer : Lwt_bytes.t ->
    ?flags : Unix.open_flag list ->
    ?perm : Unix.file_perm ->
    mode : 'a mode ->
    file_name -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t

  val open_temp_file :
    ?buffer:Lwt_bytes.t ->
    ?flags:Unix.open_flag list ->
    ?perm:Unix.file_perm ->
    ?temp_dir:string ->
    ?prefix:string ->
    unit ->
    (string * output_channel) Lwt.t

  val with_temp_file :
    ?buffer:Lwt_bytes.t ->
    ?flags:Unix.open_flag list ->
    ?perm:Unix.file_perm ->
    ?temp_dir:string ->
    ?prefix:string ->
    (string * output_channel -> 'b Lwt.t) ->
     'b Lwt.t
end
