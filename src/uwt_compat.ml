(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

module Lwt_main = Uwt.Main
module Lwt_bytes = Uwt_bytes
module Lwt_process = Uwt_process
module Lwt_throttle = Uwt_throttle

module Lwt_unix = struct

  module UU = Uwt.Unix
  module UF = Uwt.Fs
  module U = Unix

  open Lwt.Infix

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

  let handle_unix_error f x =
    Lwt.catch
      (fun () -> f x)
      (fun exn -> Unix.handle_unix_error (fun () -> raise exn) ())

  let sleep = UU.sleep

  let yield = Uwt.Main.yield
  let auto_yield timeout =
    let limit = ref (U.gettimeofday () +. timeout) in
    fun () ->
      let current = U.gettimeofday () in
      if current >= !limit then begin
        limit := current +. timeout;
        yield ();
      end else
        Lwt.return_unit

  exception Timeout
  let timeout d = sleep d >>= fun () -> Lwt.fail Timeout
  let with_timeout d f = Lwt.pick [timeout d; Lwt.apply f ()]

  type file_descr =
    | File of Uwt.file
    | Tcp of Uwt.Tcp.t
    | Pipe of Uwt.Pipe.t
    | Udp of Uwt.Udp.t

  let from_file_descr = function
  | File x -> `File x
  | Tcp x -> `Tcp x
  | Pipe x -> `Pipe x
  | Udp x  -> `Udp x

  let to_file_descr = function
  | `File x -> File x
  | `Tcp x -> Tcp x
  | `Pipe x -> Pipe x
  | `Udp  x -> Udp x

  type f_common =
    | Fd of Uwt.file
    | Stream of Uwt.Stream.t
    | U of Uwt.Udp.t

  let stdin = File Uwt.stdin
  let stdout = File Uwt.stdout
  let stderr = File Uwt.stderr

  let trans_exn = function
  | U.Unix_error(U.EUNKNOWNERR(i),b,c) when
      i = (Uwt_base.Int_result.echarset :> int) ->
    Lwt.fail (U.Unix_error(U.ENOENT,b,c))
  | x -> Lwt.fail x

  let trans_of accu = function
  | U.O_RDONLY -> Uwt.Fs_types.O_RDONLY :: accu
  | U.O_WRONLY -> Uwt.Fs_types.O_WRONLY :: accu
  | U.O_RDWR  -> Uwt.Fs_types.O_RDWR :: accu
  | U.O_NONBLOCK -> Uwt.Fs_types.O_NONBLOCK :: accu
  | U.O_APPEND -> Uwt.Fs_types.O_APPEND :: accu
  | U.O_CREAT -> Uwt.Fs_types.O_CREAT :: accu
  | U.O_TRUNC -> Uwt.Fs_types.O_TRUNC :: accu
  | U.O_EXCL -> Uwt.Fs_types.O_EXCL :: accu
  | U.O_NOCTTY -> Uwt.Fs_types.O_NOCTTY :: accu
  | U.O_DSYNC -> Uwt.Fs_types.O_DSYNC :: accu
  | U.O_SYNC -> Uwt.Fs_types.O_SYNC :: accu
  | U.O_RSYNC -> Uwt.Fs_types.O_RSYNC :: accu
#if OCAML_VERSION >= (4, 05, 0)
  | U.O_KEEPEXEC
#endif
  | U.O_SHARE_DELETE
  | U.O_CLOEXEC -> accu

  let ufail ?(param="") ecode s1 =
    Lwt.fail (U.Unix_error(ecode,s1,param))

  let help f =
    Lwt.catch f trans_exn

  let help1 f s1 =
    Lwt.catch
      ( fun () -> f s1 )
      trans_exn

  let help2 f s1 s2 =
    Lwt.catch
      ( fun () -> f s1 s2 )
      trans_exn

  let openfile path (flags:open_flag list) (perm:file_perm) =
    let f () =
      let mode = List.fold_left trans_of [] flags in
      UF.openfile ~mode ~perm path >>= fun fd ->
#if OCAML_VERSION >= (4, 05, 0)
      (* contrary to the standard runtime, cloexec is the default behaviour *)
      (if List.mem U.O_CLOEXEC flags || List.mem U.O_KEEPEXEC flags = false then
         Lwt.return_unit
       else
       match Uwt.Conv.file_descr_of_file fd with
       | None ->
         let _t : unit Lwt.t = UF.close fd in
         ufail ~param:path Unix.EBADF "open"
       | Some ufd ->
         match Unix.clear_close_on_exec ufd with
         | exception x ->
           let _t : unit Lwt.t = UF.close fd in
           Lwt.fail x
         | () -> Lwt.return_unit
      ) >>= fun () ->
#endif
      Lwt.return (File fd)
    in
    help f

  let to_com = function
  | File fd -> Fd fd
  | Pipe fd -> Stream (Uwt.Pipe.to_stream fd)
  | Tcp fd -> Stream (Uwt.Tcp.to_stream fd)
  | Udp fd -> U fd

  let close fd =
    let it s =
      if Uwt.Int_result.is_ok s then
        Lwt.return_unit
      else
        let ec = Uwt.to_unix_error @@ Uwt_base.Int_result.to_error s in
        ufail ec "close"
    in
    let fd = to_com fd in
    match fd with
    | Fd fd -> UF.close fd
    | Stream fd -> Uwt.Stream.close fd |> it
    | U fd -> Uwt.Udp.close fd |> it

  let read fd buf ofs len =
    match to_com fd with
    | Fd fd -> UF.read ~pos:ofs ~len:len ~buf fd
    | Stream fd -> Uwt.Stream.read ~pos:ofs ~len:len ~buf fd
    | U fd -> Uwt.Udp.recv ~pos:ofs ~len:len ~buf fd >>= fun x ->
      Lwt.return x.Uwt.Udp.recv_len

  let write fd buf ofs len =
    match to_com fd with
    | Fd fd -> UF.write ~pos:ofs ~len:len ~buf fd
    | Stream fd ->
      Uwt.Stream.write ~pos:ofs ~len:len ~buf fd >>= fun () ->
      Lwt.return len
    | U _ -> Lwt.fail (Unix.Unix_error(Unix.EDESTADDRREQ,"write",""))

  let write_string fd buf ofs len =
    match to_com fd with
    | Fd fd -> UF.write_string ~pos:ofs ~len:len ~buf fd
    | Stream fd ->
      Uwt.Stream.write_string ~pos:ofs ~len:len ~buf fd >>= fun () ->
      Lwt.return len
    | U _ -> Lwt.fail (Unix.Unix_error(Unix.EDESTADDRREQ,"write",""))

  let lseek fd n com =
    match fd with
    | Udp _ | Pipe _  | Tcp _ -> ufail U.ESPIPE "lseek"
    | File fd ->
      UU.lseek fd (Int64.of_int n) com >>= fun l ->
      Lwt.return (Int64.to_int l)

  let truncate path i =
    let f () =
      UF.openfile ~mode:[UF.O_WRONLY] path >>= fun fd ->
      Lwt.finalize ( fun () -> UF.ftruncate fd ~len:(Int64.of_int i) )
        ( fun () -> UF.close fd )
    in
    help f

  let ftruncate fd i =
    match fd with
    | Udp _ | Pipe _  | Tcp _ -> ufail U.EINVAL "ftruncate"
    | File fd ->
      UF.ftruncate fd ~len:(Int64.of_int i)

  let fsync fd =
    match fd with
    | Udp _ | Pipe _ | Tcp _ -> ufail U.EINVAL "fsync"
    | File fd -> UF.fsync fd

  let fdatasync fd =
    match fd with
    | Udp _ | Pipe _ | Tcp _ -> ufail U.EINVAL "fdatasync"
    | File fd -> UF.fdatasync fd

  let file_kind = function
  | UF.S_UNKNOWN (* see stat.c *)
  | UF.S_REG -> U.S_REG
  | UF.S_DIR -> U.S_DIR
  | UF.S_CHR -> U.S_CHR
  | UF.S_BLK -> U.S_BLK
  | UF.S_LNK -> U.S_LNK
  | UF.S_FIFO -> U.S_FIFO
  | UF.S_SOCK -> U.S_SOCK

  let stat_convert x =
    Lwt.return {
      st_dev = x.UF.st_dev;
      st_ino = x.UF.st_ino;
      st_kind = file_kind x.UF.st_kind;
      st_perm = x.UF.st_perm;
      st_nlink = x.UF.st_nlink;
      st_uid = x.UF.st_uid;
      st_gid = x.UF.st_gid;
      st_rdev = x.UF.st_rdev;
      st_size = Int64.to_int x.UF.st_size;
      st_atime = Int64.to_float x.UF.st_atime;
      st_mtime = Int64.to_float x.UF.st_mtime;
      st_ctime = Int64.to_float x.UF.st_ctime;
    }

  let stat f =
    help1 UF.stat f >>= stat_convert

  let lstat f =
    help1 UF.lstat f >>= stat_convert

  let fstat fd =
    match fd with
    | Udp _ | Pipe _ | Tcp _ -> ufail U.EINVAL "fsync"
    | File fd -> UF.fstat fd >>= stat_convert

  let isatty x =
    (match x with
    | File fd -> Uwt.Conv.file_descr_of_file fd |> Lwt.return
    | Udp _  | Tcp _ -> Lwt.return_none
    | Pipe fd ->
      match Uwt.Pipe.fileno fd with
      | Error _ -> Lwt.return_none
      | Ok fd -> Lwt.return (Some fd))
    >>= function
    | None -> Lwt.return_false
    | Some fd ->
      if Uwt.Misc.guess_handle fd = Uwt.Misc.Tty then
        Lwt.return_true
      else
        Lwt.return_false

  let file_exists name =
    Lwt.try_bind
      (fun () -> Uwt.Fs.stat name)
      (fun _ -> Lwt.return_true)
      (function
      | Unix.Unix_error _ -> Lwt.return_false
      | x -> trans_exn x)

  module LargeFile = struct

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

    let stat_convert x =
      Lwt.return {
        Unix.LargeFile.st_dev = x.UF.st_dev;
        st_ino = x.UF.st_ino;
        st_kind = file_kind x.UF.st_kind;
        st_perm = x.UF.st_perm;
        st_nlink = x.UF.st_nlink;
        st_uid = x.UF.st_uid;
        st_gid = x.UF.st_gid;
        st_rdev = x.UF.st_rdev;
        st_size = x.UF.st_size;
        st_atime = Int64.to_float x.UF.st_atime;
        st_mtime = Int64.to_float x.UF.st_mtime;
        st_ctime = Int64.to_float x.UF.st_ctime;
      }

    let stat f =
      help1 UF.stat f >>= stat_convert

    let lstat f =
      help1 UF.lstat f >>= stat_convert

    let fstat fd =
      match fd with
      | Pipe _  | Tcp _ | Udp _ -> ufail U.EINVAL "fstat"
      | File fd -> UF.fstat fd >>= stat_convert

    let lseek fd n com =
      match fd with
      | Pipe _  | Tcp _ | Udp _ -> ufail U.ESPIPE "lseek"
      | File fd ->
        UU.lseek fd n com >>= fun l ->
        Lwt.return (Int64.to_int l)

    let truncate path i =
      let f () =
        UF.openfile ~mode:[UF.O_WRONLY] path >>= fun fd ->
        Lwt.finalize ( fun () -> UF.ftruncate fd ~len:i )
          ( fun () -> UF.close fd )
      in
      help f

    let ftruncate fd i =
      match fd with
      | Pipe _ | Tcp _ | Udp _ -> ufail U.EINVAL "ftruncate"
      | File fd -> UF.ftruncate fd ~len:i

    let file_exists = file_exists
  end

  let unlink s = help1 UF.unlink s

  let rename src dst =
    help ( fun () -> UF.rename ~src ~dst )

  let link target link_name =
    help ( fun () -> UF.link ~target ~link_name )

  let chmod f perm =
    help ( fun () -> UF.chmod f ~perm )

  let fchmod fd perm =
    match fd with
    | Udp _ | Pipe _  | Tcp _ -> ufail U.EBADF "fchmod"
    | File fd -> UF.fchmod fd ~perm

  let chown f uid gid =
    help ( fun () -> UF.chown f ~uid ~gid )

  let fchown fd uid gid =
    match fd with
    | Udp _ | Pipe _ | Tcp _ -> ufail U.EBADF "fchown"
    | File fd -> UF.fchown fd ~uid ~gid

  let access_map = function
  | U.R_OK -> UF.Read
  | U.W_OK -> UF.Write
  | U.X_OK -> UF.Exec
  | U.F_OK -> UF.Exists

  let access s pl =
    let pl = List.map access_map pl in
    help2 UF.access s pl

  let mkdir path perm =
    help ( fun () -> UF.mkdir ~perm path )

  let rmdir path =
    help1 UF.rmdir path

  type dir_handle = {
    mutable d_closed: bool;
    mutable d_pos: int;
    d_len: int;
    d_files: string array }

  let opendir path =
    Lwt.catch (fun () ->
        UF.scandir path >>= fun a ->
        Lwt.return {
          d_closed = false;
          d_pos = 0;
          d_len = Array.length a;
          d_files = Array.map snd a })
      (function
      | Unix.Unix_error(a,b,c) as exn ->
        if b = "scandir" then
          trans_exn (Unix.Unix_error(a,"opendir",c))
        else
          trans_exn exn
      | x -> Lwt.fail x)

  let readdir h =
    if h.d_closed = true then
      ufail U.EBADF "readdir"
    else if h.d_pos >= h.d_len then
      Lwt.fail End_of_file
    else
      let s = h.d_files.(h.d_pos) in
      h.d_pos <- h.d_pos + 1;
      Lwt.return s

  let readdir_n h n =
    if h.d_closed = true then
      ufail U.EBADF "readdir"
    else if h.d_pos >= h.d_len then
      Lwt.fail End_of_file
    else
      let n = max n (h.d_len - h.d_pos) in
      let ret = Array.sub h.d_files h.d_pos n in
      h.d_pos <- h.d_pos + n;
      Lwt.return ret

  let closedir h =
    if h.d_closed = true then
      ufail U.EBADF "readdir"
    else
      let () = h.d_closed <- true in
      Lwt.return_unit

  let files_of_directory path =
    let read = ref false in
    Lwt_stream.concat (
      (Lwt_stream.from
         (fun () ->
            match !read with
            | true -> Lwt.return_none
            | false ->
              help1 UF.scandir path >>= fun ar ->
              read:= true;
              let ret =
                Array.map snd ar |>
                Lwt_stream.of_array
              in
              Lwt.return (Some ret ))))

  let symlink src dst =
    help ( fun () -> UF.symlink ~src ~dst () )

  let readlink s = help1 UF.readlink s

  let chdir = help1 UU.chdir

  let getlogin () =
    Lwt.catch
      (fun () -> UU.getlogin ())
      (function
      | Unix.Unix_error _ -> Lwt.fail (Unix.Unix_error(Unix.ENOENT,"getlogin",""))
      | x -> Lwt.fail x)

  let helpn n t =
    Lwt.catch
      t
      (function
      | Unix.Unix_error(_,s,_) when s = n -> Lwt.fail Not_found
      | x -> Lwt.fail x)

  let help1n n f s1 =
    helpn n ( fun () -> f s1 )

  let help2n n f s1 s2 =
    helpn n ( fun () -> f s1 s2 )

  let getpwnam = help1n "getpwnam" UU.getpwnam
  let getgrnam = help1n "getgrnam" UU.getgrnam
  let getpwuid = help1n "getpwuid" UU.getpwuid
  let getgrgid = help1n "getgrgid" UU.getgrgid
  let gethostname = UU.gethostname
  let gethostbyname = help1n "gethostbyname" UU.gethostbyname
  let gethostbyaddr = help1n "gethostbyaddr" UU.gethostbyaddr
  let getprotobyname = help1n "getprotobyname" UU.getprotobyname
  let getprotobynumber = help1n "getprotobynumber" UU.getprotobynumber
  let getservbyname name protocol =
    helpn "getservbyname" (fun () -> UU.getservbyname ~name ~protocol )
  let getservbyport = help2n "getservbyport" UU.getservbyport

  let getaddrinfo host service (options:Unix.getaddrinfo_option list) =
    Uwt.Dns.getaddrinfo ~host ~service options >>=function
    | Error _ -> Lwt.return_nil (* errors are ignored inside getaddrinfo.c *)
    | Ok l -> Lwt.return l

  let getnameinfo addr l =
    Uwt.Dns.getnameinfo addr l >>= function
    | Error _ -> Lwt.fail Not_found
    | Ok x -> Lwt.return x

  let pipe () =
    let fd1,fd2 = UU.pipe_exn () in
    Pipe fd1, Pipe fd2

  external epipe:
    bool -> (U.file_descr * U.file_descr) Uwt.uv_result = "uwt_pipe"

  let pclose x =
    try U.close x with U.Unix_error _ -> ()

  let pipe_in () =
    match epipe true with
    | Error x -> raise (U.Unix_error(Uwt.to_unix_error x,"pipe",""))
    | Ok (out_fd, in_fd) ->
      match Uwt.Pipe.openpipe out_fd with
      | Ok x -> Pipe x,in_fd
      | Error x ->
        pclose out_fd;
        pclose in_fd;
        raise (U.Unix_error(Uwt.to_unix_error x,"pipe",""))

  let pipe_out () =
    match epipe true with
    | Error x -> raise (U.Unix_error(Uwt.to_unix_error x,"pipe",""))
    | Ok (out_fd, in_fd) ->
      match Uwt.Pipe.openpipe in_fd with
      | Ok x -> out_fd,Pipe x
      | Error x ->
        pclose out_fd;
        pclose in_fd;
        raise (U.Unix_error(Uwt.to_unix_error x,"pipe",""))

  let system s =
    let s = Uwt_process.shell s in
    help ( fun () -> Uwt_process.exec s )

  let utimes s access modif = UF.utime s ~access ~modif

  let getcwd = Uwt.Unix.getcwd

  let chroot = Uwt.Unix.chroot

  type signal_handler_id = Uwt.Signal.t

  let on_signal_full signum cb =
    Uwt.Signal.start_exn signum ~cb

  let on_signal signum f =
    let cb _id sig' = f sig' in
    Uwt.Signal.start_exn signum ~cb

  let disable_signal_handler = Uwt.Signal.close_noerr

end

module Lwt_io = struct

  include Uwt_io

  let openf_trans = function
    | None -> None
    | Some l -> Some(List.fold_left Lwt_unix.trans_of [] l)

  let open_file : type m. ?buffer : Uwt_bytes.t -> ?flags : Unix.open_flag list -> ?perm : Unix.file_perm -> mode : m mode -> file_name -> m channel Lwt.t = fun ?buffer ?flags ?perm ~mode filename ->
    let flags = openf_trans flags in
    Uwt_io.open_file ?buffer ?flags ?perm ~mode filename

  let with_file ?buffer ?flags ?perm ~mode filename f =
    let flags = openf_trans flags in
    Uwt_io.with_file ?buffer ?flags ?perm ~mode filename f

  let open_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix () =
    let flags = openf_trans flags in
    Uwt_io.open_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix ()

  let with_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix f =
    let flags = openf_trans flags in
    Uwt_io.with_temp_file ?buffer ?flags ?perm ?temp_dir ?prefix f

end
