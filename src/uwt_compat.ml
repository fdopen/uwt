module Lwt_main = Uwt.Main
module Lwt_io = Uwt_io
module Lwt_log = Uwt_log
module Lwt_bytes = Uwt_bytes
module Lwt_process = Uwt_process
module Lwt_throttle = Uwt_throttle
module Lwt_chan = Uwt_chan

module Lwt_unix = struct

  module UU = Uwt.Unix
  module UF = Uwt.Fs
  module U = Unix

  open Lwt.Infix

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

  let from_file_descr = function
  | File x -> `File x
  | Tcp x -> `Tcp x
  | Pipe x -> `Pipe x

  let to_file_descr = function
  | `File x -> File x
  | `Tcp x -> Tcp x
  | `Pipe x -> Pipe x

  type f_common =
    | Fd of Uwt.file
    | Stream of Uwt.Stream.t

  let stdin = File Uwt.stdin
  let stdout = File Uwt.stdout
  let stderr = File Uwt.stderr

  let trans_exn x = Lwt.fail (Uwt.to_unix_exn x)

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
  | U.O_SHARE_DELETE
  | U.O_CLOEXEC -> accu

  let ufail ?(param="") ecode s1 =
    Lwt.fail (U.Unix_error(ecode,s1,param))

  let helpn n t =
    Lwt.catch
      t
      (function
      | Uwt.Uwt_error(Uwt.ENONET,s,_) when s = n -> Lwt.fail Not_found
      | x -> trans_exn x)

  let help f =
    Lwt.catch f trans_exn

  let help1 f s1 =
    Lwt.catch
      ( fun () -> f s1 )
      trans_exn

  let help1n n f s1 =
    helpn n ( fun () -> f s1 )

  let help2 f s1 s2 =
    Lwt.catch
      ( fun () -> f s1 s2 )
      trans_exn

  let help2n n f s1 s2 =
    helpn n ( fun () -> f s1 s2 )

  let help3 f s1 s2 s3 =
    Lwt.catch
      ( fun () -> f s1 s2 s3 )
      trans_exn

  let openfile path flags perm =
    let f () =
      let mode = List.fold_left trans_of [] flags in
      UF.openfile ~mode ~perm path >>= fun fd ->
      Lwt.return (File fd)
    in
    help f

  let to_com = function
  | File fd -> Fd fd
  | Pipe fd -> Stream (Uwt.Pipe.to_stream fd)
  | Tcp fd -> Stream (Uwt.Tcp.to_stream fd)

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
    | Fd fd -> help1 UF.close fd
    | Stream fd ->
      Uwt.Stream.close fd |> it

  let read fd buf ofs len =
    let fd = to_com fd in
    let f () =
      match fd with
      | Fd fd -> UF.read ~pos:ofs ~len:len ~buf fd
      | Stream fd -> Uwt.Stream.read ~pos:ofs ~len:len ~buf fd
    in
    help f

  let write fd buf ofs len =
    let fd = to_com fd in
    let f () =
      match fd with
      | Fd fd -> UF.write ~pos:ofs ~len:len ~buf fd
      | Stream fd ->
        Uwt.Stream.write ~pos:ofs ~len:len ~buf fd >>= fun () ->
        Lwt.return len
    in
    help f

  let write_string fd buf ofs len =
    let fd = to_com fd in
    let f () =
      match fd with
      | Fd fd -> UF.write_string ~pos:ofs ~len:len ~buf fd
      | Stream fd ->
        Uwt.Stream.write_string ~pos:ofs ~len:len ~buf fd >>= fun () ->
        Lwt.return len
    in
    help f

  let lseek fd n com =
    match fd with
    | Pipe _
    | Tcp _ -> ufail U.ESPIPE "lseek"
    | File fd ->
      help3 UU.lseek fd (Int64.of_int n) com >>= fun l ->
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
    | Pipe _
    | Tcp _ -> ufail U.EINVAL "ftruncate"
    | File fd ->
      let f () = UF.ftruncate fd ~len:(Int64.of_int i) in
      help f

  let fsync fd =
    match fd with
    | Pipe _
    | Tcp _ -> ufail U.EINVAL "fsync"
    | File fd ->
      help1 UF.fsync fd

  let fdatasync fd =
    match fd with
    | Pipe _
    | Tcp _ -> ufail U.EINVAL "fdatasync"
    | File fd ->
      help1 UF.fdatasync fd

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
    let open Unix in
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
    | Pipe _
    | Tcp _ -> ufail U.EINVAL "fsync"
    | File fd ->
      help1 UF.fstat fd >>= stat_convert

  let isatty x =
    (match x with
    | File fd -> Lwt.return (Some fd)
    | Tcp _ -> Lwt.return_none
    | Pipe fd ->
      match Uwt.Pipe.fileno fd with
      | Error _ -> Lwt.return_none
      | Ok fd ->
        Uwt.Conv.file_of_file_descr fd |> Lwt.return)
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
      | Uwt.Uwt_error(Uwt.ENOENT, _, _) -> Lwt.return_false
      | x -> trans_exn x)

  module LargeFile = struct

    type stats = U.LargeFile.stats
    let stat_convert x =
      let open U.LargeFile in
      Lwt.return {
        st_dev = x.UF.st_dev;
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
      | Pipe _
      | Tcp _ -> ufail U.EINVAL "fsync"
      | File fd ->
        help1 UF.fstat fd >>= stat_convert

    let lseek fd n com =
      match fd with
      | Pipe _
      | Tcp _ -> ufail U.ESPIPE "lseek"
      | File fd ->
        help3 UU.lseek fd n com >>= fun l ->
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
      | Pipe _
      | Tcp _ -> ufail U.EINVAL "ftruncate"
      | File fd ->
        let f () = UF.ftruncate fd ~len:i in
        help f

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
    | Pipe _
    | Tcp _ -> ufail U.EBADF "fchmod"
    | File fd ->
      help ( fun () -> UF.fchmod fd ~perm )

  let chown f uid gid =
    help ( fun () -> UF.chown f ~uid ~gid )

  let fchown fd uid gid =
    match fd with
    | Pipe _
    | Tcp _ -> ufail U.EBADF "fchown"
    | File fd ->
      help ( fun () -> UF.fchown fd ~uid ~gid )

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
    help1 UF.scandir path >>= fun a ->
    Lwt.return {
      d_closed = false;
      d_pos = 0;
      d_len = Array.length a;
      d_files = Array.map snd a }

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

  let getlogin = help1 UU.getlogin
  let getpwnam = help1n "getpwnam" UU.getpwnam
  let getgrnam = help1n "getgrnam" UU.getgrnam
  let getpwuid = help1n "getpwuid" UU.getpwuid
  let getgrgid = help1n "getgrgid" UU.getgrgid
  let chdir = help1 UU.chdir
  let gethostname = help1 UU.gethostname
  let gethostbyname = help1n "gethostbyname" UU.gethostbyname
  let gethostbyaddr = help1n "gethostbyaddr" UU.gethostbyaddr
  let getprotobyname = help1n "getprotobyname" UU.getprotobyname
  let getprotobynumber = help1n "getprotobynumber" UU.getprotobynumber
  let getservbyname name protocol =
    helpn "getservbyname" (fun () -> UU.getservbyname ~name ~protocol )
  let getservbyport = help2n "getservbyport" UU.getservbyport

  let getaddrinfo host service (options:Unix.getaddrinfo_option list) =
    Uwt.Dns.getaddrinfo ~host ~service options >>=fun l ->
    let f a =
      let open Unix in
      { ai_family = a.Uwt.Dns.ai_family;
        ai_socktype = a.Uwt.Dns.ai_socktype;
        ai_protocol = a.Uwt.Dns.ai_protocol;
        ai_canonname = a.Uwt.Dns.ai_canonname;
        ai_addr = a.Uwt.Dns.ai_addr }
    in
    Lwt.return (List.map f l)

  let getaddrinfo = help3 getaddrinfo

  let getnameinfo addr l =
    help2n "getnameinfo" Uwt.Dns.getnameinfo addr l

  let pipe () =
    try
      let fd1,fd2 = UU.pipe_exn () in
      Pipe fd1, Pipe fd2
    with
    | Uwt.Uwt_error(x,y,z) -> raise (U.Unix_error(Uwt.to_unix_error x,y,z))

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
end
