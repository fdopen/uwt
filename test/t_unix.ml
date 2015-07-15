open OUnit2
open Lwt.Infix
open Common

let is_http x =
  if x.Unix.s_port <> 80 then
    false
  else
    match String.lowercase x.Unix.s_name with
    | "http" | "www" -> true
    | _ -> false

type 'a t =
  | Exn of exn
  | Result of 'a

let runix f a =
  try
    Result(f a)
  with
  | exn -> Exn exn

let ruwt (t:'a Lwt.t) : 'a t =
  try
    let erg = Uwt.Main.run t in
    Result erg
  with
  | exn -> Exn exn

let adv_equal t1 t2 =
  match t1,t2 with
  | Result _, Result _ -> assert_equal t1 t2
  | Exn a, Result _
  | Result _, Exn a -> raise a
  | Exn a, Exn b ->
    match a,b with
    | Unix.Unix_error(a',_,_), Unix.Unix_error(b',_,_) ->
      if a' = b' then
        assert_equal a' b'
      else
        raise a
    | _ ->  raise a

module UUnix = Uwt_compat.Lwt_unix
module U = Unix
module UU = Uwt.Unix

let with_file = T_fs.with_file

let unix_equal f1 f2 p =
  m_true ( f1 p >>= fun s ->
           let s' = f2 p in
           return ( s = s' ))

let (//) = Filename.concat
let lock_helper =
  Filename.dirname Sys.executable_name //
  if Sys.win32 then "lock_helper.exe" else "lock_helper"


let invalid_path = match Sys.win32 with
| true -> "C:\\aYdf\\aYs"
| false -> "/aYdf/aYs"

let l = [
  ("gethostname">::
   fun _ctx ->
     unix_equal UU.gethostname U.gethostname ());
  ("gethostbyname">::
   fun _ctx ->
     m_equal U.PF_INET (
       UU.gethostbyname "google.com" >|= fun s -> s.U.h_addrtype);
     let name = "verylongandinvalidadzuarztgjbgf.com" in
     m_raises (Uwt.ENOENT,"gethostbyname",name)(
       UU.gethostbyname name));
  ("gethostbyaddr">::
   fun ctx ->
     let ip4 = U.inet_addr_of_string "8.8.8.8" in
     m_equal U.PF_INET (
       UU.gethostbyaddr ip4 >>=
       fun s -> Lwt.return s.U.h_addrtype);
     ip6_only ctx;
     let s_invalid = "1234:1234:1234::1234" in
     let ip6 = U.inet_addr_of_string "2001:4860:4860::8888"
     and ip6_invalid = U.inet_addr_of_string s_invalid in
     m_equal U.PF_INET6 (
       UU.gethostbyaddr ip6 >|= fun s -> s.U.h_addrtype);
     m_raises (Uwt.ENOENT,"gethostbyaddr","")(
       UU.gethostbyaddr ip6_invalid));
  ("getprotobyname">::
   fun _ctx ->
     unix_equal UU.getprotobyname U.getprotobyname "icmp";
     m_raises (Uwt.ENOENT,"getprotobyname","uwt")(
       UU.getprotobyname "uwt" >|= fun s -> s.U.p_proto));
  ("getprotobynumber">::
   fun _ctx ->
     unix_equal UU.getprotobynumber U.getprotobynumber 17;
     m_raises (Uwt.ENOENT,"getprotobynumber","")(
       UU.getprotobynumber max_int >|= fun s -> s.U.p_proto));
  ("getservbyname">::
   fun _ctx ->
     m_true (
       UU.getservbyname ~name:"www" ~protocol:"" >|= fun s -> is_http s );
     m_true (
       UU.getservbyname ~name:"www" ~protocol:"tcp" >|= fun s -> is_http s );
     m_raises (Uwt.ENOENT,"getservbyname","uwt")(
       UU.getservbyname ~name:"uwt" ~protocol:"udp" >|= fun s -> s.U.s_name));
  ("getservbyport">::
   fun _ctx ->
     m_true (
       UU.getservbyport 80 "" >|= fun s -> is_http s);
     m_true (
       UU.getservbyport 80 "tcp" >|= fun s -> is_http s);
     m_raises (Uwt.ENOENT,"getservbyport","udp")(
       UU.getservbyport 54325 "udp" >|= fun s -> s.U.s_name));
  ("getcwd">::
   fun _ ->
     let s1 = runix Sys.getcwd () in
     let s2 = ruwt @@ UU.getcwd () in
     adv_equal s1 s2);
  ("realpath">::
   fun _ ->
     let t = UU.realpath "." >>= fun s ->
       Uwt.Fs.stat s >|= fun l -> l.Uwt.Fs.st_kind = Uwt.Fs.S_DIR && s <> "."
     in
     m_true t );
  ("realpath_enoent">::
   fun _ ->
     let t = UU.realpath invalid_path in
     m_raises (Uwt.ENOENT,"realpath",invalid_path) t);
  ("chdir">::
   fun _ ->
     let orig = Sys.getcwd () in
     let real_tmpdir = Uwt.Main.run @@ UU.realpath @@ tmpdir () in
     Common.nm_try_finally ( fun () ->
         m_true (UU.chdir (tmpdir ()) >>= fun () ->
                 UU.getcwd () >>= fun s ->
                 UU.realpath s >|= fun cwd_real ->
                 cwd_real = real_tmpdir &&
                 tmpdir () <> orig);
         m_raises (Uwt.ENOENT,"chdir",invalid_path)(UU.chdir invalid_path)
       ) () U.chdir orig );
  ("getlogin">::
   fun ctx ->
     let s1 = runix U.getlogin () in
     let s2 = ruwt @@ UUnix.getlogin () in
     (* UUnix.getlogin uses getlogin_r on some systems. If stdin isn't a
        a tty, it won't work on some systems. *)
     let is_tty = Uwt_base.Misc.guess_handle Uwt.stdin = Uwt_base.Misc.Tty in
     skip_if_not_all ctx (not is_tty && s1 <> s2 ) "stdin no tty";
     adv_equal s1 s2);
  ("getpwnam">::
   fun ctx ->
     no_win ctx;
     let user =
       try U.getlogin () with U.Unix_error _ ->
         try Sys.getenv "USER" with Not_found ->
           Sys.getenv "USERNAME"
     in
     let s1 = runix U.getpwnam user in
     let s2 = ruwt @@ UUnix.getpwnam user in
     adv_equal s1 s2;
     let pwnam = "adfklXakja" in
     m_raises (Uwt.ENOENT,"getpwnam",pwnam)(
       UU.getpwnam pwnam));
  ("getpwuid">::
   fun ctx ->
     no_win ctx;
     let user = try U.getuid () with U.Unix_error _ -> 0 in
     let s1 = runix U.getpwuid user in
     let s2 = ruwt @@ UUnix.getpwuid user in
     adv_equal s1 s2;
     m_raises (Uwt.ENOENT,"getpwuid","")(
       UU.getpwuid (user*79 + 17) ));
  ("getgrnam">::
   fun ctx ->
     no_win ctx;
     let s1 = U.getgid () in
     let name = (U.getgrgid s1).U.gr_name in
     let s1 = runix U.getgrnam name in
     let s2 = ruwt @@ UUnix.getgrnam name in
     adv_equal s1 s2;
     let grnam = "adfklXakja" in
     m_raises (Uwt.ENOENT,"getgrnam",grnam)(
       UU.getgrnam grnam));
  ("getgrgid">::
   fun ctx ->
     no_win ctx;
     let gid = U.getgid () in
     let s1 = runix U.getgrgid gid in
     let s2 = ruwt @@ UUnix.getgrgid gid in
     adv_equal s1 s2;
     m_raises (Uwt.ENOENT,"getgrgid","")(
       UU.getgrgid (gid*79 + 17)));
  ("lockf">::
   fun _ ->
     let file = (tmpdir ()) // "lockf" in
     let cmd = (Filename.quote lock_helper) ^ " " ^ (Filename.quote file) in
     let cmd = match Sys.win32 with
     | true -> "\"" ^ cmd ^ "\""
     | false -> cmd
     in
     let t =
       with_file ~mode:[Uwt.Fs.O_WRONLY; Uwt.Fs.O_CREAT] file @@ fun fd ->
       UU.lockf fd U.F_TLOCK 0L >|= fun () -> Sys.command cmd = 0
     in
     m_true t);
  ("lockf2">::
   fun _ ->
     let file = (tmpdir ()) // "lockf2" in
     let cmd = (Filename.quote lock_helper) ^ " -h " ^ (Filename.quote file) in
     let cmd = match Sys.win32 with
     | true -> "\"" ^ cmd ^ "\""
     | false -> cmd
     in
     let t =
       ignore (Uwt_preemptive.detach ( fun () -> Sys.command cmd ) ());
       Uwt.Timer.sleep 750 >>= fun () ->
       with_file ~mode:[Uwt.Fs.O_WRONLY; Uwt.Fs.O_CREAT] file @@ fun fd ->
       Lwt.catch ( fun () ->
           UU.lockf fd U.F_TLOCK 0L >>= fun () -> Lwt.return_false
         ) ( fun exn ->
           match exn with
           | Uwt.Uwt_error((Uwt.EBUSY|Uwt.EACCES|Uwt.EAGAIN),"lockf",_) ->
             Lwt.return_true
           | x -> Lwt.fail x )
     in
     m_true t);
  ("pipe">::
   fun _ ->
     let pin,pout = Uwt_io.pipe ~cloexec:true () in
     let message = rstring_create 65_541 in
     let result = ref "" in
     let t1 = Uwt_io.write pout message >>= fun () -> Uwt_io.close pout
     and t2 = Uwt_io.read pin >>= fun s -> result:=s; Uwt_io.close pin in
     m_equal message ( Lwt.join [t1;t2] >|= fun () -> !result ));
]

let l = "Unix">:::l
