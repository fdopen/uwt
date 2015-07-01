open OUnit2
open Lwt.Infix
open Common

let is_http x =
  if x.Unix.s_port <> 80 then
    Lwt.return_false
  else
    match String.lowercase x.Unix.s_name with
    | "http" | "www" -> Lwt.return_true
    | _ -> Lwt.return_false

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
     m_equal Unix.PF_INET (
       Uwt.Unix.gethostbyname "google.com" >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     let name = "verylongandinvalidadzuarztgjbgf.com" in
     m_raises (Uwt.ENOENT,"gethostbyname",name)(
       Uwt.Unix.gethostbyname name));
  ("gethostbyaddr">::
   fun ctx ->
     let ip4 = Unix.inet_addr_of_string "8.8.8.8" in
     m_equal Unix.PF_INET (
       Uwt.Unix.gethostbyaddr ip4 >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     ip6_only ctx;
     let s_invalid = "1234:1234:1234::1234" in
     let ip6 = Unix.inet_addr_of_string "2001:4860:4860::8888"
     and ip6_invalid = Unix.inet_addr_of_string s_invalid in
     m_equal Unix.PF_INET6 (
       Uwt.Unix.gethostbyaddr ip6 >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     m_raises (Uwt.ENOENT,"gethostbyaddr","")(
       Uwt.Unix.gethostbyaddr ip6_invalid));
  ("getprotobyname">::
   fun _ctx ->
     unix_equal UU.getprotobyname U.getprotobyname "icmp";
     m_raises (Uwt.ENOENT,"getprotobyname","uwt")(
       Uwt.Unix.getprotobyname "uwt" >>= fun s ->
       return s.Unix.p_proto));
  ("getprotobynumber">::
   fun _ctx ->
     unix_equal UU.getprotobynumber U.getprotobynumber 17;
     m_raises (Uwt.ENOENT,"getprotobynumber","")(
       Uwt.Unix.getprotobynumber max_int >>= fun s ->
       return s.Unix.p_proto) );
  ("getservbyname">::
   fun _ctx ->
     m_true (
       Uwt.Unix.getservbyname ~name:"www" ~protocol:"" >>=
       fun s -> is_http s );
     m_true (
       Uwt.Unix.getservbyname ~name:"www" ~protocol:"tcp" >>=
       fun s -> is_http s );
     m_raises (Uwt.ENOENT,"getservbyname","uwt")(
       Uwt.Unix.getservbyname ~name:"uwt" ~protocol:"udp" >>=
       fun s -> return s.Unix.s_name));
  ("getservbyport">::
   fun _ctx ->
     m_true (
       Uwt.Unix.getservbyport 80 "" >>= fun s -> is_http s);
     m_true (
       Uwt.Unix.getservbyport 80 "tcp" >>= fun s -> is_http s);
     m_raises (Uwt.ENOENT,"getservbyport","udp")(
       Uwt.Unix.getservbyport 54325 "udp" >>=
       fun s -> return s.Unix.s_name));
  ("getcwd">::
   fun _ ->
     let s1 = runix Sys.getcwd () in
     let s2 = ruwt @@ Uwt.Unix.getcwd () in
     adv_equal s1 s2);
  ("realpath">::
   fun _ ->
     let t = Uwt.Unix.realpath "." >>= fun s ->
       Uwt.Fs.stat s >>= fun l ->
       return (l.Uwt.Fs.st_kind = Uwt.Fs.S_DIR &&
               s <> ".")
     in
     m_true t );
  ("realpath_enoent">::
   fun _ ->
     let t = Uwt.Unix.realpath invalid_path >>= fun (_:string) ->
       Lwt.return_true
     in
     m_raises (Uwt.ENOENT,"realpath",invalid_path) t);
  ("chdir">::
   fun _ ->
     let orig = Sys.getcwd () in
     let real_tmpdir = Uwt.Main.run @@ Uwt.Unix.realpath @@ tmpdir () in
     Common.nm_try_finally ( fun () ->
         m_true (Uwt.Unix.chdir (tmpdir ()) >>= fun () ->
                 Uwt.Unix.getcwd () >>= fun s ->
                 Uwt.Unix.realpath s >>= fun cwd_real ->
                 let erg =
                   cwd_real = real_tmpdir &&
                   tmpdir () <> orig
                 in
                 Lwt.return erg );
         m_raises (Uwt.ENOENT,"chdir",invalid_path)(Uwt.Unix.chdir invalid_path)
       ) () Unix.chdir orig );
  ("getlogin">::
   fun ctx ->
     let s1 = runix Unix.getlogin () in
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
       try Unix.getlogin () with Unix.Unix_error _ ->
         try Sys.getenv "USER" with Not_found ->
           Sys.getenv "USERNAME"
     in
     let s1 = runix Unix.getpwnam user in
     let s2 = ruwt @@ UUnix.getpwnam user in
     adv_equal s1 s2;
     let pwnam = "adfklXakja" in
     m_raises (Uwt.ENOENT,"getpwnam",pwnam)(
       Uwt.Unix.getpwnam pwnam));
  ("getpwuid">::
   fun ctx ->
     no_win ctx;
     let user = try Unix.getuid () with Unix.Unix_error _ -> 0 in
     let s1 = runix Unix.getpwuid user in
     let s2 = ruwt @@ UUnix.getpwuid user in
     adv_equal s1 s2;
     m_raises (Uwt.ENOENT,"getpwuid","")(
       Uwt.Unix.getpwuid (user*79 + 17) ));
  ("getgrnam">::
   fun ctx ->
     no_win ctx;
     let s1 = Unix.getgid () in
     let name = (Unix.getgrgid s1).Unix.gr_name in
     let s1 = runix Unix.getgrnam name in
     let s2 = ruwt @@ UUnix.getgrnam name in
     adv_equal s1 s2;
     let grnam = "adfklXakja" in
     m_raises (Uwt.ENOENT,"getgrnam",grnam)(
       Uwt.Unix.getgrnam grnam));
  ("getgrgid">::
   fun ctx ->
     no_win ctx;
     let gid = Unix.getgid () in
     let s1 = runix Unix.getgrgid gid in
     let s2 = ruwt @@ UUnix.getgrgid gid in
     adv_equal s1 s2;
     m_raises (Uwt.ENOENT,"getgrgid","")(
       Uwt.Unix.getgrgid (gid*79 + 17)));
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
       UU.lockf fd Unix.F_TLOCK 0L >>= fun () ->
       let code = Sys.command cmd in
       Lwt.return (code = 0)
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
       let x = Uwt_preemptive.detach ( fun () -> Sys.command cmd ) () in
       Uwt.Timer.sleep 750 >>= fun () ->
       with_file ~mode:[Uwt.Fs.O_WRONLY; Uwt.Fs.O_CREAT] file @@ fun fd ->
       Lwt.catch ( fun () ->
           UU.lockf fd Unix.F_TLOCK 0L >>= fun () ->
           Lwt.cancel x;
           Lwt.return_false
         ) ( fun exn ->
           Lwt.cancel x;
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
     m_equal message ( Lwt.join [t1;t2] >>= fun () -> Lwt.return !result ));
]

let l = "Unix">:::l
