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

let l = [
  ("gethostname">::
   fun _ctx ->
     m_true (Uwt.Unix.gethostname () >>= fun s -> return (s <> "")));
  ("gethostbyname">::
   fun _ctx ->
     m_equal Unix.PF_INET (
       Uwt.Unix.gethostbyname "google.com" >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     m_raises (Uv.ENOENT,"uwt_gethostbyname","")(
       Uwt.Unix.gethostbyname "verylongandinvalidadzuarztgjbgf.com"));
  ("gethostbyaddr">::
   fun _ctx ->
     let ip4 = Unix.inet_addr_of_string "8.8.8.8"
     and ip6 = Unix.inet_addr_of_string "2001:4860:4860::8888"
     and ip6_invalid = Unix.inet_addr_of_string "1234:1234:1234::1234" in
     m_equal Unix.PF_INET (
       Uwt.Unix.gethostbyaddr ip4 >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     m_equal Unix.PF_INET6 (
       Uwt.Unix.gethostbyaddr ip6 >>=
       fun s -> Lwt.return s.Unix.h_addrtype);
     m_raises (Uv.ENOENT,"uwt_gethostbyaddr","")(
       Uwt.Unix.gethostbyaddr ip6_invalid));
  ("getprotobyname">::
   fun _ctx ->
     m_equal 1 (
       Uwt.Unix.getprotobyname "icmp" >>= fun s ->
       return s.Unix.p_proto);
     m_raises (Uv.ENOENT,"uwt_getprotobyname","")(
       Uwt.Unix.getprotobyname "uwt" >>= fun s ->
       return s.Unix.p_proto));
  ("getprotobynumber">::
   fun _ctx ->
     m_equal "udp" (Uwt.Unix.getprotobynumber 17 >>= fun s ->
                    return @@ String.lowercase s.Unix.p_name);
     m_equal "udp" (
       Uwt.Unix.getprotobynumber 17 >>= fun s ->
       return @@ String.lowercase s.Unix.p_name));
  ("getservbyname">::
   fun _ctx ->
     m_true (
       Uwt.Unix.getservbyname ~name:"www" ~protocol:"" >>=
       fun s -> is_http s );
     m_true (
       Uwt.Unix.getservbyname ~name:"www" ~protocol:"tcp" >>=
       fun s -> is_http s );
     m_raises (Uv.ENOENT,"uwt_getservbyname","")(
       Uwt.Unix.getservbyname ~name:"uwt" ~protocol:"udp" >>=
       fun s -> return s.Unix.s_name));
  ("getservbyport">::
   fun _ctx ->
     m_true (
       Uwt.Unix.getservbyport 80 "" >>= fun s -> is_http s);
     m_true (
       Uwt.Unix.getservbyport 80 "tcp" >>= fun s -> is_http s);
     m_raises (Uv.ENOENT,"uwt_getservbyport","")(
       Uwt.Unix.getservbyport 54325 "udp" >>=
       fun s -> return s.Unix.s_name));
]

let l = "Unix">:::l
