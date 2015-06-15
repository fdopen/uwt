open Common

open Lwt.Infix
let dnstest host =
  let module UD = Uwt.Dns in
  let opts = Unix.([AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_DGRAM ]) in
  UD.getaddrinfo ~host ~service:"" opts >>= function
  | [] -> Lwt.fail_with "nothing found"
  | (hd::_) as s1 ->
    UD.getnameinfo hd.UD.ai_addr [] >>= fun n1 ->
    no_win (); (* doesn't work, why? *)
    let opts2 = Unix.([AI_FAMILY PF_INET6; AI_SOCKTYPE SOCK_STREAM ]) in
    UD.getaddrinfo ~host ~service:"www" opts2 >>= function
    | [] -> Lwt.fail_with "nothing found"
    | (hd::_) as s2 ->
      UD.getnameinfo hd.UD.ai_addr [] >>= fun n2 ->
      if
        n1.UD.hostname <> "" &&
        n2.UD.hostname <> "" &&
        String.length n1.UD.service > 0  &&
        String.length n2.UD.service > 0  &&
        List.length (List.map D.show_addr_info s1) > 0 &&
        List.length (List.map D.show_addr_info s2) > 0 &&
        (List.for_all (fun x -> x.UD.ai_family = Unix.PF_INET &&
                                x.UD.ai_socktype = Unix.SOCK_DGRAM) s1) &&
        List.for_all (fun x -> x.UD.ai_family = Unix.PF_INET6 &&
                               x.UD.ai_socktype = Unix.SOCK_STREAM) s2
      then
        Lwt.return_true
      else
        Lwt.return_false

open OUnit2

let l = [
  ("getaddrinfo/getnameinfo">::
   fun _ctx ->
     let open Uwt in
     m_true ( dnstest "google.com" );
     m_true ( Lwt.catch ( fun () -> dnstest "asdfli4uqoi5tukjgakjlhadfkogle.com"
                          >>= fun _ -> Lwt.return_false )
                (function
                | Uwt_error((EAI_NONAME|ENOENT|EAI_NODATA),_,_) ->
                  Lwt.return_true
                | x -> Lwt.fail x )));
]
let l = "Dns">:::l
