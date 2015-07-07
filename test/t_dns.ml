open Common

type 'a h =
  | Exn of exn
  | Ret of 'a

open Lwt.Infix

let help f =
  Lwt.catch
    ( fun () -> f () >>= fun s -> Lwt.return (Ret s) )
    ( fun exn -> Lwt.return (Exn exn) )

let getnameinfo x l =
  let module UD = Uwt.Dns in
  help ( fun () -> UD.getnameinfo x l ) >>= function
  | Exn ( Uwt.Uwt_error(Uwt.ENOENT,_,_) as exn ) ->
    let addr = Uwt.Conv.to_unix_sockaddr_exn x in
    let ok =
      try ignore (Unix.getnameinfo addr l); false with Not_found -> true
    in
    if ok then Lwt.return_none else Lwt.fail exn
  | Exn x -> Lwt.fail x
  | Ret x -> Lwt.return (Some x)

let getaddrinfo ~host ~service opts =
  let module UD = Uwt.Dns in
  UD.getaddrinfo ~host ~service opts >>= function
  | [] ->
    if Unix.getaddrinfo host service opts = [] then
      Lwt.return []
    else
      Lwt.fail_with "nothing found"
  | l -> Lwt.return l

let dnstest ctx host =
  let module UD = Uwt.Dns in
  let opts = Unix.([AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_DGRAM ]) in
  getaddrinfo ~host ~service:"" opts  >>= function
  | [] -> Lwt.return_true
  | (hd::_) as s1 ->
    getnameinfo hd.UD.ai_addr [] >>= function
    | None -> Lwt.return_true
    | Some n1 ->
      no_win ctx; (* doesn't work, why? *)
      let opts2 = Unix.([AI_FAMILY PF_INET6; AI_SOCKTYPE SOCK_STREAM ]) in
      getaddrinfo ~host ~service:"www" opts2 >>= function
      | [] -> Lwt.return_true
      | (hd::_) as s2 ->
        getnameinfo hd.UD.ai_addr [] >|= function
        | None -> true
        | Some n2 ->
          n1.Unix.ni_hostname <> "" &&
          n2.Unix.ni_hostname <> "" &&
          String.length n1.Unix.ni_service > 0  &&
          String.length n2.Unix.ni_service > 0  &&
          List.length (List.map D.show_addr_info s1) > 0 &&
          List.length (List.map D.show_addr_info s2) > 0 &&
          (List.for_all (fun x -> x.UD.ai_family = Unix.PF_INET &&
                                  x.UD.ai_socktype = Unix.SOCK_DGRAM) s1) &&
          List.for_all (fun x -> x.UD.ai_family = Unix.PF_INET6 &&
                                 x.UD.ai_socktype = Unix.SOCK_STREAM) s2
open OUnit2

let l = [
  ("getaddrinfo/getnameinfo">::
   fun ctx ->
     let open Uwt in
     m_true (dnstest ctx "google.com");
     let t =
       Lwt.catch ( fun () ->
           dnstest ctx "asdfli4uqoi5tukjgakjlhadfkogle.com"
           >|= fun _ -> false )
         (function
         | Uwt_error((EAI_NONAME|ENOENT|EAI_NODATA),_,_) -> Lwt.return_true
         | x -> Lwt.fail x )
     in
     m_true t);
]
let l = "Dns">:::l
