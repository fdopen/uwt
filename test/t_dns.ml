open Common

open Lwt.Infix

let getnameinfo x l =
  let module UD = Uwt.Dns in
  UD.getnameinfo x l >>= function
  | Error Uwt.ENOENT ->
    let ok = try ignore (Unix.getnameinfo x l); false with Not_found -> true in
    if ok then Lwt.return_none
    else Lwt.fail (Unix.Unix_error(Unix.ENOENT,"getnameinfo",""))
  | Error x -> Lwt.fail (Unix.Unix_error(Uwt.to_unix_error x ,"getnameinfo",""))
  | Ok x -> Lwt.return (Some x)

let getaddrinfo ~host ~service opts =
  let module UD = Uwt.Dns in
  UD.getaddrinfo ~host ~service opts >>= function
  | Ok [] ->
    if Unix.getaddrinfo host service opts = [] then
      Lwt.return []
    else
      Lwt.fail_with "nothing found"
  | Ok l -> Lwt.return l
  | Error x -> Lwt.fail (Unix.Unix_error(Uwt.to_unix_error x,"getaddrinfo",host))

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
     m_true (dnstest ctx "google.com");
     let t =
       Lwt.catch ( fun () ->
           dnstest ctx "asdfli4uqoi5tukjgakjlhadfkogle.com"
           >|= fun _ -> false )
         (function
         | Unix.Unix_error(x,_,_) as exn ->
           (match Uwt.of_unix_error x with
           | Uwt.ENOENT | Uwt.EAI_NONAME | Uwt.EAI_NODATA -> Lwt.return_true
           | _ -> Lwt.fail exn)
         | x -> Lwt.fail x )
     in
     m_true t);
]
let l = "Dns">:::l
