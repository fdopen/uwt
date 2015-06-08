open Lwt.Infix
module U = Uwt
module M = Uv_misc

(* boilerplate for output *)
let show_sockaddr s =
  let open Uv in
  let open M in
  match ip4_name s with
  | Ok ("0.0.0.0" as x) ->
    (match ip6_name s with
     | Ok s -> s
     | Error _ -> x)
  | Ok s -> s
  | Error _ ->
    match ip6_name s with
    | Ok s -> s
    | Error _ -> "(unknown)"


module Uv = struct
  include Uv
  let pp_sockaddr fmt s = show_sockaddr s |> Format.fprintf fmt "%s"
end

type socket_domain = [%import: Unix.socket_domain] [@@deriving show]
type socket_type = [%import: Unix.socket_type] [@@deriving show]
type getaddrinfo_option = [%import: Unix.getaddrinfo_option] [@@deriving show]

type sockaddr = [%import: Uv.sockaddr]
type addr_info = [%import: Uwt.Dns.addr_info] [@@deriving show]
type name_info = [%import: Uwt.Dns.name_info] [@@deriving show]

type h = {
  ai : addr_info;
  ni : name_info;
} [@@deriving show]

let getaddrinfo ~ip6 host =
  let opts = [ Unix.AI_FAMILY (if ip6 then Unix.PF_INET6 else Unix.PF_INET);
               Unix.AI_SOCKTYPE Unix.SOCK_DGRAM  ] in
  Uwt.Dns.getaddrinfo ~host ~service:"" opts

let get_infos (hosts:string list) =
  let s = ref [] in
  let help ip6 host =
    Lwt.catch ( fun () ->
        getaddrinfo ~ip6 host >>= fun l ->
        Lwt_list.map_p ( fun ai ->
            Uwt.Dns.getnameinfo ai.ai_addr [] >>= fun ni ->
            Lwt.return ({ ai ; ni }) ) l
        >>= fun l ->
        s := (host,l) :: ! s;
        Lwt.return_unit )
      (function (* not all domains have ip6 entries. error codes differs
                   from operating system to operating system, ... *)
      | Uv.Uv_error((Uv.EAI_NONAME|Uv.ENOENT|Uv.EAI_NODATA),_,_)
        when ip6 = true ->
        Lwt.return_unit
      | x -> Lwt.fail x )
  in
  Lwt_list.iter_p ( fun s -> Lwt.join [ help true s ; help false s ] ) hosts
  >>= fun () -> Lwt.return (List.rev !s)

let () =
  let t =
    get_infos [ "google.de" ; "ocaml.org" ; "caml.inria.fr" ;
                "theguardian.com" ; "camlcity.org" ] >>= fun l ->
    Lwt_list.iter_s ( fun (s,l) ->
        Uwt_io.printl s >>= fun () ->
        Lwt_list.iter_s ( fun l ->
            show_h l |> Uwt_io.printl
          ) l
      ) l
  in
  Uwt.Main.run t

let () = Uwt.valgrind_happy ()
