open Lwt.Infix

open Show_uwt

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
        getaddrinfo ~ip6 host >>= function
        | Error x ->
          Lwt.fail (Unix.Unix_error(Uwt.to_unix_error x,"getaddrinfo",host))
        | Ok l ->
          Lwt_list.map_p ( fun ai ->
              Uwt.Dns.getnameinfo ai.ai_addr [] >>= function
              | Ok ni -> Lwt.return ({ ai ; ni })
              | Error x ->
                let er = Uwt.to_unix_error x in
                Lwt.fail (Unix.Unix_error(er,"getnameinfo",host))
            ) l
        >>= fun l ->
        s := (host,l) :: ! s;
        Lwt.return_unit )
      (function (* not all domains have ip6 entries. error codes differs
                   from operating system to operating system, ... *)
      | Unix.Unix_error(Unix.ENOENT,_,_) when ip6 = true-> Lwt.return_unit
      | Unix.Unix_error(Unix.EUNKNOWNERR(x),_,_) when ip6 = true && (
          x = (Uwt.Int_result.eai_noname:>int) ||
          x = (Uwt.Int_result.eai_nodata:>int) ) ->
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

let () = Uwt.Debug.valgrind_happy ()
