open OUnit
open Lwt.Infix
let return = Lwt.return
let m_equal s t =
  assert_equal s (Uwt.Main.run t)
let m_raises (a,b,c) t =
  assert_raises
    (Uv.Uv_error(a,b,c))
    (fun () -> Uwt.Main.run t)
let m_true t = m_equal true t

let try_finally fnormal finalizer =
  let module T = struct type 'a t = Ok of 'a | Error of exn end in
  Lwt.catch ( fun () ->
      fnormal () >>= fun x ->
      Lwt.return (T.Ok x)
    ) ( fun exn -> Lwt.return (T.Error exn) ) >>= fun t ->
  finalizer () >>= fun () ->
  match t with
  | T.Ok x -> Lwt.return x
  | T.Error p -> Lwt.fail p


let has_ip6 =
  Uv_misc.interface_addresses_exn () |> Array.to_list |>
  List.exists ( fun x ->
      Uv.Conv.unix_sockaddr_of_sockaddr x.Uv_misc.address
      |> Unix.domain_of_sockaddr = Unix.PF_INET )

let ip6_option =
  OUnit2.Conf.make_bool "no_ip6" false "force ignoring of ip6 related tests"

let multiplicand =
  OUnit2.Conf.make_int
    "multiplicand"
    1
    "control how much data is written in stress tests"

let contingent =
  OUnit2.Conf.make_bool
    "skip_contingent"
    true
    "skip test cases which results are highly contingent"

let all =
  OUnit2.Conf.make_bool
    "all"
    false
    "enable all possible test cases"

let is_contingent ctx =
  OUnit2.skip_if
    ( contingent ctx && all ctx = false )
    "skip contingent test"

let ip6_only ctx =
  let n = ip6_option ctx in
  OUnit2.skip_if
    ((not has_ip6 || n) && all ctx = false )
    "no ip6"

let has_ip6 ctx =
  has_ip6 && not (ip6_option ctx)

let no_win () = OUnit2.skip_if Sys.win32 "no windows support (yet)"

module D = struct
  let show_sockaddr s =
    let open Uv in
    let open Uv_misc in
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
  type sockaddr = [%import: Uv.sockaddr]
  type timeval = [%import: Uv_misc.timeval] [@@deriving show]
  type cpu_times = [%import: Uv_misc.cpu_times] [@@deriving show]
  type cpu_info = [%import: Uv_misc.cpu_info] [@@deriving show]
  type interface_address = [%import: Uv_misc.interface_address] [@@deriving show]
  type rusage = [%import: Uv_misc.rusage] [@@deriving show]

  type socket_domain = [%import: Unix.socket_domain] [@@deriving show]
  type socket_type = [%import: Unix.socket_type] [@@deriving show]
  type addr_info = [%import: Uwt.Dns.addr_info] [@@deriving show]
  type file_kind = [%import: Uv.Fs.file_kind] [@@deriving show]
  type stats = [%import: Uv.Fs.stats] [@@deriving show]

  let qstat x = show_stats x |> String.length > 50
end
(*
module Fs_t = struct
  let qstat x = D.show_stats x |> String.length > 50
end
*)
