open OUnit
open Lwt.Infix
let return = Lwt.return
let m_equal s t =
  assert_equal s (Uwt.Main.run t)
let m_raises (a,b,c) t =
  assert_raises
    (Uwt.Uwt_error(a,b,c))
    (fun () -> Uwt.Main.run t)
let m_true t = m_equal true t

let nm_try_finally f x finallly' y =
  let res =
    try f x with exn -> finallly' y; raise exn in
  finallly' y;
  res

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
  Uwt_base.Misc.interface_addresses_exn () |> Array.to_list |>
  List.exists ( fun x ->
      Uwt.Conv.unix_sockaddr_of_sockaddr x.Uwt_base.Misc.address
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
  include Show_uwt
  let qstat x = show_stats x |> String.length > 50
end
