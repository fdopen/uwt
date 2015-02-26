open OUnit
open Lwt.Infix
let return = Lwt.return
let m_equal s t =
  assert_equal s (Uwt.Main.run t)
let m_raises (a,b,c) t =
  assert_raises
    (Uwt.Uwt_error(a,b,c))
    (fun () -> Uwt.Main.run t)
let m_true t =
  assert_equal true (Uwt.Main.run t)

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
