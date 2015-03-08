open Lwt.Infix

module T = Uwt.Timer
module L = Lwt

let sleep () =
  T.sleep 1_000 >>= fun () ->
  Uwt_io.printl "slept!"

let once () = (* like sleep *)
  let sleeper,waker = Lwt.task () in
  let cb _ = Lwt.wakeup waker () in
  let (_:T.t) = T.start_exn ~repeat:0  ~timeout:2_000 ~cb in
  sleeper >>= fun () ->
  Uwt_io.printl "once"

let three_times () =
  let i = ref 2 in
  let sleeper,waker = Lwt.task () in
  let cb t =
    let ic = !i in
    Uwt_io.printf "%d\n%!" ic |> Lwt.ignore_result;
    decr i;
    if ic = 0 then (
      T.close_noerr t;
      Lwt.wakeup waker ()
    )
  in
  let (_:T.t) = T.start_exn ~repeat:1_000  ~timeout:1_000 ~cb in
  sleeper

let () =
  Uwt.Main.run (L.join [ once () ; sleep (); three_times () ])


let () = Uwt.valgrind_happy ()
