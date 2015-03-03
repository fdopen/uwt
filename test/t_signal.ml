open Lwt.Infix
let wait_for_signal () =
  let sleeper,waker = Lwt.task () in
  let cb sighandle i  =
    Uwt.Signal.close_noerr sighandle;
    if i <> Sys.sigusr1 then
      Lwt.wakeup_exn waker (Failure "wrong signal")
    else
      Lwt.wakeup waker true
  in
  let sighandle = Uwt.Signal.start_exn Sys.sigusr1 ~cb in
  Lwt.on_cancel sleeper ( fun () -> Uwt.Signal.close_noerr sighandle);
  sleeper

let send_signal () =
  Uwt.Timer.sleep 10 >>= fun () ->
  let pid = Unix.getpid ()
  and signum = Sys.sigusr1 in
  Uwt.Process.kill_exn ~pid ~signum;
  Uwt.Timer.sleep 10 >>= fun () ->
  Lwt.return_false

open Common
open OUnit2
let l = [
  ("start_exn">::
   fun _ctx ->
     no_win ();
     m_true (Lwt.pick [wait_for_signal (); send_signal ()]))
]

let l = "Signal">:::l
