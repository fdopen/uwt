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
  Uwt.Timer.sleep 3_000 >>= fun () ->
  Lwt.fail_with "send_signal_failure"

open Common
open OUnit2
let l = [
  ("start_exn">::
   fun ctx ->
     no_win ctx;
     m_true (Lwt.pick [wait_for_signal (); send_signal ()]));
  ("constants">::
   fun _ ->
     assert_equal Sys.sighup (-4);
     assert_equal Sys.sigint (-6);
     assert_equal Sys.sigkill (-7);
     assert_equal Sys.sigterm (-11);
  );
]

let l = "Signal">:::l
