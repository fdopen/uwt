open Common
open Lwt.Infix

let poll_file () =
  Uwt.Fs.mkdtemp "uwt-fs-poll.XXXXXX" >>= fun tmpfile ->
  try_finally ( fun () ->
      let cb_called = ref 0 in
      let sleeper,waker = Lwt.task () in
      let abort s msg =
        Uwt.Fs_poll.close_noerr s;
        Lwt.wakeup_exn waker (Failure msg)
      in
      let cb t = function
      | Uwt.Error _ -> abort t "error in cb"
      | Uwt.Ok x ->
        let open Uwt.Fs_poll in
        if Fs_t.qstat x.curr &&
           Fs_t.qstat x.prev &&
           x.curr.Uwt.Fs.st_mtime -. x.prev.Uwt.Fs.st_mtime > 0. then
          incr cb_called
        else
          abort t "unexpected change"
      in
      let s = Uwt.Fs_poll.start_exn tmpfile 2_000 ~cb in
      try_finally ( fun () ->
          let rec iter i =
            if i = 0 then
              Uwt.Timer.sleep 1_950 >>= fun () ->
              Lwt.return_unit
            else
              Uwt.Timer.sleep 1_950 >>= fun () ->
              let t = Unix.gettimeofday () in
              Uwt.Fs.utime tmpfile ~access:t ~modif:t >>= fun () ->
              iter (pred i)
          in
          Lwt.pick [ iter 3 ; sleeper ] >>= fun () ->
          if !cb_called = 3 then
            Lwt.return_true
          else
            Lwt.return_false
        ) ( fun () -> Uwt.Fs_poll.close_noerr s; Lwt.return_unit )
    ) ( fun () ->
      let cmd = "rm -rf " ^ (Filename.quote tmpfile) in
      Sys.command cmd |> ignore;
      Lwt.return_unit )

open OUnit2
let l = [
  ("poll_file">:: fun _ctx -> m_true ( poll_file () ));
]

let l = "Fs_poll">:::l
