open Common
open Lwt.Infix

let poll_file () =
  Uwt.Fs.mkdtemp "uwt-fs-poll.XXXXXX" >>= fun tmpfile ->
  Lwt.finalize ( fun () ->
      let cb_called = ref 0 in
      let sleeper,waker = Lwt.task () in
      let abort s msg =
        Uwt.Fs_poll.close_noerr s;
        Lwt.wakeup_exn waker (Failure msg)
      in
      let cb t = function
      | Uwt.Error x -> abort t @@ Uwt.strerror x
      | Uwt.Ok x ->
        let open Uwt.Fs_poll in
        let s1 = String.length @@ Show_uwt.show_stats x.curr
        and s2 = String.length @@ Show_uwt.show_stats x.prev in
        assert ( s1 - s2 <> max_int );
        if D.qstat x.curr &&
           D.qstat x.prev &&
           Int64.sub x.curr.Uwt.Fs.st_mtime x.prev.Uwt.Fs.st_mtime > 0L then
          incr cb_called
        else
          abort t "unexpected change"
      in
      let s = Uwt.Fs_poll.start_exn tmpfile 2_000 ~cb in
      Lwt.finalize ( fun () ->
          let rec iter i =
            if i = 0 then
              Uwt.Timer.sleep 1_800
            else
              Uwt.Timer.sleep 1_800 >>= fun () ->
              let t = Unix.gettimeofday () in
              Uwt.Fs.utime tmpfile ~access:t ~modif:t >>= fun () ->
              iter (pred i)
          in
          Lwt.pick [ iter 3 ; sleeper ] >|= fun () ->
          !cb_called = 3
        ) ( fun () -> Uwt.Fs_poll.close_noerr s; Lwt.return_unit )
    ) ( fun () -> remove_dir tmpfile; Lwt.return_unit )

open OUnit2
let l = [
  ("poll_file">:: fun ctx ->
      is_contingent ctx;
      m_true ( poll_file () ));
]

let l = "Fs_poll">:::l
