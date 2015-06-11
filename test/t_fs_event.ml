open OUnit2
open Lwt.Infix
open Common

let write_something tmp_dir =
  let rec iter n =
    Uwt.Timer.sleep 200 >>= fun () ->
    if n > 3 then
      Lwt.return_false
    else
      let fln = Filename.concat tmp_dir (string_of_int n) in
      let open Uwt.Fs in
      openfile ~mode:([O_WRONLY; O_CREAT]) fln >>= fun fd ->
      close fd >>= fun () -> iter (succ n)
  in
  iter 0

let test () =
  let cnt = ref 0 in
  let error = ref false in
  let sleep,waker = Lwt.task () in
  let cb t = function
  | Uwt.Error _ ->
    Uwt.Fs_event.close_noerr t;
    error:= true;
    Lwt.wakeup waker ()
  | Uwt.Ok _  -> (* I don't test the content, because it is not provided by
                    all platforms *)
    incr cnt;
    if !cnt > 3 then (
      Uwt.Fs_event.close_noerr t;
      Lwt.wakeup waker ()
    );
  in
  let fln = "uwt-test.XXXXXX" in
  Uwt.Fs.mkdtemp fln >>= fun fln ->
  let remove  = lazy (
    let cmd = "rm -rf " ^ Filename.quote fln in
    Sys.command cmd |> ignore )
  in
  Lwt.catch ( fun () ->
      let t = Uwt.Fs_event.start_exn fln [] ~cb in
      Lwt.finalize ( fun () ->
          let t1 = sleep >>= fun () -> return (!error = false) in
          let t2 = write_something fln in
          Lwt.pick [ t1 ; t2 ] >>= fun x -> Lazy.force remove ; return x
        ) (fun () -> Uwt.Fs_event.close_noerr t ; Lwt.return_unit)
    ) ( fun exn -> Lazy.force remove ; Lwt.fail exn )

let l = [
  ("tmpdir watcher">::
   fun _ctx ->
     m_true (test ())); ]

let l = "Fs_event">:::l
