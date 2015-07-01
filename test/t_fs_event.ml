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
    ignore (Sys.command cmd) )
  in
  Lwt.catch ( fun () ->
      let t = Uwt.Fs_event.start_exn fln [] ~cb in
      Lwt.finalize ( fun () ->
          let t1 = sleep >>= fun () -> return (!error = false) in
          let t2 = write_something fln in
          Lwt.pick [ t1 ; t2 ] >>= fun x -> Lazy.force remove ; return x
        ) (fun () -> Uwt.Fs_event.close_noerr t ; Lwt.return_unit)
    ) ( fun exn -> Lazy.force remove ; Lwt.fail exn )

let () =
  (* Workaround for stupid oUnit.
     Mac Os X sets the environement variable __CF_USER_TEXT_ENCODING
     under certain conditions.
     And oUnit always checks for each test case, if the environment doesn't
     change.
     In the following lines I try to trigger some event, that
     sets / modify __CF_USER_TEXT_ENCODING before oUnit can
     inspect the environment. It hopefully won't change again.
  *)
  if Uwt_base.Sys_info.os = Uwt_base.Sys_info.Mac then (
    try
      let cb t _ =
        Uwt.Fs_event.close_noerr t
      in
      let fln = Filename.get_temp_dir_name () in
      let t = Uwt.Fs_event.start_exn fln [] ~cb in
      Uwt.Fs_event.close_noerr t
    with
    | Uwt.Uwt_error _ -> ()
  )

let l = [
  ("tmpdir watcher">::
   fun _ctx ->
     m_true (test ())); ]

let l = "Fs_event">:::l
