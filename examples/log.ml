open Lwt.Infix

let section =
  let s = Uwt_log.Section.make "uwt" in
  Uwt_log.Section.set_level s Lwt_log.Info;
  s

let init_logging () =
  let id = Unix.getpid () in
  let file_name =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("uwt-" ^ string_of_int id ^ ".log")
  in
  let template =
    "$(date).$(milliseconds) ($(pid)|$(section)|$(level)) $(message)"
  in
  let chan_log =
    Uwt_log.channel ~template ~close_mode:`Keep ~channel:Uwt_io.stdout ()
  in
  Uwt_log.file ~template ~mode:`Truncate ~file_name ()
  >>= fun file_log ->
  (* Not useful, not portable, but it works :)
   let sys_log = Uwt_log.syslog ~facility:`Auth () in
  *)
  Uwt_log.default := Uwt_log.broadcast [chan_log; file_log];
  Uwt_log.ign_info_f ~section "logging to %s" file_name;
  Lwt.return_unit

let flood_log () =
  let i = ref 20 in
  let sleeper,waker = Lwt.task () in
  let cb t =
    let ic = !i in
    Uwt_log.ign_info_f ~section "countdown: %d" ic ;
    decr i;
    if ic = 0 then (
      Uwt.Timer.close_noerr t;
      Lwt.wakeup waker ()
    )
  in
  let _ = Uwt.Timer.start_exn ~repeat:200  ~timeout:200 ~cb in
  sleeper

let () =
  let t = init_logging () >>= fun () -> flood_log () in
  Uwt.Main.run t
