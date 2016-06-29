let () =
  Random.self_init ();
  if Sys.unix then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Uwt.Process.disable_stdio_inheritance ()

open OUnit2

let tests =
  "All">:::[
    T_io.l;
    T_gc.l;
    T_fs.l;
    T_unix.l;
    T_preemptive.l;
    T_tcp.l;
    T_spawn.l;
    T_fs_event.l;
    T_udp.l;
    T_pipe.l;
    T_dns.l;
    T_misc.l;
    T_signal.l;
    T_poll.l;
    T_fs_poll.l;
    T_tty.l;
    T_fs_sync.l;
    T_stub.l;
    T_preemptive.l; (* yes twice, it must be tested again after T_spawn *)
    T_timer.l;
  ]

exception Do_exit of int
let mexit i =
  if i <> 0 then (
    prerr_endline "test case failure";
  );
  raise (Do_exit i)

let main () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  ignore (OUnit2.run_test_tt_main ~exit:mexit tests)

let cleanup () =
  let open Lwt.Infix in
  let t =
    (* close callbacks are defered, remove them first *)
    Uwt.Main.yield () >>= fun () ->
    Uwt.Main.yield () >>= fun () ->
    let _  : unit Uwt.uv_result = T_fs_sync.with_file
        ~mode:Uwt.Fs.[O_CREAT;O_TRUNC;O_WRONLY]
        "p_end.log" @@ fun fd ->
      ignore (Uwt.Debug.print_all_handles fd);
      Ok ()
    in
    Lwt.return_unit
  in
  Uwt.Main.run t ;
  Uwt.Debug.valgrind_happy ()

let () =
  try
    main ();
    raise (Do_exit 0)
  with
  | Do_exit i ->
    (match Sys.getenv "WITH_VALGRIND" with
    | exception Not_found -> cleanup ()
    | "false" | "0" | "" | "FALSE" -> ()
    | _ -> cleanup () );
    exit i
  | exn ->
    Printexc.to_string exn |> prerr_endline ;
    exit 2
