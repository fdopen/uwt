let () = Random.self_init ()
let () = if Sys.unix then Sys.set_signal Sys.sigpipe Sys.Signal_ignore

open OUnit2

let tests =
  "All">:::[
    T_fs.l;
    T_unix.l;
    T_preemptive.l;
    T_tcp.l;
    T_spawn.l;
    T_fs_event.l;
    T_udp.l;
    T_compat.l;
    T_pipe.l;
    T_dns.l;
    T_misc.l;
    T_signal.l;
  ]

exception Do_exit of int
let mexit i =
  if i <> 0 then (
    prerr_endline "test case failure";
  );
  raise (Do_exit i)

let main () =
  Unix.putenv "OUNIT_RUNNER" "sequential";
  OUnit2.run_test_tt_main ~exit:mexit tests |> ignore

let () =
  try
    main ();
    raise (Do_exit 0)
  with
  | Do_exit i ->
    (match Sys.getenv "WITH_VALGRIND" with
    | "false" | "0" | "" | "FALSE" -> ()
    | _ -> Uwt.valgrind_happy ()
    | exception Not_found -> Uwt.valgrind_happy ());
    exit i
  | exn ->
    Printexc.to_string exn |> prerr_endline ;
    exit 2
