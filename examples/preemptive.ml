open Lwt.Infix

let p_buf = Bytes.create 2

let () =
  let rec iter_main i =
    if i > 5 then
      Lwt.return_unit
    else
      Uwt.Timer.sleep 1_000 >>= fun () ->
      Uwt_io.printf "'Hello World' from main (%d)\n" i >>= fun () ->
      iter_main (succ i)
  in
  let rec iter_detached tn i =
    if i > 5 then
      ()
    else
      let () = Unix.sleep 1 in
      Printf.printf "'Hello World' from detached thread (%d:%d)\n%!" tn i;
      let s =
        Printf.sprintf
          "'Hello World' from detached, but executed in main (%d:%d)"
          tn i;
      in
      let () = Uwt_preemptive.run_in_main ( fun () -> Uwt_io.printl s) in
      iter_detached tn (succ i)
  in
  Uwt_preemptive.simple_init ();
  Uwt_preemptive.set_bounds (0,2);
  let a i =
    Uwt_preemptive.detach (iter_detached i) 0
  in
  let b = iter_main 0 in
  Uwt.Main.run (Lwt.join [a 1; a 2 ; a 3 ; b]);
  Unix.sleep 1;
  let b = iter_main 0 in
  let end' =
    Lwt.join [a 1;b] >>= fun () ->
    Uwt_io.flush_all ()
  in
  Uwt.Main.run end'


let () = Uwt.Debug.valgrind_happy ()
