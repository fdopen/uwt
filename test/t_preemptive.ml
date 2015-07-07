open OUnit2
open Lwt.Infix

let l _ctx =
  let t =
    let rec minisleep ms =
      let sec = (float_of_int ms) /. 1000. in
      try
        ignore (Unix.select [] [] [] sec)
      with
      | Unix.Unix_error(Unix.EINTR,_,_) -> minisleep ms
    in
    let sleep_time = 25 in
    let cnt = ref 0 in
    let cnt_mutex = Mutex.create () in
    let incr_cnt () =
      Mutex.lock cnt_mutex;
      incr cnt;
      Mutex.unlock cnt_mutex
    in
    let deferred_found = ref false in
    let first_iteration = ref true in
    let rec iter_main i =
      if i > 4 then
        Lwt.return_unit
      else
        Uwt.Timer.sleep sleep_time >>= fun () ->
        incr_cnt ();
      iter_main (succ i)
    in
    let rec iter_detached tn i =
      if i = 0 && !first_iteration &&
         !deferred_found = false && !cnt > 20 then
        deferred_found:= true ;
      if i > 4 then
        ()
      else
        let () = minisleep sleep_time in
        let () =
          Uwt_preemptive.run_in_main ( fun () -> incr_cnt (); Lwt.return_unit )
        in
        incr_cnt ();
        iter_detached tn (succ i)
    in
    Uwt_preemptive.simple_init ();
    Uwt_preemptive.set_bounds (0,2);
    let a i = Uwt_preemptive.detach (iter_detached i) 0 in
    let b = iter_main 0 in
    Uwt.Main.run (Lwt.join [a 1; a 2 ; a 3 ; b]);
    first_iteration:= false;
    if !deferred_found = false then (
      prerr_endline "no thread deferred\n";
      cnt := min_int
    );
    minisleep sleep_time;
    let b = iter_main 0 in
    let end' = Lwt.join [a 1; b ] in
    Uwt.Main.run end';
    !cnt
  in
  assert_equal 50 t

let l =  "preemptive_test">:: l
