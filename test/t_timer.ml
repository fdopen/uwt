open OUnit2

let close () =
  ignore (T_tcp.close_servers ());
  ignore (T_pipe.close_server ())

let l = [
  ("Timer normal">::
   fun _ctx ->
     let sleeper,waker = Lwt.task () in
     let cnt = ref 3 in
     let cb t =
       decr cnt;
       if !cnt = 0 then (
         Uwt.Timer.close_noerr t;
         Lwt.wakeup waker true
       )
     in
     let t = Uwt.Timer.start_exn ~repeat:100 ~timeout:5 ~cb in
     Common.nm_try_finally ( fun s ->
         let erg = Uwt.Main.run s in
         assert_equal erg true ) sleeper
       ( fun t -> Uwt.Timer.close_noerr t ) t
  );
  ("Timer unref">::
   fun _ctx ->
     close ();
     let cb _ = () in
     let f () =
       let sleeper,_ = Lwt.task () in
       let t = Uwt.Timer.start_exn ~repeat:100 ~timeout:5 ~cb in
       Common.nm_try_finally ( fun s ->
           let () = Uwt.Timer.unref t in
           let erg = Uwt.Main.run s in
           assert_equal erg true ) sleeper
         ( fun t -> Uwt.Timer.close_noerr t ) t
     in
     let erg =
       try
         f ();
         false
       with
       | Uwt.Main.Main_error(Uwt.EOF,_) -> true
       | _ -> false
     in
     assert_bool "raises error" erg );
  ("Timer unref running">::
   fun _ctx ->
     close ();
     let cnt = ref 0 in
     let cb _ = incr cnt in
     let t = Uwt.Timer.start_exn ~repeat:100 ~timeout:5 ~cb in
     Common.nm_try_finally ( fun t ->
         let () = Uwt.Timer.unref t in
         Uwt.Main.run (Uwt.Timer.sleep 500)
       ) t
       ( fun t -> Uwt.Timer.close_noerr t ) t;
     (* there are no guarantees *)
     let ok = !cnt > 3 && !cnt < 7 in
     assert_bool "unref timer was active" ok);
]

let l = "Timer">:::l
