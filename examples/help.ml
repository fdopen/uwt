let timeout =
  let rec f = function
  | [] -> None
  | "-t"::((snd::_) as tl)->
    (match int_of_string snd with
     | x -> Some( x * 1000 )
     | exception(Failure _) -> f tl)
  | _::tl -> f tl
  in
  Array.to_list Sys.argv |> f

let wait () =
  let signals = [ Sys.sigint; Sys.sigterm ] in
  let sleeper,waker = Lwt.task () in
  let wake_once = lazy (Lwt.wakeup waker () ) in
  let cb _sig _i  = Lazy.force wake_once  in
  let l = List.map ( fun s -> Uwt.Signal.start_exn s ~cb ) signals in
  let close_all () = List.iter Uwt.Signal.close_noerr l; Lwt.return_unit in
  let ts = match timeout with
  | None -> [ sleeper ]
  | Some x ->  [ sleeper ; Uwt.Timer.sleep x ]
  in
  Lwt.finalize
    ( fun () -> Lwt.pick ts )
    ( fun () -> close_all () )


let clean () = Uwt.valgrind_happy ()
