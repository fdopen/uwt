let () = Random.self_init ()

open Lwt.Infix
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
let chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
let chars_len = String.length chars

let write_something tmp_dir =
  let x = Bytes.of_string "uwt.fs_event.XXXXXX.log" in
  let rec iter n =
    Uwt.Timer.sleep (n * 1_000 )  >>= fun () ->
    for i = 13 to 18 do
      let c = Random.int chars_len in
      Bytes.set x i chars.[c]
    done;
    Lwt.catch ( fun () ->
        let fln = Filename.concat tmp_dir (Bytes.to_string x) in
        Uwt_io.printf "will write to %s\n" fln >>= fun () ->
        Uwt_io.flush Uwt_io.stdout >>= fun () ->
        let open Uwt.Fs in
        let open Uv.Fs in
        openfile ~mode:([O_WRONLY; O_CREAT; O_EXCL]) fln >>= fun fd ->
        write_string ~buf:"Hello World!" fd >>= fun _ ->
        close fd >>= fun () -> Uwt.Timer.sleep 1_000 >>= fun () ->
        Uwt.Fs.unlink fln >>= fun () -> Lwt.return_unit
      ) ( function
      | Uv.Uv_error(Uv.EEXIST,_,_) -> Lwt.return_unit
      | x -> Lwt.fail x )
    >>= fun () -> iter (succ n)
  in
  iter 0
