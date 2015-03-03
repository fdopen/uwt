open Lwt.Infix
module U = Uwt
open Uwt.Pipe
open Common

module Echo_server = struct

  let addr =
    let name =
      Printf.sprintf "uwt_pipt_%d_%d" (Unix.getpid ()) (Unix.getuid ())
    in
    match Sys.win32 with
    | true -> "\\\\?\\pipe\\" ^ name
    | false -> Filename.concat (Filename.get_temp_dir_name ()) name

  let echo_client c =
    let buf = Uwt_bytes.create 4096 in
    let rec iter () =
      read_ba ~buf c >>= function
      | 0 -> close_wait c
      | len ->
        write_ba ~buf ~len c >>= fun () ->
        iter ()
    in
    try_finally ( fun () -> iter () )
      ( fun () -> close_noerr c ; Lwt.return_unit )

  let on_listen server x =
    if Uwt.Result.is_error x then
      Uwt_io.printl "listen error" |> ignore
    else
      let client = init_exn () in
      let t = accept_raw ~server ~client in
      if Uwt.Result.is_error t then
        Uwt_io.printl "accept error" |> ignore
      else
        echo_client client |> ignore

  let start () =
    let server = init_exn () in
    try_finally ( fun () ->
        bind_exn server addr;
        let addr2 = getsockname_exn server in
        if addr2 <> addr then
          failwith "pipe address differ";
        listen_exn server ~back:8 ~cb:on_listen;
        let s,_ = Lwt.task () in
        s
      ) ( fun () -> close_noerr server ; Lwt.return_unit )
end


module Client = struct

  let test raw =
    let buf_write = Buffer.create 128 in
    let buf_read = Buffer.create 128 in
    let t = init_exn () in
    connect t Echo_server.addr >>= fun () ->

    let rec really_read len =
      let buf = Bytes.create len in
      read t ~buf >>= fun len' ->
      if len' = 0 || len' > len then (
        close_noerr t ;
        Lwt.return_unit
      )
      else (
        Buffer.add_subbytes buf_read buf 0 len';
        let len'' = len - len' in
        if len'' = 0 then
          Lwt.return_unit
        else
          really_read len''
      )
    in

    let pipe_write = match raw with
    | true -> write_raw
    | false -> write
    in
    let rec write i =
      if i <= 0 then
        Lwt.return_unit
      else
        let buf_len = Random.int 934 + 1 in
        let buf = Bytes.init buf_len ( fun i -> Char.chr (i land 255) ) in
        Buffer.add_bytes buf_write buf;
        pipe_write t ~buf >>= fun () ->
        really_read buf_len >>= fun () ->
        write (pred i)
    in
    write 1024 >>= fun () ->
    close_wait t >>= fun () ->
    Lwt.return ((Buffer.contents buf_write) = (Buffer.contents buf_read))
end

let server_init = lazy (
  let server_thread = Echo_server.start () in
  Uwt.Main.at_exit ( fun () -> Lwt.cancel server_thread; Lwt.return_unit ))


let write_much client =
  let buf = Uwt_bytes.create 32768 in
  for i = 0 to pred 32768 do
    Uwt_bytes.set buf i (Char.chr (i land 255))
  done;
  let rec iter n =
    if n = 0 then
      write_ba client ~buf >>= fun () ->
      Lwt.fail (Failure "everything written!")
    else (
      write_ba client ~buf |> ignore;
      iter (pred n)
    )
  in
  iter 100

open OUnit2
let l = [
  ("echo_server">::
   fun _ctx ->
     Lazy.force server_init |> ignore ;
     m_true ( Uwt.Main.yield () >>= fun () -> Lwt.return_true );
     m_true ( Client.test true );
     m_true ( Client.test false ));
  ("write_allot">::
   fun ctx ->
     let l () =
       let client = init_exn () in
       try_finally ( fun () ->
           connect client Echo_server.addr >>= fun () ->
           let buf_len = 65536 in
           let x = max 1 (multiplicand ctx) in
           let buf_cnt = 64 * x in
           let bytes_read = ref 0 in
           let bytes_written = ref 0 in
           let buf = Uwt_bytes.create buf_len in
           for i = 0 to pred buf_len do
             buf.{i} <- Char.chr (i land 255);
           done;
           let sleeper,waker = Lwt.task () in
           let cb_read = function
           | Uwt.Ok b ->
             for i = 0 to Bytes.length b - 1 do
               if Bytes.unsafe_get b i <> Char.chr (!bytes_read land 255) then
                 Lwt.wakeup_exn waker (Failure "read wrong content");
               incr bytes_read;
             done
           | Uwt.Error Uwt.EOF -> Lwt.wakeup waker ()
           | Uwt.Error _ -> Lwt.wakeup_exn waker (Failure "fatal error!")
           in
           let cb_write () =
             bytes_written := buf_len + !bytes_written;
             Lwt.return_unit
           in
           for _i = 1 to buf_cnt do
             ignore ( write_ba client ~buf >>= cb_write );
           done;
           if write_queue_size client = 0 then
             Lwt.wakeup_exn waker
               (Failure "write queue size empty after write requests");
           read_start_exn client ~cb:cb_read;
           let t_shutdown = shutdown client >>= fun () ->
             if write_queue_size client <> 0 then
               Lwt.fail (Failure "write queue size not empty after shutdown")
             else
               Lwt.return_unit
           in
           Lwt.join [ t_shutdown ; sleeper ] >>= fun () ->
           close_wait client >>= fun () ->
           let success =
             !bytes_read = !bytes_written &&
             !bytes_read = buf_len * buf_cnt
           in
           Lwt.return success
         ) ( fun () -> close_noerr client; Lwt.return_unit)
     in
     m_true (l ()));
  ("write_abort">::
   fun _ctx ->
     let client = init_exn () in
     m_true (try_finally ( fun () ->
         connect client Echo_server.addr >>= fun () ->
         let write_thread = write_much client in
         close_wait client >>= fun () ->
         Lwt.catch ( fun () -> write_thread )
           ( function
           | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
           | x -> Lwt.fail x )
       ) ( fun () -> close_noerr client; Lwt.return_unit )));
  ("read_abort">::
   fun _ctx ->
     let client = init_exn () in
     m_true (try_finally ( fun () ->
         connect client Echo_server.addr >>= fun () ->
         let read_thread =
           let buf = Bytes.create 128 in
           read client ~buf >>= fun _ ->
           Lwt.fail (Failure "read successful!")
         in
         let _ =
           Uwt.Timer.sleep 40 >>= fun () ->
           close_noerr client ; Lwt.return_unit
         in
         Lwt.catch ( fun () -> read_thread )(function
           | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
           | x -> Lwt.fail x )
       ) ( fun () -> close_noerr client; Lwt.return_unit )));
]

let l  = "Pipe">:::l
