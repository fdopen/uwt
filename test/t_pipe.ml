open Lwt.Infix
module U = Uwt
open Uwt.Pipe
open Common

module Echo_server = struct
  let addr =
    let name =
      Printf.sprintf "uwt_pipe_%d_%d" (Unix.getpid ()) (Unix.getuid ())
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
    Lwt.finalize ( fun () -> iter () )
      ( fun () -> close_noerr c ; Lwt.return_unit )

  let on_listen server x =
    if Uwt.Int_result.is_error x then
      ignore(Uwt_io.printl "listen error")
    else
      let client = init () in
      let t = accept_raw ~server ~client in
      if Uwt.Int_result.is_error t then
        ignore(Uwt_io.printl "accept error")
      else
        ignore(echo_client client)

  let start () =
    let server = init () in
    Lwt.finalize ( fun () ->
        bind_exn server ~path:addr;
        let addr2 = getsockname_exn server in
        if addr2 <> addr then
          failwith "pipe address differ";
        listen_exn server ~max:8 ~cb:on_listen;
        let (s:unit Lwt.t),_ = Lwt.task () in
        s
      ) ( fun () -> close_noerr server ; Lwt.return_unit )
end

module Client = struct
  let test raw =
    let buf_write = Buffer.create 128 in
    let buf_read = Buffer.create 128 in
    let t = init () in
    connect t ~path:Echo_server.addr >>= fun () ->

    let rec really_read len =
      let buf = Bytes.create len in
      read t ~buf >>= fun len' ->
      if len' = 0 || len' > len then
        let () = close_noerr t in
        Lwt.return_unit
      else
        let () = Buffer.add_subbytes buf_read buf 0 len' in
        let len'' = len - len' in
        if len'' = 0 then
          Lwt.return_unit
        else
          really_read len''
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
        let buf = rbytes_create buf_len in
        Buffer.add_bytes buf_write buf;
        pipe_write t ~buf >>= fun () ->
        really_read buf_len >>= fun () ->
        write (pred i)
    in
    write 1024 >>= fun () ->
    close_wait t >|= fun () ->
    Buffer.contents buf_write = Buffer.contents buf_read
end

let server_thread = ref None
let close_server () =
  (match !server_thread with
  | None -> ()
  | Some t ->
    server_thread := None;
    Lwt.cancel t);
  Lwt.return_unit

let server_init () =
  match !server_thread with
  | Some _ -> ()
  | None ->
    server_thread := Some( Echo_server.start () );
    Uwt.Main.at_exit close_server

let write_much client =
  let buf = rba_create 32_768 in
  let rec iter n =
    if n = 0 then
      write_ba client ~buf >>= fun () ->
      Lwt.fail (Failure "everything written!")
    else (
      ignore(write_ba client ~buf);
      iter (pred n)
    )
  in
  iter 100

let with_client f =
  server_init ();
  with_pipe f

open OUnit2
let l = [
  ("echo_server">::
   fun _ctx ->
     server_init ();
     m_true ( Uwt.Main.yield () >|= fun () -> true );
     m_true ( Client.test true );
     m_true ( Client.test false ));
  ("write_allot">::
   fun ctx ->
     server_init ();
     let t = with_client @@ fun client ->
       connect client ~path:Echo_server.addr >>= fun () ->
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
       close_wait client >|= fun () ->
       !bytes_read = !bytes_written &&
       !bytes_read = buf_len * buf_cnt
     in
     m_true t);
  ("fileno">::
   fun _ctx ->
     let (fd1,fd2) = Unix.pipe () in
     let conv x = match Uwt.Conv.file_of_file_descr x with
     | None -> assert false
     | Some t -> t
     in
     let file1 = conv fd1
     and file2 = conv fd2 in
     let p1 = Uwt.Pipe.openpipe_exn fd1
     and p2 = Uwt.Pipe.openpipe_exn fd2 in
     let fd1' = Uwt.Pipe.fileno_exn p1
     and fd2' = Uwt.Pipe.fileno_exn p2 in
     let file1' = conv fd1'
     and file2' = conv fd2' in
     assert_equal fd1 fd1';
     assert_equal fd2 fd2';
     assert_equal file1 file1';
     assert_equal file2 file2';
     assert_equal false (file1 = file2);
     Uwt.Pipe.close_noerr p1;
     Uwt.Pipe.close_noerr p2;
     let open U in
     let is_error = match Uwt.Pipe.fileno p1 with
     | Error(EBADF|UWT_EBADF) -> true
     | Ok _ | Error _ -> false
     in
     assert_equal true is_error);
  ("write_abort">::
   fun ctx ->
     no_win_xp ctx; (* no, i won't debug obsolete systems ... *)
     let t = with_client @@ fun client ->
       connect client ~path:Echo_server.addr >>= fun () ->
       let write_thread = write_much client in
       close_wait client >>= fun () ->
       Lwt.catch ( fun () -> write_thread )
         (function
         | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
         | x -> Lwt.fail x)
     in
     m_true t);
  ("write_cancel">::
   fun _ctx ->
     let t = with_client @@ fun client ->
       connect client ~path:Echo_server.addr >>= fun () ->
       let buf = rba_create 1_299_827 in
       let t = write_ba client ~buf in
       let _ : unit Lwt.t = Lwt.pause () >|= fun () -> Lwt.cancel t in
       t
     in
     assert_canceled t);
  ("read_abort">::
   fun _ctx ->
     let t = with_client @@ fun client ->
       connect client ~path:Echo_server.addr >>= fun () ->
       let read_thread =
         let buf = Bytes.create 128 in
         read client ~buf >>= fun _ ->
         Lwt.fail (Failure "read successful!")
       in
       let _ : unit Lwt.t =
         Uwt.Timer.sleep 40 >>= fun () ->
         close_noerr client ; Lwt.return_unit
       in
       Lwt.catch ( fun () -> read_thread )(function
         | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
         | x -> Lwt.fail x )
     in
     m_true t);
  ("echo_pipe_uwt_io">::
   fun _ctx ->
     let path =
       let s =
         Printf.sprintf "uwt_pipe2_%d_%d" (Unix.getpid ()) (Unix.getuid ())
       in
       match Sys.win32 with
       | true -> "\\\\?\\pipe\\" ^ s
       | false -> Filename.concat (Filename.get_temp_dir_name ()) s
     in
     let addr = Unix.ADDR_UNIX path in
     let server = Uwt_io.establish_server addr @@ fun (ic,oc) ->
       let rec iter () =
         Uwt_io.read_char ic >>= fun s ->
         Uwt_io.write_char oc s >>= fun () ->
         iter ()
       in
       let t = Lwt.finalize iter (fun () ->
           Uwt_io.close ic >>= fun () -> Uwt_io.close oc )
       in
       ignore t
     in
     let t =
       Lwt.finalize ( fun () ->
           with_client @@ fun client ->
           connect client ~path >>= fun () ->
           let rc = Uwt_io.of_pipe ~mode:Uwt_io.input client
           and wc = Uwt_io.of_pipe ~mode:Uwt_io.output client in
           let count = Random.int 8192 in
           let rec t1 n =
             if n = count then
               Lwt.return_unit
             else
               let char = Char.chr @@ n land 255 in
               Uwt_io.write_char wc char >>= fun () ->
               t1 (succ n)
           and t2 n =
             if n = count then
               Lwt.return_unit
             else
               Uwt_io.read_char rc >>= fun char ->
               let char' = Char.chr @@ n land 255 in
               assert (char' = char);
               t2 (succ n)
           in
           Lwt.join [t1 0; t2 0] >>= fun () -> Lwt.return_true )
         ( fun () -> Uwt_io.shutdown_server server ; Lwt.return_unit)
     in
     m_true t);
]

let l  = "Pipe">:::l
