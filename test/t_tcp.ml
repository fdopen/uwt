open Lwt.Infix

let server_port = 9004
let server_ip = "127.0.0.1"
let server_backlog = 8

let server6_port = 9006
let server6_ip = "::1"

module U = Uwt
open Uwt.Tcp
open Common


module type Sockaddr =
sig
  val sockaddr : Uwt.sockaddr
end

let bind_exn s addr =
  if  Unix.PF_INET6 = (Uwt.Compat.to_unix_sockaddr addr
                       |> Unix.domain_of_sockaddr)
  then
    bind_exn ~mode:[ Ipv6_only ] s ~addr ()
  else
    bind_exn s ~addr ()

module Echo_server (X: Sockaddr) = struct
  let sockaddr = X.sockaddr

  let echo_client c =
    let buf = Uwt_bytes.create 65_536 in
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
    if Uwt.Int_result.is_error x then
      Uwt_io.printl "listen error" |> ignore
    else
      match accept server with
      | U.Error _ -> Uwt_io.printl "accept error" |> Lwt.ignore_result
      | U.Ok c -> echo_client c |> ignore

  let start () =
    let server = init () in
    try_finally ( fun () ->
        bind_exn server sockaddr;
        let sockaddr2 = getsockname_exn server in
        (* I'm sure about this test. Does the ocaml unix library
           support equality compare for the abstract type
           Unix.inet_addr ? *)
        if Uwt.Compat.to_unix_sockaddr sockaddr <>
           Uwt.Compat.to_unix_sockaddr sockaddr2 then
          failwith "server sockaddr differ";
        listen_exn server ~max:server_backlog ~cb:on_listen;
        let s,_ = Lwt.task () in
        s
      ) ( fun () -> close_noerr server ; Lwt.return_unit )
end

module Client = struct

  let test raw addr =
    let buf_write = Buffer.create 128 in
    let buf_read = Buffer.create 128 in
    let t = Uwt.Tcp.init () in
    Uwt.Tcp.connect t ~addr >>= fun () ->

    let rec really_read len =
      let buf = Bytes.create len in
      Uwt.Tcp.read t ~buf >>= fun len' ->
      if len' = 0 || len' > len then (
        Uwt.Tcp.close_noerr t ;
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

    let tcp_write = match raw with
    | true -> Uwt.Tcp.write_raw
    | false -> Uwt.Tcp.write
    in
    let rec write i =
      if i <= 0 then
        Lwt.return_unit
      else
        let buf_len = Random.int 934 + 1 in
        let buf = Bytes.init buf_len ( fun i -> Char.chr (i land 255) ) in
        Buffer.add_bytes buf_write buf;
        tcp_write t ~buf >>= fun () ->
        really_read buf_len >>= fun () ->
        write (pred i)
    in
    write 1024 >>= fun () ->
    Uwt.Tcp.close_wait t >>= fun () ->
    Lwt.return ((Buffer.contents buf_write) = (Buffer.contents buf_read))
end

module Server = Echo_server (
  struct
    let sockaddr = U.Misc.ip4_addr_exn server_ip server_port
  end)

module Server6 = Echo_server (
  struct
    let sockaddr = U.Misc.ip6_addr_exn server6_ip server6_port
  end)

let server_init = lazy (
  let server_thread = Server.start () in
  Uwt.Main.at_exit ( fun () -> Lwt.cancel server_thread; Lwt.return_unit ))

let server6_init = lazy (
  let server_thread = Server6.start () in
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
let test_port = 8931
let l = [
  ("echo_server">::
   fun ctx ->
     Lazy.force server_init |> ignore ;
     m_true ( Client.test true Server.sockaddr );
     m_true ( Client.test false Server.sockaddr );
     ip6_only ctx;
     Lazy.force server6_init |> ignore ;
     m_true ( Client.test true Server6.sockaddr );
     m_true ( Client.test false Server6.sockaddr ));
  ("connect_timeout">::
   fun _ctx -> (* an unreachable address should not block the event loop *)
     let l addr =
       let t = init () in
       try_finally ( fun () ->
           let p1 = connect t ~addr >>= fun () -> Lwt.return_false in
           let p2 = Uwt.Timer.sleep 50 >>= fun () -> Lwt.return_true in
           Lwt.pick [ p1 ; p2 ] )
         ( fun () -> close_noerr t ; Lwt.return_unit )
     in
     m_true (l (Uwt.Misc.ip4_addr_exn "8.8.8.8" 9999)));
  ("bind_error">::
   fun ctx ->
     let l sockaddr =
       let s1 = init () in
       let s2 = init () in
       try_finally ( fun () ->
           let cb _ _ = () in
           bind_exn s1 sockaddr;
           bind_exn s2 sockaddr;
           let () = listen_exn ~max:8 ~cb s1 in
           let () = listen_exn ~max:8 ~cb s2 in
           Lwt.return_unit
         ) ( fun () -> close_noerr s1 ; close_noerr s2 ; Lwt.return_unit )
     in
     let sockaddr = Uwt.Misc.ip4_addr_exn "0.0.0.0" test_port in
     m_raises (Uwt.EADDRINUSE,"uv_listen","") (l sockaddr);
     ip6_only ctx;
     let sockaddr = Uwt.Misc.ip6_addr_exn "::0" test_port in
     m_raises (Uwt.EADDRINUSE,"uv_listen","") (l sockaddr));
  ("write_allot">::
   fun ctx ->
     let l addr =
       let client = init () in
       try_finally ( fun () ->
           connect client ~addr >>= fun () ->
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
         ) ( fun () -> close_noerr client; Lwt.return_unit )
     in
     m_true (l Server.sockaddr);
     ip6_only ctx;
     m_true (l Server6.sockaddr));
  ("write_abort">::
   fun _ctx ->
     let client = init () in
     m_true (try_finally ( fun () ->
         Uwt.Tcp.connect client ~addr:Server.sockaddr >>= fun () ->
         let write_thread = write_much client in
         close_wait client >>= fun () ->
         Lwt.catch ( fun () -> write_thread )
           ( function
           | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
           | x -> Lwt.fail x )
       ) ( fun () -> close_noerr client; Lwt.return_unit ));
  );
  ("read_abort">::
   fun _ctx ->
     Lazy.force server_init |> ignore ;
     let client = init () in
     m_true (try_finally ( fun () ->
         Uwt.Tcp.connect client ~addr:Server.sockaddr >>= fun () ->
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
  ("getpeername">::
   fun _ctx ->
     Lazy.force server_init |> ignore ;
     let client = init () in
     m_true (try_finally ( fun () ->
         Uwt.Tcp.connect client ~addr:Server.sockaddr >>= fun () ->
         match Uwt.Tcp.getpeername_exn client |> Uwt.Compat.to_unix_sockaddr with
         | Unix.ADDR_INET(y,x) ->
           if server_port = x &&
              server_ip = Unix.string_of_inet_addr y
           then
             Lwt.return_true
           else
             Lwt.return_false
         | Unix.ADDR_UNIX _ -> Lwt.return_false
       ) ( fun () -> close_noerr client; Lwt.return_unit )));
  (* The following test the same as 'write_abort' above (regarding TCP).
     The intention is to ensure, that lwt behaves as expected *)
  ("write_abort_pick">::
   fun _ctx ->
     let client = init () in
     m_true (try_finally ( fun () ->
         Uwt.Tcp.connect client ~addr:Server.sockaddr >>= fun () ->
         let write_thread =
           Lwt.catch ( fun () ->
               write_much client
             ) ( fun x -> Lwt.pause () >>= fun () -> Lwt.fail x )
         in
         let close_thread = close_wait client >>= fun () -> Lwt.return_true in
         Lwt.pick [ close_thread ; write_thread ]
       ) ( fun () -> close_noerr client; Lwt.return_unit ));
  );
  ("write_abort_pick2">::
   fun _ctx ->
     Lazy.force server_init |> ignore ;
     let client = init () in
     m_true (try_finally ( fun () ->
         Uwt.Tcp.connect client ~addr:Server.sockaddr >>= fun () ->
         let write_thread =
           Lwt.catch ( fun () ->
               write_much client
             ) ( function
             | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
             | x -> Lwt.fail x )
         in
         let close_thread = close_wait client >>= fun () -> Lwt.return_false in
         Lwt.pick [ close_thread ; write_thread ]
       ) ( fun () -> close_noerr client; Lwt.return_unit ));
  );
]

let l  = "Tcp">:::l
