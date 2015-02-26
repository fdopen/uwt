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

let bind_exn s sockaddr =
  if  Unix.PF_INET6 = (Uwt.Compat.to_unix_sockaddr sockaddr
                       |> Unix.domain_of_sockaddr)
  then
    bind_exn ~mode:[ Ipv6_only ] s sockaddr
  else
    bind_exn s sockaddr

module Echo_server (X: Sockaddr) = struct
  let sockaddr = X.sockaddr

  let echo_client c =
    let buf = Uwt_bytes.create 65_536 in
    let rec iter () =
      read_ba ~buf c >>= function
      | 0 -> close c
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
      match accept server with
      | U.Error _ -> Uwt_io.printl "accept error" |> Lwt.ignore_result
      | U.Ok c -> echo_client c |> ignore

  let start () =
    let server = init_exn () in
    try_finally ( fun () ->
        bind_exn server sockaddr;
        listen_exn server ~back:server_backlog ~cb:on_listen;
        let s,_ = Lwt.task () in
        s
      ) ( fun () -> close_noerr server ; Lwt.return_unit )
end

module Client = struct

  let test addr =
    let buf_write = Buffer.create 128 in
    let buf_read = Buffer.create 128 in
    let t = Uwt.Tcp.init_exn () in
    Uwt.Tcp.connect t addr >>= fun () ->

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

    let rec write i =
      if i <= 0 then
        Lwt.return_unit
      else
        let buf_len = Random.int 934 + 1 in
        let buf = Bytes.init buf_len ( fun i -> Char.chr (i land 255) ) in
        Buffer.add_bytes buf_write buf;
        Uwt.Tcp.write t ~buf >>= fun () ->
        really_read buf_len >>= fun () ->
        write (pred i)
    in
    write 1024 >>= fun () ->
    Uwt.Tcp.close t >>= fun () ->
    Lwt.return ((Buffer.contents buf_write) = (Buffer.contents buf_read))
end

open OUnit2


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

let has_ip6 =
  Uwt.Misc.interface_addresses_exn () |> Array.to_list |>
  List.exists ( fun x ->
      Uwt.Compat.to_unix_sockaddr x.Uwt.Misc.address
      |> Unix.domain_of_sockaddr = Unix.PF_INET )

let ip6_only () = skip_if (not has_ip6) "no ip6"


let test_port = 8931

let l = [
  ("echo_server">::
   fun _ctx ->
     Lazy.force server_init |> ignore ;
     m_true ( Client.test Server.sockaddr );
     ip6_only ();
     Lazy.force server6_init |> ignore ;
     m_true ( Client.test Server6.sockaddr ));
  ("connect_timeout">::
   fun _ctx -> (* an unreachable address should not block the event loop *)
     let l sockaddr =
       let t = init_exn () in
       try_finally ( fun () ->
           let p1 = connect t sockaddr >>= fun () -> Lwt.return_false in
           let p2 = Uwt.Timer.sleep 50 >>= fun () -> Lwt.return_true in
           Lwt.pick [ p1 ; p2 ] )
         ( fun () -> close_noerr t ; Lwt.return_unit )
     in
     m_true (l (Uwt.Misc.ip4_addr_exn "8.8.8.8" 9999)));
  ("bind_error">::
   fun _ctx ->
     let l sockaddr =
       let s1 = init_exn () in
       let s2 = init_exn () in
       try_finally ( fun () ->
           let cb _ _ = () in
           bind_exn s1 sockaddr;
           bind_exn s2 sockaddr;
           let () = listen_exn ~back:8 ~cb s1 in
           let () = listen_exn ~back:8 ~cb s2 in
           Lwt.return_unit
         ) ( fun () -> close_noerr s1 ; close_noerr s2 ; Lwt.return_unit )
     in
     let sockaddr = Uwt.Misc.ip4_addr_exn "0.0.0.0" test_port in
     m_raises (Uwt.EADDRINUSE,"uv_listen","") (l sockaddr);
     ip6_only ();
     let sockaddr = Uwt.Misc.ip6_addr_exn "::0" test_port in
     m_raises (Uwt.EADDRINUSE,"uv_listen","") (l sockaddr));
  ("write_allot">::
   fun _ctx ->
     let l addr =
       let client = init_exn () in
       try_finally ( fun () ->
           connect client addr >>= fun () ->
           let buf_len = 65536 in
           let buf_cnt = 4096 in
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
           close client >>= fun () ->
           let success =
             !bytes_read = !bytes_written &&
             !bytes_read = buf_len * buf_cnt
           in
           Lwt.return success
         ) ( fun () -> close_noerr client; Lwt.return_unit )
     in
     m_true (l Server.sockaddr);
     ip6_only ();
     m_true (l Server6.sockaddr));
]

let l  = "Tcp">:::l
