open Lwt.Infix

let server_port = 9008
let server_ip = "127.0.0.1"
let server_backlog = 8

let server6_port = 9010
let server6_ip = "::1"

open Uwt.Udp
open Common

let bind_exn s sockaddr =
  if  Unix.PF_INET6 = (Uwt.Conv.unix_sockaddr_of_sockaddr sockaddr
                       |> Unix.domain_of_sockaddr)
  then
    bind_exn ~mode:[ Ipv6_only; Reuse_addr ] s ~addr:sockaddr ()
  else
    bind_exn ~mode:[ Reuse_addr ] s ~addr:sockaddr ()

let start_iter_server_bytes addr =
  let server = Uwt.Udp.init () in
  try_finally ( fun () ->
      let () = bind_exn server addr in
      let buf = Bytes.create 65536 in
      let rec iter () =
        recv ~buf server >>= fun x ->
        if x.recv_len > 0 then (
          match x.sockaddr with
          | None -> Lwt.return_false
          | Some sockaddr ->
            let b = Bytes.sub buf 0 x.recv_len in
            send server ~buf:b sockaddr |> ignore;
            iter ()
        )
        else
          Lwt.return_false
      in
      iter ()
    ) ( fun () -> Uwt.Udp.close_wait server )

let start_iter_server_ba addr =
  let server = Uwt.Udp.init () in
  try_finally ( fun () ->
      let () = bind_exn server addr in
      let buf = Uwt_bytes.create 65536 in
      let rec iter () =
        recv_ba ~buf server >>= fun x ->
        if x.recv_len > 0 then (
          match x.sockaddr with
          | None -> Lwt.return_false
          | Some sockaddr ->
            let b = Uwt_bytes.extract buf 0 x.recv_len in
            send_ba server ~buf:b sockaddr |> ignore;
            iter ()
        )
        else
          Lwt.return_false
      in
      iter ()
    ) ( fun () -> Uwt.Udp.close_wait server )

let start_server_cb addr : bool Lwt.t =
  let server = Uwt.Udp.init () in
  let sleeper,waker = Lwt.task () in
  let e s = Lwt.wakeup_exn waker (Failure s) in
  let cb = function
  | Uwt.Udp.Data (_,None) -> e "no sockaddr"
  | Uwt.Udp.Partial_data(_,_) -> e "partial data"
  | Uwt.Udp.Empty_from _ -> e "empty datagram"
  | Uwt.Udp.Transmission_error _ -> e "transmission error"
  | Uwt.Udp.Data(b,Some x) -> Uwt.Udp.send server ~buf:b x |> ignore
  in
  try_finally ( fun () ->
      let () = bind_exn server addr in
      let addr2 = Uwt.Udp.getsockname_exn server in
      if Uwt.Conv.unix_sockaddr_of_sockaddr addr <>
         Uwt.Conv.unix_sockaddr_of_sockaddr addr2 then
        failwith "udp server sockaddr differ";
      let () = Uwt.Udp.recv_start_exn server ~cb in
      sleeper
    ) ( fun () -> Uwt.Udp.close_wait server )

let start_client ~raw ~iter ~length addr =
  let client = Uwt.Udp.init () in
  let buf = Bytes.create length in
  let buf_recv = Bytes.create length in
  for i = 0 to pred length do
    Bytes.set buf i (Char.chr (i land 255))
  done;
  let send = match raw with
  | true -> send_raw
  | false -> send
  in
  let rec f n =
    if n = 0 then
      Lwt.return_true
    else (
      send client ~buf addr >>= fun () ->
      recv client ~buf:buf_recv >>= fun x ->
      if x.recv_len < length || x.is_partial || x.sockaddr = None ||
         buf <> buf_recv
      then
        Lwt.return_false
      else
        f (pred n)
    )
  in
  try_finally ( fun () -> f iter ) ( fun () -> close_wait client )

let sockaddr4 = Uwt_base.Misc.ip4_addr_exn server_ip server_port
let sockaddr6 = Uwt_base.Misc.ip6_addr_exn server6_ip server6_port

open OUnit2
let l = [
  ("echo_server">::
   fun ctx ->
     let f addr =
       let f raw =
         let f server =
           m_true (
             let server = server addr in
             let client = start_client ~raw ~iter:1_000 ~length:10 addr in
             Lwt.pick [ server ; client ]);
           m_true (
             let server = server addr in
             let client = start_client ~raw ~iter:100 ~length:999 addr in
             Lwt.pick [ server ; client ]);
           m_raises (Uwt.EMSGSIZE,"uv_udp_send","") (
             let server = server addr in
             let client = start_client ~raw ~iter:2 ~length:65536 addr in
             Lwt.pick [ server ; client ]);
         in
         f start_iter_server_bytes;
         f start_iter_server_ba;
         f start_server_cb
       in
       f true;
       f false;
     in
     f sockaddr4;
     ip6_only ctx;
     f sockaddr6);
  ("read_abort">::
   fun _ctx ->
     let server = start_iter_server_ba sockaddr4 in
     let client = init () in
     m_true (try_finally ( fun () ->
         send_raw_string client ~buf:"PING" sockaddr4 >>= fun () ->
         let read_thread =
           let buf = Bytes.create 128 in
           recv ~buf client >>= fun _ ->
           Lwt.fail (Failure ("read should have been aborted"));
         in
         close_noerr client;
         Lwt.catch ( fun () -> read_thread ) (function
           | Uwt.Uwt_error(Uwt.ECANCELED,_,_) -> Lwt.return_true
           | x -> Lwt.fail x )
       ) ( fun () ->
         close_noerr client;
         Lwt.cancel server;
         Lwt.return_unit )));
  ("write_allot">::
   fun ctx ->
     is_contingent ctx;
     let l addr : bool Lwt.t =
       let server = start_iter_server_ba addr in
       let client = init () in
       try_finally ( fun () ->
           let buf_len = 1024 in
           let x = max 1 (multiplicand ctx) in
           let buf_cnt = 4096 * x in
           let bytes_read = ref 0 in
           let buf = Uwt_bytes.create buf_len in
           for i = 0 to pred buf_len do
             buf.{i} <- Char.chr (i land 255);
           done;
           let sleeper,waker = Lwt.task () in
           let e s =
             Lwt.wakeup_exn waker (Failure s);
             close_noerr client
           in
           let cb_read = function
           | Uwt.Udp.Data (_,None) -> e "no sockaddr"
           | Uwt.Udp.Partial_data(_,_) -> e "partial data"
           | Uwt.Udp.Empty_from _ -> e "empty datagram"
           | Uwt.Udp.Transmission_error _ -> e "transmission error"
           | Uwt.Udp.Data(b,Some _) ->
             for i = 0 to Bytes.length b - 1 do
               if Bytes.unsafe_get b i <> Char.chr (!bytes_read land 255) then
                 e "read wrong content";
               incr bytes_read;
             done
           in
           let rec cb_write i started =
             if i = 0 then
               Uwt.Timer.sleep 100 >>= fun () -> Lwt.return_unit
             else
               send_ba ~buf client addr >>= fun () ->
               if started = false then
                 recv_start_exn client ~cb:cb_read;
               (* TODO: udp is unreliable and this test might fail. Make this
                  and similar tests optional. *)
               Uwt.Main.yield () >>= fun () ->
               cb_write (pred i) true
           in
           Lwt.pick [ cb_write buf_cnt false ; sleeper ] >>= fun () ->
           if !bytes_read = buf_len * buf_cnt then Lwt.return_true
           else Lwt.return_false
         ) ( fun () -> close_noerr client; Lwt.cancel server; Lwt.return_unit )
     in
     m_true (l sockaddr4);
     ip6_only ctx;
     m_true (l sockaddr6));
]

let l  = "Udp">:::l
