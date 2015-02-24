open Lwt.Infix

let server_port = 9009
let server_ip4 = "127.0.0.1"
let server_backlog = 32
let client_buf_size = 1024

module U = Uwt
module T = U.Tcp

module Server = struct
  let echo_client c =
    let buf = Bytes.create client_buf_size in
    let rec iter () =
      T.read ~buf c >>= function
      | 0 -> Lwt.return_unit
      | len ->
        T.write ~buf ~len c >>= fun () ->
        iter ()
    in
    Lwt.catch ( fun () -> iter () >>= fun () -> T.close c )
      ( fun x -> T.close_noerr c ; Lwt.fail x )

  let on_listen server x =
    if Uwt.Result.is_error x then
      Uwt_io.printl "listen error" |> ignore
    else
      match T.accept server with
      | U.Error _ -> Uwt_io.printl "accept error" |> Lwt.ignore_result
      | U.Ok c -> echo_client c |> ignore

  let start () =
    let server = T.init_exn () in
    Lwt.catch ( fun () ->
        let sock = U.Misc.ip4_addr_exn server_ip4 server_port in
        T.bind_exn server sock;
        let () = T.listen_exn server ~back:server_backlog ~cb:on_listen in
        Uwt.Timer.sleep 900_000 >>= fun () -> T.close server >>= fun ()->
        Lwt.return_false
      ) ( fun exn -> T.close_noerr server ; Lwt.fail exn )
end

module Client = struct

  let test () =
    Uwt.Timer.sleep 500 >>= fun () -> (* let the server start *)
    let buf_write = Buffer.create 128 in
    let buf_read = Buffer.create 128 in
    let t = Uwt.Tcp.init_exn () in
    Uwt.Tcp.connect t (Uwt.Misc.ip4_addr_exn server_ip4 server_port) >>= fun ()->

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
        let buf = Bytes.init buf_len ( fun _i -> Random.int 256 |> Char.chr ) in
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
open Common

let l = [
  ("echo_server">::
   fun _ctx ->
       m_true (Lwt.pick [ Server.start () ; Client.test () ]))
]

let l  = "Tcp">:::l
