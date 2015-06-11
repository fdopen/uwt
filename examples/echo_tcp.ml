open Lwt.Infix

let server_port = 9009
let server_ip4 = "127.0.0.1"
let server_backlog = 32
let client_buf_size = 1024

module U = Uwt
module T = U.Tcp

let echo_client c =
  let buf = Bytes.create client_buf_size in
  let rec iter () =
    T.read ~buf c >>= function
    | 0 -> Lwt.return_unit
    | len ->
      T.write ~buf ~len c >>= fun () ->
      iter ()
  in
  Lwt.finalize iter ( fun () -> T.close_noerr c; Lwt.return_unit )

let on_listen server x =
  if Uwt.Int_result.is_error x then
    Uwt_io.printl "listen error" |> ignore
  else
    match T.accept server with
    | Uwt.Error _ -> Uwt_io.printl "accept error" |> Lwt.ignore_result
    | Uwt.Ok c -> echo_client c |> ignore

let echo_server () =
  let server = T.init () in
  Lwt.finalize ( fun () ->
      let addr = Uwt.Misc.ip4_addr_exn server_ip4 server_port in
      T.bind_exn server ~addr ();
      let () = T.listen_exn server ~max:server_backlog ~cb:on_listen in
      Help.wait ()
    ) ( fun () -> T.close_noerr server; Lwt.return_unit )

let () = U.Main.run (echo_server ())

let () = Help.clean ()
