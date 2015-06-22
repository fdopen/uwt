open Lwt.Infix

let server_port = 9009
let server_ip4 = "127.0.0.1"
let server_backlog = 32
let client_buf_size = 1024

module U = Uwt
module T = U.Tcp

let message = "<html><body><h1>Hello World</h1></html>"
let message = Bytes.of_string (
    "HTTP/1.0 200 OK\r\n"	^
    "Content-type: text/html\r\n"	^
    "Content-length: " ^ (String.length message |> string_of_int) ^ "\r\n"  ^
    "\r\n" ^
    message )

let b_len = 512
let b = Bytes.create 512
let output_dummy c =
  Lwt.finalize ( fun () ->
      T.read ~buf:b c >>= fun _n ->
      T.write c ~buf:message
    ) ( fun () -> T.close_noerr c ; Lwt.return_unit )

let on_listen server x =
  if Uwt.Int_result.is_error x then
    ignore(Uwt_io.printl "listen error")
  else
    match T.accept server with
    | Uwt.Error _ -> ignore (Uwt_io.printl "accept error")
    | Uwt.Ok c -> ignore (output_dummy c)

let hello_server () =
  let server = T.init () in
  Lwt.finalize ( fun () ->
      let addr = Uwt.Misc.ip4_addr_exn server_ip4 server_port in
      T.bind_exn server ~addr ();
      let () = T.listen_exn server ~max:server_backlog ~cb:on_listen in
      Help.wait ()
    ) ( fun () -> T.close_noerr server; Lwt.return_unit )

let () = U.Main.run (hello_server ())

(* let () = Help.clean () *)
