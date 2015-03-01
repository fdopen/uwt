let server_port = 9009

let log s =
  Uwt_io.eprintl s |> ignore

let start_server () =
  let server = Uwt.Udp.init_exn () in
  let cb = function
  | Uwt.Udp.Data (_,None) ->
    log "data, but no address"
  | Uwt.Udp.Partial_data(_,None) ->
    log "partial data, but no address"
  | Uwt.Udp.Empty_from _ ->
    log "empty datagram"
  | Uwt.Udp.Transmission_error _ ->
    log "transmission error"
  | Uwt.Udp.Data(b,Some x)
  | Uwt.Udp.Partial_data(b,Some x) ->
    Uwt.Udp.send server ~buf:b x |> ignore
  in
  Lwt.finalize ( fun () ->
      let addr = Uwt.Misc.ip4_addr_exn "127.0.0.1" server_port in
      let () =
        Uwt.Udp.bind_exn
          ~mode:[ Uwt.Udp.Reuse_addr ]
          server
          addr
      in
      let () = Uwt.Udp.recv_start_exn server ~cb in
      Help.wait ()
    ) ( fun () -> Uwt.Udp.close_noerr server; Lwt.return_unit )

(*
usage: start from shell
$ nc -u 127.0.0.1 9009
and begin typing
*)
let () =
  Uwt.Main.run (start_server ())
