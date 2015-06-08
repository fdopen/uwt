let test_strings = [ "akkkk" ; "dkasjf" ; "aldkfjasdf" ; "kkazsdfutu" ; "dafy" ]
let server_port = 9009
let server_ip = "127.0.0.1"


let rec really_read fd buf ofs len =
  match Unix.read fd buf ofs len with
  | x -> x
  | exception Unix.Unix_error(Unix.EINTR,_,_) -> really_read fd buf ofs len

open Common
open Lwt.Infix

let connect () =
  let rec iter accu i =
    if i = 0 then
      Lwt.fail accu
    else
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let addr = Unix.inet_addr_of_string server_ip in
      let addr = Unix.ADDR_INET(addr,server_port) in
      match Unix.connect fd addr with
      | () -> Lwt.return fd
      | exception (Unix.Unix_error _ as x) ->
        Unix.close fd;
        Uwt.Timer.sleep 5 >>= fun () ->
        iter x (pred i)
  in
  iter Not_found 20

let write_strings server client =
  let rec iter = function
  | [] -> Uwt.Tcp.close_noerr server; Lwt.return_unit
  | buf::tl->
    Uwt.Tcp.write_string client ~buf >>= fun () ->
    Uwt.Timer.sleep 125 >>= fun () ->
    iter tl
  in
  iter test_strings

let on_listen server x =
  if Uv.Int_result.is_error x then
    failwith "on_listen error"
  else
    match Uwt.Tcp.accept server with
    | Uv.Error _ -> failwith "accept error"
    | Uv.Ok c -> write_strings server c |> ignore

let poll_read () =
  let server = Uwt.Tcp.init () in
  let t = try_finally ( fun () ->
      let addr = Uv_misc.ip4_addr_exn server_ip server_port in
      Uwt.Tcp.bind_exn server ~addr ();
      let () = Uwt.Tcp.listen_exn server ~max:10 ~cb:on_listen in
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let close = lazy ( Unix.close fd ) in
      try_finally ( fun () ->
          let addr = Unix.inet_addr_of_string server_ip in
          let addr = Unix.ADDR_INET(addr,server_port) in
          Unix.connect fd addr;
          let socket = match Uv.Conv.socket_of_file_descr fd with
          | None -> failwith "Uwt.Compat.socket_of_file_descr"
          | Some x -> x
          in
          let sleeper,waker = Lwt.task () in
          let abort s msg =
            Uwt.Poll.close_noerr s;
            Lazy.force close;
            Lwt.wakeup_exn waker (Failure msg)
          in
          let buf = Buffer.create 128 in
          let cb_called = ref 0 in
          let cb s = function
          | Uv.Error x -> abort s (Uv.err_name x)
          | Uv.Ok x ->
            match x with
            | Uwt.Poll.Readable_writable
            | Uwt.Poll.Writable -> abort s "writable not requested"
            | Uwt.Poll.Readable ->
              let b = Bytes.create 128 in
              let len = really_read fd b 0 128 in
              if len = 0 then (
                Uwt.Poll.close_noerr s;
                Lazy.force close;
                Lwt.wakeup waker ()
              )
              else (
                incr cb_called;
                Buffer.add_subbytes buf b 0 len;
              )
          in
          let s = Uwt.Poll.start_socket_exn socket Uwt.Poll.Readable ~cb in
          try_finally ( fun () ->
              Lwt.pick [ sleeper ; Uwt.Timer.sleep 2_000 ] >>= fun () ->
              if !cb_called > 1 && !cb_called <= List.length test_strings &&
                 Buffer.contents buf = String.concat "" test_strings then
                Lwt.return_true
              else
                Lwt.return_false
            ) ( fun () -> Uwt.Poll.close_noerr s ; Lwt.return_unit )
        ) ( fun () -> Lazy.force close ; Lwt.return_unit )
    ) ( fun () -> Uwt.Tcp.close_noerr server ; Lwt.return_unit )
  in
  Uwt.Main.run t

open OUnit2
let l = [
  ("poll_read">:: fun _ctx -> assert_equal true (poll_read ()));
]

let l = "Poll">:::l
