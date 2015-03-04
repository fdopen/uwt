let t_finally f x finallly' y =
  let res =
    try f x with exn -> finallly' y; raise exn
  in
  finallly' y;
  res

let write_all fd str =
  let len = String.length str in
  let rec iter ofs =
    if ofs >= len then
      ()
    else
      match Unix.write_substring fd str ofs (len - ofs) with
      | x -> iter (ofs + x)
      | exception Unix.Unix_error((Unix.EINTR|Unix.EAGAIN),_,_) ->
        iter ofs
  in
  iter 0

let rec sleep_short t =
  match Unix.select [] [] [] t with
  | _,_,_ -> ()
  | exception Unix.Unix_error((Unix.EINTR|Unix.EAGAIN),_,_) ->
    sleep_short t

let test_strings = [ "akkkk" ; "dkasjf" ; "aldkfjasdf" ; "kkazsdftgzuterzu" ]
let server_address = Unix.inet_addr_of_string "127.0.0.1"
let server_port = 12345

let server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  t_finally ( fun socket ->
      (try Unix.setsockopt socket Unix.SO_REUSEADDR true
      with Unix.Unix_error _ -> () );
      Unix.bind socket (Unix.ADDR_INET (server_address, server_port));
      Unix.listen socket 10;
      let (fd, _) = Unix.accept socket in
      t_finally ( fun fd ->
          Unix.set_nonblock fd;
          let f s =
            sleep_short 0.3;
            write_all fd s
          in
          List.iter f test_strings;
          Unix.shutdown fd Unix.SHUTDOWN_ALL;
        ) fd Unix.close fd
    ) socket Unix.close socket

let rec really_read fd buf ofs len =
  match Unix.read fd buf ofs len with
  | x -> x
  | exception Unix.Unix_error(Unix.EINTR,_,_) ->
    really_read fd buf ofs len

open Common
open Lwt.Infix
let poll_read () =
  let server_thread = Uwt_preemptive.detach server () in
  Uwt.Timer.sleep 10 >>= fun () ->
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let close = lazy ( Unix.close fd ) in
  try_finally ( fun () ->
      Unix.connect fd (Unix.ADDR_INET (server_address, server_port));
      let socket = match Uwt.Compat.socket_of_file_descr fd with
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
      | Uwt.Error _ -> abort s "event error"
      | Uwt.Ok x ->
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
            Buffer.add_subbytes buf b 0 len
          )
      in
      let s = Uwt.Poll.start_socket_exn socket Uwt.Poll.Readable ~cb in
      try_finally ( fun () ->
          Lwt.join [ sleeper ; server_thread ] >>= fun () ->
          if !cb_called = List.length test_strings &&
             Buffer.contents buf = String.concat "" test_strings then
            Lwt.return_true
          else
            Lwt.return_false
        ) ( fun () -> Uwt.Poll.close_noerr s ; Lwt.return_unit )
    ) ( fun () -> Lazy.force close ; Lwt.return_unit )

open OUnit2
let l = [
  ("poll_read">:: fun _ctx -> m_true ( poll_read () ));
]

let l = "Poll">:::l
