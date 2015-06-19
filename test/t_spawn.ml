open OUnit2
open Lwt.Infix
open Common

module P = Uwt.Process
let () = Random.self_init ()

(* We feed cat from stdin - and read its output from stdout.
   Then, we compare the result *)
let cat_test () =
  let len = 16_777_211 in (* 16MB, limit for 32-bit OCaml *)
  let b = Bytes.create len in
  for i = 0 to pred len do
    Bytes.unsafe_set b i (Char.chr (i land 255))
  done;
  let stdin = Uwt.Pipe.init () in
  let stdout = Uwt.Pipe.init () in
  let p =
    P.spawn_exn
      ~stdin:(P.Pipe stdin)
      ~stdout:(P.Pipe stdout)
      "cat" [ "cat" ; "-" ]
  in
  let out_buf = Buffer.create len in
  let out_bytes = Bytes.create 65_536 in
  let rec read i =
    Uwt.Pipe.read ~buf:out_bytes stdout >>= fun n ->
    match n with
    | 0 -> Uwt.Pipe.close_wait stdout
    | n -> Buffer.add_subbytes out_buf out_bytes 0 n ; read (succ i)
  in
  let write =
    Uwt.Pipe.write stdin ~buf:b >>= fun () -> Uwt.Pipe.close_wait stdin
  in
  Lwt.join [ write ; read 0 ] >>= fun () ->
  P.close_wait p >>= fun () ->
  let blen = Buffer.length out_buf in
  if blen <> len then (
    Uwt_io.eprintf "different lengths! written:%d vs. read:%d\n" len blen
    >>=fun () -> Lwt.return_false
  )
  else if b <> Buffer.to_bytes out_buf then (
    Uwt_io.eprintl "input and output have the same length, buf differ"
    >>=fun () -> Lwt.return_false
  )
  else
    Lwt.return_true


let gzip_error_message = "exit status negative"
(* We feed gzip from stdin, connect it's output to gunzip,
   read gunzip's output and compare it with the original
   message. *)
let gzip_test ?prog ?args () =
  let len = 16_777_211 in
  let b = Uwt_bytes.create len in
  for i = 0 to pred len do
    Uwt_bytes.unsafe_set b i (Char.chr (i land 255))
  done;
  let stdin = Uwt.Pipe.init () in
  let stdout = Uwt.Pipe.init () in
  let gzip_exit_status,waker = Lwt.task () in
  let p_gzip_closed = ref false in
  let exit_cb flag waker t ~exit_status ~term_signal:_ =
    flag := true;
    P.close_noerr t;
    Lwt.wakeup waker exit_status
  in
  let p_gzip =
    let prog = match prog with
    | None -> "gzip"
    | Some x -> x
    and args = match args with
    | None -> [ "gzip" ; "--fast" ]
    | Some x -> x
    in
    P.spawn_exn
      ~stdin:(P.Pipe stdin)
      ~stdout:(P.Pipe stdout)
      ~exit_cb:(exit_cb p_gzip_closed waker)
      prog args
  in
  let p_gunzip_closed = ref false in
  let gunzip_exit_status,waker = Lwt.task () in
  let stdout,p_gunzip =
    try
      let stdout' = Uwt.Pipe.init () in
      let p_gunzip =
        P.spawn_exn
          ~exit_cb:(exit_cb p_gunzip_closed waker)
          ~stdin:(P.Pipe stdout)
          ~stdout:(P.Pipe stdout')
          "gunzip" [ "gunzip" ]
      in
      Uwt.Pipe.close_noerr stdout;
      stdout',p_gunzip
    with
    | exn ->
      Uwt.Process.process_kill p_gzip |> ignore ;
      Uwt.Pipe.close_noerr stdout;
      Uwt.Pipe.close_noerr stdin;
      raise exn
  in
  Lwt.catch ( fun () ->
      let out_buf = Buffer.create len in
      let out_bytes = Bytes.create 65_536 in
      let rec read i =
        Uwt.Pipe.read ~buf:out_bytes stdout >>= fun n ->
        match n with
        | 0 -> Uwt.Pipe.close_wait stdout
        | n -> Buffer.add_subbytes out_buf out_bytes 0 n ; read (succ i)
      in
      let write =
        Uwt.Pipe.write_ba stdin ~buf:b >>= fun () -> Uwt.Pipe.close_wait stdin
      in
      Lwt.join [ write ; read 0 ] >>= fun () ->
      Lwt_list.map_p ( fun s -> s ) [ gzip_exit_status ; gunzip_exit_status ]
      >>= (function
        | 0::0::[] ->  Lwt.return_unit
        | _ -> Lwt.fail_with gzip_error_message) >>= fun () ->
      let blen = Buffer.length out_buf in
      if blen <> len then
        Printf.sprintf "different lengths! written:%d vs. read:%d\n" len blen
        |> Lwt.fail_with
      else if Uwt_bytes.to_bytes b <> Buffer.to_bytes out_buf then
        Printf.sprintf "input and output have the same length, buf differ"
        |> Lwt.fail_with
      else
        Lwt.return_true
    ) ( fun exn ->
      if !p_gunzip_closed = false then (
        Uwt.Process.process_kill p_gunzip |> ignore;
        Uwt.Process.close_noerr p_gunzip;
      );
      if !p_gzip_closed = false then (
        Uwt.Process.process_kill p_gzip |> ignore ;
        Uwt.Process.close_noerr p_gzip;
      );
      Uwt.Pipe.close_noerr stdin;
      Uwt.Pipe.close_noerr stdout;
      Lwt.fail exn )


let kill_test t =
  let s,w = Lwt.task () in
  let exit_cb p ~exit_status:_ ~term_signal =
    P.close_noerr p;
    (* signals are emulated on windows. Term signal is not supported,
       if kill is used instead of process_kill *)
    if Sys.unix || t then
      Lwt.wakeup w (term_signal = Sys.sigterm)
    else
      Lwt.wakeup w true
  in
  let p =
    P.spawn_exn ~exit_cb
      "sleep" [ "sleep" ; "20" ]
  in
  try_finally ( fun () ->
      Uwt.Timer.sleep 10 >>= fun () ->
      (match t with
      | true -> P.process_kill_exn p Sys.sigterm;
      | false ->
        let pid = P.pid_exn p
        and signum = Sys.sigterm in
        P.kill_exn ~pid ~signum);
      s
    ) ( fun () -> P.close_noerr p ; Lwt.return_unit )

let exn_ok = function
| Failure t when t = gzip_error_message -> Lwt.return_true
| Uwt.Uwt_error(Uwt.EPIPE,_,_) -> Lwt.return_true
| x -> Lwt.fail x

let l = [
  ("cat_test">:: fun _ctx -> m_true (cat_test ()));
  ("process_kill">:: fun _ctx -> m_true (kill_test true));
  ("kill">:: fun _ctx -> m_true (kill_test false));
  ("gzip_test">:: fun _ -> m_true (gzip_test ()));
  ("gzip_failure">:: fun _ ->
      let l =
        Lwt.catch ( fun () ->
            gzip_test ~prog:"cat" ~args:["cat";"-"] () >>= fun _ ->
            Lwt.fail_with "success in gzip failure" )
          exn_ok
      in
      m_true l);
  ("gzip_failure2">:: fun _ ->
      let l =
        Lwt.catch ( fun () ->
            gzip_test ~prog:"bzip2" ~args:["bzip2";"--fast"] () >>= fun _ ->
            Lwt.fail_with "success in gzip failure2")
          exn_ok
      in
      m_true l);
]

let l = "Spawn">:::l
