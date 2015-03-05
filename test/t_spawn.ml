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

let l = [
  ("cat_test">:: fun _ctx -> m_true (cat_test ()));
  ("process_kill">:: fun _ctx -> m_true (kill_test true));
  ("kill">:: fun _ctx -> m_true (kill_test false));
]

let l = "Spawn">:::l
