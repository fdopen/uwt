open Lwt.Infix
module P = Uwt.Process

let () = Random.self_init ()

(* We feed cat from stdin - and read its output from stdout.
   Then, we compare the result *)
let cat_test () =
  let len = 16_777_211 in (* 16MB, limit for 32-bit OCaml *)
  let b = Bytes.init len ( fun _ -> Random.int 256 |> Char.chr ) in
  let stdin = Uwt.Pipe.init () in
  let stdout = Uwt.Pipe.init () in
  let p =
    P.spawn_exn
      ~stdin:(P.Create_pipe stdin)
      ~stdout:(P.Create_pipe stdout)
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
  let write = Uwt.Pipe.write stdin ~buf:b >>= fun () ->
    Uwt.Pipe.close_wait stdin
  in
  Lwt.join [ write ; read 0 ] >>= fun () ->
  P.close_wait p >>= fun () ->
  let blen = Buffer.length out_buf in
  if blen <> len then
    Uwt_io.eprintf "different lengths! written:%d vs. read:%d\n" len blen
  else if b <> Buffer.to_bytes out_buf then
    Uwt_io.eprintl "input and output have the same length, buf differ"
  else
    Uwt_io.printl "input and output are the same :)"

let () =
  let p = cat_test () in
  Uwt.Main.run p

let () = Uwt.Debug.valgrind_happy ()
