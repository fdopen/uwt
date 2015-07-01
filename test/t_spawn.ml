open OUnit2
open Lwt.Infix
open Common

module P = Uwt.Process

(* We feed cat from stdin - and read its output from stdout.
   Then, we compare the result *)
let cat_test () =
  let len = 16_777_211 in (* 16MB, limit for 32-bit OCaml *)
  let b = rstring_create len in
  let stdin = Uwt.Pipe.init () in
  let stdout = Uwt.Pipe.init () in
  let sleeper,waker = Lwt.task () in
  let exit_cb t ~exit_status ~term_signal:_ =
    P.close_noerr t;
    Lwt.wakeup waker exit_status
  in
  let _ : Uwt.Process.t =
    P.spawn_exn
      ~exit_cb
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
  let write =
    Uwt.Pipe.write_string stdin ~buf:b >>= fun () -> Uwt.Pipe.close_wait stdin
  in
  Lwt.join [ write ; read 0 ] >>= fun () ->
  sleeper >>= fun ec ->
  if ec <> 0 then
    Lwt.fail_with "wrong exit status"
  else
    let blen = Buffer.length out_buf in
    if blen <> len then (
      Uwt_io.eprintf "different lengths! written:%d vs. read:%d\n" len blen
      >>= fun () -> Lwt.return_false
    )
    else if b <> Buffer.contents out_buf then (
      Uwt_io.eprintl "input and output have the same length, buf differ"
      >>= fun () -> Lwt.return_false
    )
    else
      Lwt.return_true


let gzip_error_message = "exit status negative"

let p_close s =
  Lwt.catch ( fun () -> Uwt.Pipe.close_wait s )
    ( fun _ -> Lwt.return_unit )

(* We feed gzip from stdin, connect it's output to gunzip,
   read gunzip's output and compare it with the original
   message. *)
let gzip_test ?prog ?args () =
  let len = 131_073 in
  let b = rba_create len in
  let stdin = Uwt.Pipe.init () in
  let stdout = Uwt.Pipe.init () in
  let exit_cb f waker t ~exit_status ~term_signal:_ =
    f:=true;
    P.close_noerr t;
    Lwt.wakeup waker exit_status
  in
  let gzip_exit_status,waker = Lwt.task () in
  let p_gzip_finished = ref false in
  let p_gzip =
    let prog = match prog with
    | None -> "gzip"
    | Some x -> x
    and args = match args with
    | None -> [ "gzip" ; "--fast" ]
    | Some x -> x
    in
    P.spawn_exn
      ~stdin:(P.Create_pipe stdin)
      ~stdout:(P.Create_pipe stdout)
      ~exit_cb:(exit_cb p_gzip_finished waker)
      prog args
  in
  let gunzip_exit_status,waker = Lwt.task () in
  let p_gunzip_finished = ref false in
  let p_gunzip,stdout =
    try
      let stdout' = Uwt.Pipe.init () in
      let x =
        P.spawn_exn
          ~exit_cb:(exit_cb p_gunzip_finished waker)
          ~stdin:(P.Inherit_pipe stdout)
          ~stdout:(P.Create_pipe stdout')
          "gzip" [ "gzip" ; "-d" ]
      in
      Uwt.Pipe.close_noerr stdout;
      x,stdout'
    with
    | exn ->
      Uwt.Pipe.close_noerr stdout;
      Uwt.Pipe.close_noerr stdin;
      Uwt.Process.process_kill_exn p_gzip Sys.sigkill;
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
        | 2::0::[]
        | 0::2::[]
        | 2::2::[] (* 2: warnings, not errors - and we still test the output *)
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
      p_close stdin >>= fun () ->
      p_close stdout >>= fun () ->
      if !p_gzip_finished = false then
        ignore (Uwt.Process.process_kill p_gzip Sys.sigkill);
      if !p_gunzip_finished = false then
        ignore (Uwt.Process.process_kill p_gunzip Sys.sigkill);
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


let with_npipe f =
  Lwt.wrap1 (Uwt.Unix.pipe_exn ~cloexec:true) () >>= fun (p1,p2) ->
  let pipes = Uwt.Pipe.to_stream p1, Uwt.Pipe.to_stream p2 in
  Lwt.finalize ( fun () -> f pipes )
    ( fun () ->
        Uwt.Pipe.close_noerr p1 ; Uwt.Pipe.close_noerr p2 ;
        Lwt.return_unit )

let pipeline ?(wrong_args=false) () =
  with_npipe ( fun (read1,write1) ->
      with_npipe ( fun (read2,write2) ->
          with_npipe ( fun (read3,write3) ->
              let ok x s =
                s#close >>= fun s ->
                if s = Unix.WEXITED 0 || ( x && s = Unix.WEXITED 2 ) then
                  Lwt.return_true
                else
                  Lwt.return_false
              in
              let ok s = ok false s
              and okgzip s = ok true s in
              let buf_len =
                (* This is a workaround for the wired
                   behaviour of cygwin's gzip that I don't
                   want to debug. The behaviour is the same for
                   dd if=/dev/zero bs=128k count=1000 | gzip | gzip -d | gzip -d | wc -c
                   from cmd.exe (not cygwin)
                *)
                if wrong_args && Sys.win32 then
                  1021
                else
                  245_411
              in
              let buf = rstring_create buf_len in
              let stderr = `Dev_null in
              let force_close s f =
                Lwt.catch ( fun () -> f s )
                  ( fun exn ->
                      match s#state with
                      | Uwt_process.Exited _ -> Lwt.fail exn
                      | Uwt_process.Running ->
                        Lwt.catch ( fun () -> s#close >>= fun _ ->
                                    Lwt.fail exn )
                          ( fun _ -> Lwt.fail exn ))
              in
              let p1 =
                Uwt_process.with_process_out
                  ~stderr
                  ~stdout:(`Stream_move write1)
                  ("gzip",[|"gzip"|]) @@ fun s -> force_close s @@ fun s ->
                Uwt.Main.yield () >>= fun () ->
                Uwt_io.write s#stdin buf >>= fun () ->
                okgzip s
              in
              let p2 =
                Uwt_process.with_process_none
                  ~stderr
                  ~stdin:(`Stream_move read1)
                  ~stdout:(`Stream_move write2)
                  ("gzip",[|"gzip";"-d"|]) okgzip
              in
              let p3 =
                let args = match wrong_args with
                | false -> ("cat",[|"cat";"-"|])
                | true -> ("gzip",[|"gzip";"-d"|])
                in
                Uwt_process.with_process_none
                  ~stderr
                  ~stdin:(`Stream_move read2)
                  ~stdout:(`Stream_move write3)
                  args okgzip
              in
              let p4 =
                Uwt_process.with_process_in
                  ~stderr
                  ~stdin:(`Stream_move read3)
                  ("cat",[|"cat";"-"|]) @@ fun s -> force_close s @@ fun s ->
                Uwt_io.read s#stdout >>= fun buf' ->
                ok s >>= function
                | true -> Lwt.return (buf' = buf)
                | false -> Lwt.return_false
              in
              let ret_status = ref true
              and exn = ref None in
              let f s =
                Lwt.catch ( fun () ->
                    s >>= function
                    | false -> ret_status:=false; Lwt.return_unit
                    | true -> Lwt.return_unit
                  ) ( fun e ->
                    if !exn = None then
                      exn:= Some e;
                    Lwt.return_unit )
              in
              Lwt_list.iter_p f [p1;p2;p3;p4] >>= fun () ->
              match !exn with
              | Some x -> Lwt.fail x
              | None -> Lwt.return (!ret_status)
            )))

let exn_ok = function
| Failure t when t = gzip_error_message -> Lwt.return_true
| Uwt.Uwt_error(Uwt.ENOTCONN,"write",_)
| Uwt.Uwt_error((Uwt.EPIPE|Uwt.EOF),_,_) -> Lwt.return_true
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
            gzip_test ~prog:"gzip" ~args:["gzip";"-d"] () >>= fun _ ->
            Lwt.fail_with "success in gzip failure2")
          exn_ok
      in
      m_true l);
  ("process_in">:: fun _ ->
      let msg = "Hello World" in
      let t =
        Uwt_process.with_process_in ("echo",[|"echo";msg|]) @@ fun s ->
        Uwt_io.read s#stdout
      in
      m_equal (msg^"\n") t );
  ("process">:: fun _ ->
      let len = 524_288 in
      let msg = rstring_create len in
      let cwd = tmpdir () in
      let t =
        Uwt_process.with_process ~cwd ("cat",[|"cat";"-"|]) @@ fun s ->
        let res = ref "" in
        let tw = Uwt_io.write s#stdin msg >>= fun () ->
            Uwt_io.close s#stdin
        and tr = Uwt_io.read s#stdout >>= fun s -> res:=s ; Lwt.return_unit in
        Lwt.join [tw;tr] >>= fun () ->
        Lwt.return !res
      in
      m_equal msg t);
  ("pipeline">:: fun _ -> m_true (pipeline ()));
  ("pipeline2">:: fun _ ->
      let t =
        Lwt.catch ( fun () ->
            pipeline ~wrong_args:true () >>= fun l ->
            Lwt.return (not l) )
          (function
          | Uwt.Uwt_error(Uwt.EPIPE,_,_)
          | Uwt.Uwt_error(_,("write"|"read"),_) ->  Lwt.return_true
          | x -> Lwt.fail x)
      in
      m_true t);
  ("sleep_timeout">::
   fun _ ->
     let t =
       Uwt_process.with_process_none
         ~stdin:`Dev_null
         ~stdout:`Dev_null
         ~stderr:`Dev_null
         ~timeout:0.25
         ("sleep",[|"sleep";"10"|]) @@ fun s ->
       s#close >>=function
       | Unix.WSTOPPED _
       | Unix.WEXITED _ -> Lwt.return_false
       | Unix.WSIGNALED _ -> Lwt.return_true
     in
     m_true t);
]

let l = "Spawn">:::l
