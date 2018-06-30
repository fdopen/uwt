(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

#include "config.inc"
open Lwt.Infix

type command = string * string array

#if HAVE_WINDOWS <> 0
let shell cmd = ("cmd.exe", [|"cmd.exe"; "/c"; cmd|])
#else
let shell cmd = ("/bin/sh", [|"/bin/sh"; "-c"; cmd|])
#endif

type redirection =
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Unix.file_descr
    | `FD_move of Unix.file_descr
    | `File_copy of Uwt.file
    | `File_move of Uwt.file
    | `Pipe_copy of Uwt.Pipe.t
    | `Pipe_move of Uwt.Pipe.t
    | `Stream_copy of Uwt.Stream.t
    | `Stream_move of Uwt.Stream.t
    ]

type iredirection =
  [ redirection | `Pipe of Uwt.Pipe.t ]
(*
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Unix.file_descr
    | `FD_move of Unix.file_descr
    | `Pipe of Uwt.Pipe.t
    ]
*)


let get_fd fd = function
| `Keep ->   Some (Uwt.Process.Inherit_file fd)
| `Pipe x -> Some (Uwt.Process.Create_pipe x)
| `Dev_null (* libuv always redirects stdin, stdout, stderr to /dev/null *)
| `Close   -> None
| `Pipe_move p
| `Pipe_copy p -> Some (Uwt.Process.Inherit_pipe p)
| `Stream_copy s
| `Stream_move s -> Some (Uwt.Process.Inherit_stream s)
| `File_copy s
| `File_move s -> Some (Uwt.Process.Inherit_file s)
| `FD_copy fd'
| `FD_move fd' ->
  match Uwt.Conv.file_of_file_descr fd' with
  | None -> failwith "Uwt.File details changed"
  | Some x ->
    Some (Uwt.Process.Inherit_file x)


type res = {
  exit_status: int;
  term_signal: int;
}

type proc = {
  t : Uwt.Process.t;
  sleeper : res Lwt.t
}

let spawn
    ?uid
    ?gid
    ?env
    ?cwd
    ?(stdin:iredirection=`Keep)
    ?(stdout:iredirection=`Keep)
    ?(stderr:iredirection=`Keep)
    ((prog, args):command)
  =
  let pstdin  = get_fd Uwt.stdin stdin
  and pstdout = get_fd Uwt.stdout stdout
  and pstderr = get_fd Uwt.stderr stderr
  and sleeper,waker = Lwt.task () in
  let exit_cb t ~exit_status ~term_signal =
    Uwt.Process.close_noerr t;
    Lwt.wakeup waker {exit_status; term_signal}
  in
  let env = match env with
  | None -> None
  | Some x -> Some(Array.to_list x)
  in
  let prog =
    if prog = "" && Array.length args > 0 then
      args.(0)
    else
      prog in
  let t =
    Uwt.Process.spawn_exn
      ?uid
      ?gid
      ?cwd
      ?env
      ?stdin:pstdin
      ?stdout:pstdout
      ?stderr:pstderr
      ~exit_cb
      prog
      (Array.to_list args)
  in
  let close = function
  | `FD_move fd -> Unix.close fd
  | `Pipe_move p -> Uwt.Pipe.close_noerr p
  | `Stream_move s -> Uwt.Stream.close_noerr s
  | `File_move s -> let _ : unit Lwt.t = Uwt.Fs.close s in ()
  | _ -> ()
  in
  close stdin;
  close stdout;
  close stderr;
  {
    t;
    sleeper
  }

type state =
  | Running
  | Exited of Unix.process_status


let status x = (* WSTOPPED not provided libuv *)
  if x.term_signal <> 0 then
    Unix.WSIGNALED(x.term_signal)
  else
    Unix.WEXITED(x.exit_status)


let ignore_close chan = ignore (Uwt_io.close chan)

class virtual common timeout proc channels =
  let wait = proc.sleeper in
object(self)
  val mutable closed = false
  method pid = Uwt.Process.pid_exn proc.t

  method state =
    match Lwt.poll wait with
      | None -> Running
      | Some x -> Exited (status x)

  method kill signum =
    if Lwt.state wait = Lwt.Sleep then
      Uwt.Process.kill_exn ~pid:(Uwt.Process.pid_exn proc.t) ~signum

  method terminate =
    if Lwt.state wait = Lwt.Sleep then
      Uwt.Process.process_kill_exn proc.t Sys.sigkill

  method close =
    if closed then self#status
    else (
      closed <- true;
      Lwt.protected (Lwt.join (List.map Uwt_io.close channels))
      >>= fun () -> self#status
    )
  method status = Lwt.protected wait >|= status

  initializer
    (* Ensure channels are closed when no longer used. *)
    List.iter (Gc.finalise ignore_close) channels;
    (* Handle timeout. *)
    match timeout with
      | None ->
          ()
      | Some dt ->
          ignore (
            (* Ignore errors since they can be obtained by
               self#close. *)
            Lwt.try_bind
              (fun () ->
                 Lwt.choose [(Uwt.Unix.sleep dt >>= fun () -> Lwt.return_false);
                             (wait >>= fun _ -> Lwt.return_true)])
              (function
                 | true ->
                     Lwt.return_unit
                 | false ->
                     self#terminate;
                     self#close >>= fun _ -> Lwt.return_unit)
              (fun _exn ->
                 (* The exception is dropped because it can be
                    obtained with self#close. *)
                 Lwt.return_unit)
          )
end

external cast_chan : 'a Uwt_io.channel -> unit Uwt_io.channel = "%identity"

class process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  let stdin = (stdin : redirection option :> iredirection option)
  and stdout = (stdout : redirection option :> iredirection option)
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd in
object
  inherit common timeout proc []
end

class process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  let stdin = (stdin : redirection option :> iredirection option)
  and stdout = Uwt.Pipe.init ()
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?uid ?gid ?env ?cwd ?stdin ~stdout:(`Pipe stdout) ?stderr cmd in
  let stdout =
    Uwt_io.of_stream
      ~mode:Uwt_io.input
      (Uwt.Pipe.to_stream stdout)
  in
object
  inherit common timeout proc [cast_chan stdout]
  method stdout = stdout
end

class process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd =
  let stdin = Uwt.Pipe.init ()
  and stdout = (stdout : redirection option :> iredirection option)
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc = spawn ?cwd ?uid ?gid ?env ~stdin:(`Pipe stdin) ?stdout ?stderr cmd in
  let stdin =
    Uwt_io.of_stream
      ~mode:Uwt_io.output
      (Uwt.Pipe.to_stream stdin)
  in
object
  inherit common timeout proc [cast_chan stdin]
  method stdin = stdin
end

class process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd =
  let stdin = Uwt.Pipe.init ()
  and stdout = Uwt.Pipe.init ()
  and stderr = (stderr : redirection option :> iredirection option) in
  let proc =
    spawn ?uid ?gid ?env ?cwd ~stdin:(`Pipe stdin) ~stdout:(`Pipe stdout) ?stderr cmd
  in
  let stdin =
    Uwt_io.of_stream
      ~mode:Uwt_io.output
      (Uwt.Pipe.to_stream stdin)
  and stdout =
    Uwt_io.of_stream
      ~mode:Uwt_io.input
      (Uwt.Pipe.to_stream stdout)
  in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout]
  method stdin = stdin
  method stdout = stdout
end

class process_full ?timeout ?uid ?gid ?env ?cwd cmd =
  let stdin = Uwt.Pipe.init ()
  and stdout = Uwt.Pipe.init ()
  and stderr = Uwt.Pipe.init () in
  let proc =
    spawn ?uid ?gid ?env ?cwd
      ~stdin:(`Pipe stdin)
      ~stdout:(`Pipe stdout)
      ~stderr:(`Pipe stderr)
      cmd
  in
  let stdin =
    Uwt_io.of_stream
      ~mode:Uwt_io.output
      (Uwt.Pipe.to_stream stdin)
  and stdout =
    Uwt_io.of_stream
      ~mode:Uwt_io.input
      (Uwt.Pipe.to_stream stdout)
  and stderr =
    Uwt_io.of_stream
      ~mode:Uwt_io.input
      (Uwt.Pipe.to_stream stderr)
  in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout; cast_chan stderr]
  method stdin = stdin
  method stdout = stdout
  method stderr = stderr
end

let open_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  new process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd
let open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  new process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd
let open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd =
  new process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd
let open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd =
  new process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd
let open_process_full ?timeout ?uid ?gid ?env ?cwd cmd =
  new process_full ?timeout ?uid ?gid ?env ?cwd cmd


let make_with backend ?timeout ?uid ?gid ?env ?cwd cmd f =
  let process = backend ?timeout ?uid ?gid ?env ?cwd cmd in
  Lwt.finalize
    (fun () -> f process)
    (fun () ->
      process#close >>= fun _ ->
      Lwt.return_unit)

let with_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd f =
  make_with (open_process_none ?stdin ?stdout ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd f =
  make_with (open_process_in ?stdin ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd f =
  make_with (open_process_out ?stdout ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd f =
  make_with (open_process ?stderr) ?timeout ?uid ?gid ?env ?cwd cmd f
let with_process_full ?timeout ?uid ?gid ?env ?cwd cmd f =
  make_with open_process_full ?timeout ?uid ?gid ?env ?cwd cmd f

let exec ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd =
  (open_process_none ?timeout ?uid ?gid ?env ?cwd ?stdin ?stdout ?stderr cmd)#close


let ignore_close ch =
  ignore (Uwt_io.close ch)

let read_opt read ic =
  Lwt.catch
    (fun () -> read ic >|= fun x -> Some x)
    (function
    | End_of_file -> Lwt.return_none
    | Unix.Unix_error(Unix.EPIPE, _, _) when Sys.win32 -> Lwt.return_none
    | Unix.Unix_error(x, _, _)  when Uwt.of_unix_error x = Uwt.EOF ->
      Lwt.return_none
    | exn -> Lwt.fail exn)

let recv_chars pr =
  let ic = pr#stdout in
  Gc.finalise ignore_close ic;
  Lwt_stream.from (fun _ ->
      read_opt Uwt_io.read_char ic >>= fun x ->
      if x = None then begin
        Uwt_io.close ic >>= fun () ->
        Lwt.return x
      end else
        Lwt.return x)

let recv_lines pr =
  let ic = pr#stdout in
  Gc.finalise ignore_close ic;
  Lwt_stream.from (fun _ ->
                     read_opt Uwt_io.read_line ic >>= fun x ->
                     if x = None then begin
                       Uwt_io.close ic >>= fun () ->
                       Lwt.return x
                     end else
                       Lwt.return x)

let recv pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Uwt_io.read ic)
    (fun () -> Uwt_io.close ic)

let recv_line pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Uwt_io.read_line ic)
    (fun () -> Uwt_io.close ic)

let send f pr data =
  let oc = pr#stdin in
  Lwt.finalize
    (fun () -> f oc data)
    (fun () -> Uwt_io.close oc)

(* Receiving *)

let pread ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_chars ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_chars (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_line ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_line (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

let pread_lines ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd =
  recv_lines (open_process_in ?timeout ?uid ?gid ?env ?cwd ?stdin ?stderr cmd)

(* Sending *)

let pwrite ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd text =
  send Uwt_io.write (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) text

let pwrite_chars ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd chars =
  send Uwt_io.write_chars (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) chars

let pwrite_line ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd line =
  send Uwt_io.write_line (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) line

let pwrite_lines ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd lines =
  send Uwt_io.write_lines (open_process_out ?timeout ?uid ?gid ?env ?cwd ?stdout ?stderr cmd) lines

(* Mapping *)

type 'a map_state =
  | Init
  | Save of 'a option Lwt.t
  | Done

(* Monitor the thread [sender] in the stream [st] so write errors are
   reported. *)
let monitor sender st =
  let sender = sender >|= fun () -> None in
  let state = ref Init in
  Lwt_stream.from
    (fun () ->
       match !state with
         | Init ->
             let getter = Lwt.apply Lwt_stream.get st in
             let result _ =
               match Lwt.state sender with
                 | Lwt.Sleep ->
                     (* The sender is still sleeping, behave as the
                        getter. *)
                     getter
                 | Lwt.Return _ ->
                     (* The sender terminated successfully, we are
                        done monitoring it. *)
                     state := Done;
                     getter
                 | Lwt.Fail _ ->
                     (* The sender failed, behave as the sender for
                        this element and save current getter. *)
                     state := Save getter;
                     sender
             in
             Lwt.try_bind (fun () -> Lwt.choose [sender; getter]) result result
         | Save t ->
             state := Done;
             t
         | Done ->
             Lwt_stream.get st)

let pmap ?timeout ?uid ?gid ?env ?cwd ?stderr cmd text =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Uwt_io.write pr text in
  let getter = recv pr in
  Lwt.catch
    (fun () ->
      (* Wait for both to terminate, returning the result of the
         getter. *)
      sender >>= fun () -> getter)
    (function
    | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.fail exn
    | exn -> Lwt.fail exn)

let pmap_chars ?timeout ?uid ?gid ?env ?cwd ?stderr cmd chars =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  let sender = send Uwt_io.write_chars pr chars in
  monitor sender (recv_chars pr)

let pmap_line ?timeout ?uid ?gid ?env ?cwd ?stderr cmd line =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Uwt_io.write_line pr line in
  let getter = recv_line pr in
  Lwt.catch
    (fun () ->
      (* Wait for both to terminate, returning the result of the
         getter. *)
      sender >>= fun () -> getter)
    (function
    | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.fail exn
    | exn -> Lwt.fail exn)

let pmap_lines ?timeout ?uid ?gid ?env ?cwd ?stderr cmd lines =
  let pr = open_process ?timeout ?uid ?gid ?env ?cwd ?stderr cmd in
  let sender = send Uwt_io.write_lines pr lines in
  monitor sender (recv_lines pr)
