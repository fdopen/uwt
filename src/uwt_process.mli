(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(** Process management *)

(** This module allows you to spawn processes and communicate with them. *)

type command = string * string array
    (** A command. The first field is the name of the executable and
        the second is the list of arguments. For example:

        {[
          ("ls", [|"ls"; "-l"|])
        ]}

        Notes:

        - if the name is the empty string, then the first argument
          will be used (for backward compatibility with [Lwt_process]).

        - It is not possible to ``inline'' an argument,
          i.e. split it into multiple arguments with "\000".
          (like under [Lwt_process])
    *)

val shell : string -> command
  (** A command executed with the shell. (with ["/bin/sh -c <cmd>"] on
      Unix and ["cmd.exe /c <cmd>"] on Windows). *)

(** All the following functions take an optional argument
    [timeout]. If specified, after expiration, the process will be
    sent a [Unix.sigkill] signal and channels will be closed. *)

(** {2 High-level functions} *)

(** {3 Redirections} *)

(** A file descriptor redirection. It describes how standard file
    descriptors are redirected in the child process. *)
type redirection =
    [ `Keep  (** The file descriptor is left unchanged *)
    | `Dev_null  (** Connect the file descriptor to [/dev/null] *)
    | `Close  (** It's now the same as `Dev_null *)
    | `FD_copy of Unix.file_descr
        (** The file descriptor is replaced by the given
            one *)
    | `FD_move of Unix.file_descr
        (** The file descriptor is replaced by the given one, which is
            then closed. *)
    | `File_copy of Uwt.file
    | `File_move of Uwt.file
    | `Pipe_copy of Uwt.Pipe.t
    | `Pipe_move of Uwt.Pipe.t
    | `Stream_copy of Uwt.Stream.t
    | `Stream_move of Uwt.Stream.t ]


(** Note: all optional redirection arguments default to [`Keep] *)

(** {3 Executing} *)

val exec :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> Unix.process_status Lwt.t
  (** Executes the given command and returns its exit status.
      [Unix.WSTOPPED] is not supported by libuv at the moment.
      It will return either [Unix.WSIGNALED] or [Unix.WEXITED]
  *)

(** {3 Receiving} *)

val pread :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt.t
val pread_chars :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t
val pread_line :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt.t
val pread_lines :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t

(** {3 Sending} *)

val pwrite :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string -> unit Lwt.t
val pwrite_chars :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t -> unit Lwt.t
val pwrite_line :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string -> unit Lwt.t
val pwrite_lines :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t -> unit Lwt.t

(** {3 Mapping} *)

val pmap :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> string -> string Lwt.t
val pmap_chars :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t -> char Lwt_stream.t
val pmap_line :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> string -> string Lwt.t
val pmap_lines :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t -> string Lwt_stream.t

(** {2 Spawning processes} *)

(** State of a sub-process *)
type state =
  | Running
      (** The process is still running *)
  | Exited of Unix.process_status
      (** The process has exited *)

class process_none :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command ->
object
  method pid : int
    (** Pid of the sub-process *)

  method state : state
    (** Return the state of the process *)

  method kill : int -> unit
    (** [kill signum] sends [signum] to the process if it is still
        running. *)

  method terminate : unit
    (** Terminates the process. It is equivalent to [kill Sys.sigkill]
        on Unix but also works on Windows (unlike {!kill}). *)

  method status : Unix.process_status Lwt.t
    (** Threads which wait for the sub-process to exit then returns its
        exit status *)

  method close : Unix.process_status Lwt.t
    (** Closes the process and returns its exit status. This closes all
        channels used to communicate with the process *)
end

val open_process_none :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> process_none
val with_process_none :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> (process_none -> 'a Lwt.t) -> 'a Lwt.t

class process_in :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdout : Uwt_io.input_channel
    (** The standard output of the process *)
end

val open_process_in :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> process_in
val with_process_in :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

class process_out :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdin : Uwt_io.output_channel
    (** The standard input of the process *)
end

val open_process_out :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> process_out
val with_process_out :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

class process :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdin : Uwt_io.output_channel
  method stdout : Uwt_io.input_channel
end

val open_process :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> process
val with_process :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  ?stderr : redirection ->
  command -> (process -> 'a Lwt.t) -> 'a Lwt.t

class process_full :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  command ->
object
  inherit process_none

  method stdin : Uwt_io.output_channel
  method stdout : Uwt_io.input_channel
  method stderr : Uwt_io.input_channel
end

val open_process_full :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  command -> process_full
val with_process_full :
  ?timeout : float ->
  ?uid:int ->
  ?gid:int ->
  ?env : string array ->
  ?cwd: string ->
  command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
