0.2.0 2017-XX-XX
---------------------------
- `Uwt.Misc.guess_handle` now takes an `Unix.file_descr` as parameter
   (instead of `Uwt.file`). This doesn't follow the libuv prototypes,
   but makes more sense in the OCaml context. `Uwt.Misc.guess_handle`
   can also classify UDP and TCP sockets on Windows.

- The IPC interface has changed:
  * `Uwt.Stream.write2` has been removed. There are now properly typed
     `Uwt.Pipe.write2*` functions that can also send UDP handles to a
     child process.
  * `Uwt.Stream.accept_raw` has been removed. Instead
    `Uwt.Pipe.accept_ipc` must be used to accept handles that were
    send via the libuv specific IPC interface. `Uwt.Tcp.accept_raw`
    and `Uwt.Pipe.accept_raw` still exist.
  * The `Uwt.Process.stdio` type has been
    extended. `Uwt.Process.spawn` now allows to create duplex stream
    for IPC (previously not exposed by uwt)

 - Exception update: Many functions `Lwt.fail` with `exception
   Unix_error of error * string * string` (or the `foo_exn` functions
   will raise such exceptions). The first string component of this
   exception is supposed to contain the function name. However there
   was no consistency what was put there as name. If the function is
   called uv_foo (or `uv_xy_foo`, `uv_foo_xy`, etc.) and `man 2 foo`
   or `man 3 foo` lists a closely related function that could be
   wrapped by `uv_foo`, `foo` is used as function name (even if a
   different, system dependent function is used internally by
   libuv). This leads to more consistent behaviour regarding to `Unix`
   and `Lwt_unix`. If it's not obvious (by just looking at the name),
   which function is called by libuv, `uv_foo` is used instead.

-  The mli files are now better documented.

- `Uwt.Tcp.keepalive` has been split in two functions:
  `Uwt.Tcp.enable_keepalive` and `Uwt.Tcp.disable_keepalive`

- Tasks that are intended to run in libuv's threadpool and are
  canceled via `Lwt.cancel` now report the success of the cancellation
  immediately instead of slightly deferred. So the following check
  will work now:

  ```ocaml
  let s = Uwt.Fs.stat "foo" in
  (* more code *)
  assert (Lwt.state s = Lwt.Sleep); (* relevant use case *)
  Lwt.cancel s;
  match Lwt.state s with
  | Lwt.Sleep -> print_endline "cancel failed, the job is already running in background"
  | Lwt.Fail Lwt.Canceled -> print_endline "cancel success"
  | _ -> prerr_endline "Huh?"```

- Bugs in the following functions have been fixed:
  * `Uwt.Udp.set_multicast_ttl` (it rejected valid parameters)
  * `Uwt.Handle_ext.get_send_buffer_size` /
    `Uwt.Handle_ext.get_recv_buffer_size` (they've returned wrong
    results)
  * `Uwt.Stream.write2` (now ``Uwt.Pipe.write2`) (invalid memory
    access under Windows, because libuv's interface slightly differs
    on windows and unix)

0.1.0 2017-03-02
---------------------------
- new functions: Uwt.Stream.writev and Uwt.Fs.writev

- the opam package will now always use the internal copy of
  libuv.

- prepare OCaml 4.05 support

- internal libuv version updated to 1.11.0

- avoid memcpy calls inside Uwt.Stream.read and Uwt.Udp.recv
  (*nix only)

- Set up Travis for OS X testing

0.0.4 2016-11-04
---------------------------
- API CHANGE: `Uwt_error` removed. Functions that possibly fail with
  no-unix error codes will now always return `('a, Uwt.error) result
  Lwt.t`, all other functions will `Lwt.fail` with
  `Unix_error`. `Unix.ECANCELED` is unfortunately missing, you have to
  use `Uwt.of_unix_error x = Uwt.ECANCELED` instead.
  
- uwt now compiles with Microsoft Visual Studio (14.0 only)
  
- internal libuv version updated to 1.10.0

- update lwt.unix compatibility layer
