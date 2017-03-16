0.1.1 2017-XX-XX
---------------------------
-  The `Uwt.Process.stdio` type has been extended. Uwt.Process.spawn
   now allows to create duplex stream for IPC (previously not exposed
   by uwt)

- `Uwt.Tcp.keepalive` has been split in two functions:
  `Uwt.Tcp.enable_keepalive` and `Uwt.Tcp.disable_keepalive`

- Tasks that are intended to run in libuv's threadpool and are canceled
  via `Lwt.cancel` now report the success of the cancellation
  instanlty instead of slightly deferred. So the following check will
  work now:

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
 - `Uwt.Udp.set_multicast_ttl` (it rejected valid parameters)
 - `Uwt.Handle_ext.get_send_buffer_size` /
   `Uwt.Handle_ext.get_recv_buffer_size` (they've returned the wrong
   results)
 - `Uwt.Stream.write2` (invalid memory access under Windows, because
   libuv's interface slightly differs on windows and unix)

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
