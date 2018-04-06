0.3.0 2018-04-15
---------------------------

- update to lwt 4.0.0:
 * The modules `Uwt_chan` and `Uwt_log` have been removed. `Uwt_log`
   is available as an external library
 * `Uwt_io.file_length` now fails with `EISDIR` when used on a
   directory (see https://github.com/ocsigen/lwt/issues/563)

- update to libuv 1.20.0:
 * `Uwt.Fs.copyfile` now supports file cloning

0.2.4 2018-02-10
---------------------------

- fix compilation under OCaml 4.06.0 (Windows)

- update to libuv 1.19.1:
   * new flags for `Uwt.Fs.openfile`: `O_DIRECT`, `O_EXLOCK`,
     `O_NOATIME`, `O_SYMLINK`, `O_NOFOLLOW`, and `O_DIRECTORY`
   * new function: `Uwt.Pipe.chmod` to allow access from other users
   * new function: `Uwt.Misc.getppid` (similar to `Unix.getppid`, but also
     supported under Windows).

0.2.3 2017-09-08
---------------------------

- update to libuv 1.14.1

- new functions from `Lwt_io`: `Uwt_io.open_temp_file` and
  `Uwt_io.with_temp_file`

- `Uwt_base.Sys_info.win_version` now reports your current Windows
  version, even if your executable is not manifested.

0.2.2 2017-08-17
---------------------------

- update to libuv 1.14.0:
  * new function: `Uwt.Fs.copyfile`
  * `Uwt.Poll.start` now supports watching for sysfs interrupts or TCP
    out-of-band messages
  * `Uwt.Udp.try_send` now works under Windows

- `Uwt.Tcp.enable_keepalive` will now fail, if you pass an `Uwt.Tcp.t`
  handle to it, that doesn't wrap a socket yet. libuv silently ignores
  the delay parameter in this case.  You can use `Uwt.Tcp.init_ipv4` or
  `Uwt.Tcp.init_ipv6` instead of `Uwt.Tcp.init` to circumvent this problem.

- `Uwt_io` and `Uwt_log` have been updated to match the recent `lwt.unix`
  changes.

0.2.1 2017-06-01
---------------------------

- update internal libuv version to to 1.12.0
  * new functions: `Uwt.Signal.oneshot` and
    `Uwt_base.Misc.{put,set,unset}env` (added because of utf8 support
    under windows)

0.2.0 2017-12-05
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

- new functions: `Uwt.Fs.pread` / `Uwt.Fs.pwrite` / `Uwt.Fs.pwritev`

- The mli files are now better documented.

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
