(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(** Uwt main module *)

(** Basic overview:

 - naming conventions mirror the conventions of libuv, so you can
   easily consult the official
   {{:http://docs.libuv.org/en/v1.x/index.html} [libuv manual]}.
   Selected parts of the manual are now included inside the mli files
   (credits to {{:https://github.com/libuv/libuv/blob/v1.x/AUTHORS} [Authors]}),
   the libuv manual is however more up-to-date.

 - Requests are translated to lwt-threads, therefore [uv_req_t] is
   kept internal.

 - Callbacks that are called continually are most of the time not
   translated to the usual lwt semantic.

 - Uwt is {b not} {i compatible} with [lwt.unix]. It's not a further
   [Lwt_engine] in addition to [select] and [libev].

 - Uwt is {b not} {i thread safe}. All uwt functions should be called
   from your main thread.

   Please notice, that there are subtle differences compared to
   [lwt.unix]. Because all requests are accomplished by libuv
   (sometimes in parallel in different threads), you don't have that
   kind of low level control, that you have with [lwt.unix]. Also [Lwt.cancel]
   behaves differently and you won't notice it by looking at the type
   information.

   - [Stream.write] and related functions can never be canceled. The
   analogous write functions in [Lwt_unix] however are cancelable as
   long as the underlying file descriptor is not opened in blocking
   mode. ({!Stream.read} currently returns the only cancelable thread
   inside {!Stream} and its subclasses. But there is no guarantee that
   {!Tcp.read} will continue to be cancelable under Windows in future
   libuv versions.)

   - Everything that is done in work threads (e.g. File I/O, DNS
   resolution, etc.) is cancelable, as long as the task is just queued
   for execution and not already running in a parallel thread. However, it's
   never possible to cancel a background task in [Lwt_unix]. *)

include module type of Uwt_base
  with type error = Uwt_base.error
  with type 'a uv_result = 'a Uwt_base.uv_result
  with type file = Uwt_base.file
  with type sockaddr = Uwt_base.sockaddr
  with type 'a Int_result.t = 'a Uwt_base.Int_result.t
  with type Fs_types.uv_open_flag = Uwt_base.Fs_types.uv_open_flag
  with type Fs_types.file_kind = Uwt_base.Fs_types.file_kind
  with type Fs_types.symlink_mode = Uwt_base.Fs_types.symlink_mode
  with type Fs_types.access_permission = Uwt_base.Fs_types.access_permission
  with type Fs_types.stats = Uwt_base.Fs_types.stats
  with type Misc.timeval = Uwt_base.Misc.timeval
  with type Misc.rusage = Uwt_base.Misc.rusage
  with type Misc.cpu_times = Uwt_base.Misc.cpu_times
  with type Misc.cpu_info = Uwt_base.Misc.cpu_info
  with type Misc.interface_address = Uwt_base.Misc.interface_address
  with type Misc.handle_type = Uwt_base.Misc.handle_type
  with type Misc.version = Uwt_base.Misc.version
  with type Iovec_write.t = Uwt_base.Iovec_write.t

module Main : sig
  (** Analogue of [Lwt_main] *)

  exception Main_error of error * string
  (** Main_error is thrown, when uv_run returns an error - or if lwt
      doesn't report any result and libuv reports, that there are no
      pending tasks. *)

  exception Fatal of exn * Printexc.raw_backtrace
  (** You shouldn't raise exceptions, if you are using uwt. Always use
      {!Lwt.fail}. If you throw exceptions nevertheless, uwt can
      sometimes not propagate the exceptions to the OCaml runtime
      immediately. This applies for example to exceptions that occur
      inside iterative callbacks (like [Stream.read_start],
      [Timer.start], [Poll.start], etc. ). They are passed to
      {!Lwt.async_exception_hook} instead. If your {!Lwt.async_exception_hook}
      then also throws an exception, it is silently ignored.

      However, uwt cannot catch all exceptions at the right
      moment. Don't call any uwt function (especially {!run}) again,
      if you catch such an exception below {!run}. A workaround is
      currently not implemented, because only rare exceptions like
      [Out_of_memory] and [Stackoverflow] are 'fatal' under rare
      conditions - and they usually mean you are in unrecoverable
      trouble anyway.

      {[
        let rec main t1  =
          match Uwt.Main.run t1 with
          | exception Uwt.Main.Fatal(e,p) -> (* fatal, restart your process *)
            log_fatal e p ; cleanup () ; exit 2
          | exception x -> log_normal x ; main t3 (* safe *)
          | result -> let y = ... (* no error *)
      ]}
  *)

  val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

  val run : 'a Lwt.t -> 'a
  (** Unlike {!Lwt_main.run}, it's not allowed to nest calls to Uwt.Main.run.
      The following code is invalid, an exception [Main_error] will be thrown:
      {[
        let help () =
          let () = Uwt.Main.run foo in
          Lwt.return_unit
        in
        Uwt.Main.run (help ())
      ]}

      And {!Uwt.Main.run} will complain about missing work ([Main_error] again):
      {[
        let s,t = Lwt.task () in
        Uwt.Main.run s
      ]}

      With [lwt.unix] the code above could lead to a busy loop (wasting your cpu
      time - but it depends on the selected  Lwt_engine).
      If you really want your process to run forever, without waiting for any
      I/O, you can create a [Uwt.Timer.t] that gets called repeatedly, but does
      nothing. *)

[@@@ocaml.warning "-3"]

  val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called before the main iteration. *)

  val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called after the main iteration. *)

  val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  (** Sets of functions executed just before the program exit.

      Notes:
      - each hook is called exactly one time
      - exceptions raised by hooks are ignored

      Don't use {!Pervasives.exit} together with uwt - or only use it outside any
      function that is passed to {!Main.run}. {!Pervasives.exit}
      interrupts the normal code flow and will leave libuv's internal
      state in an inconsistent state. Your exit hooks will never be called. *)

[@@@ocaml.warning "+3"]

  val at_exit : (unit -> unit Lwt.t) -> unit
  (** [at_exit hook] adds hook at the left of [exit_hooks]*)

  val cleanup : unit -> unit
  (** Call {!cleanup}, if you've called {!run} and and don't intend to
      call {!run} again any time soon. It will free some internally used
      memory, but not all. *)
end

module Fs : sig
  include Fs_functions with type 'a t := 'a Lwt.t
end

module Handle : sig
  (** {!Handle.t} is the base type for all libuv handle types. *)

  type t

  val close : t -> Int_result.unit
  (** Handles are closed automatically, if they are not longer referenced from
      the OCaml heap. Nevertheless, you should nearly always close them with
      {!close}, because:

      - if they wrap a file descriptor, you will sooner or later run
        out of file descriptors. The OCaml garbage collector doesn't give any
        guarantee, when orphaned memory blocks are removed.

      - you might have registered some repeatedly called action (e.g. timeout,
        read_start,...), that prevent that all references get removed from the
        OCaml heap.

      However, it's safe to write code in this manner:
      {[
        let s = Uwt.Tcp.init () in
        let c = Uwt.Tcp.init () in
        Uwt.Tcp.nodelay s false;
        Uwt.Tcp.simultaneous_accepts true;
        if foobar () then (* no file descriptor yet assigned, no need to worry
                             about exceptions inside foobar,... *)
          Lwt.return_unit (* no need to close *)
        else
          ...
      ]}

      If you want - for whatever reason - keep a file descriptor open
      for the whole lifetime of your process, remember to keep a
      reference to its handle. *)

  val close_noerr : t -> unit

  val close_wait : t -> unit Lwt.t
  (** Prefer {!close} or {!close_noerr} to {!close_wait}. {!close} or
      {!close_noerr} return immediately (there are no useful error
      messages, beside perhaps a notice, that you've already closed that
      handle).

      {!close_wait} is only useful, if you intend to wait until all
      concurrent write and read threads related to this handle are
      canceled. *)

  val is_active : t -> bool
  (** Returns non-zero if the handle is active, zero if it's
      inactive. What "active" means depends on the type of handle:

      - A [Async.t] handle is always active and cannot be deactivated,
      except by closing it with uv_close().

      - A [Pipe.t], [Tcp.t], [Udp.t], etc. handle - basically any
      handle that deals with i/o - is active when it is doing something
      that involves i/o, like reading, writing, connecting, accepting
      new connections, etc.

      Rule of thumb: if a handle of type [Uwt.Foo.t] has a
      uv_foo_start() function, then it's active from the moment that
      function is called. Likewise, uv_foo_stop() deactivates the
      handle again. *)

  val ref': t -> unit
  (** Reference the given handle. References are idempotent, that is,
      if a handle is already referenced calling this function again
      will have no effect. *)

  val unref: t -> unit
  (** Un-reference the given handle. References are idempotent, that
      is, if a handle is not referenced calling this function again will
      have no effect.*)

  val has_ref: t -> bool
  (** Returns non-zero if the handle is referenced, zero otherwise. *)
end

module Handle_ext : sig
  type t

  val get_send_buffer_size : t -> Int_result.int
  (** Gets the size of the send buffer that the operating system uses
      for the socket.*)

  val get_send_buffer_size_exn : t -> int


  val get_recv_buffer_size : t -> Int_result.int
  (** Gets the size of the receive buffer that the operating system
      uses for the socket.*)

  val get_recv_buffer_size_exn : t -> int

  val set_send_buffer_size : t -> int -> Int_result.unit
  (** Sets the size of the send buffer that the operating system uses
      for the socket.*)

  val set_send_buffer_size_exn : t -> int -> unit

  val set_recv_buffer_size : t -> int -> Int_result.unit
  (** Sets the size of the receive buffer that the operating
      system uses for the socket. *)

  val set_recv_buffer_size_exn : t -> int -> unit
end

module Handle_fileno : sig
  (** The usage of {!fileno} is unsafe and strongly discouraged.
      But it's sometimes necessary, if you need to interact with third parties
      libraries. Rules:

      - You must still keep your original handle around.
        Otherwise uwt will close the handle when the garbage collector removes
        it.

      - Always close the handle with {!Handle.close} (or {!Tcp.close}, etc.),
        not [Unix.close] or any other function.

      - Never pass the file descriptor to functions like {!Pipe.openpipe}
        or {!Poll.start}. *)

  type t

  val fileno : t -> Unix.file_descr uv_result
  val fileno_exn : t -> Unix.file_descr
end

module Stream : sig
  (** Stream handles provide an abstraction of a duplex communication
      channel. [Stream.t] is an abstract type, libuv provides 3 stream
      implementations in the form of [Tcp.t], [Pipe.t] and [Tty.t]. *)

  type t
  include module type of Handle with type t := t

  val to_handle : t -> Handle.t

  val is_readable : t -> bool
  val is_writable : t -> bool

  val read_start : t -> cb:(Bytes.t uv_result -> unit) -> Int_result.unit
  (** Read data from an incoming stream. The [~cb] will
      be made several times until there is no more data to read or
      {!read_stop} is called. *)

  val read_start_exn : t -> cb:(Bytes.t uv_result -> unit) -> unit

  val read_stop : t -> Int_result.unit
  (** Stop reading data from the stream. *)

  val read_stop_exn : t -> unit

  val read : ?pos:int -> ?len:int -> t -> buf:bytes -> int Lwt.t
  (** There is currently no [uv_read] function in libuv, just [uv_read_start]
      and [uv_read_stop]. This is a wrapper for your convenience. It calls
      read_stop internally, if you don't continue with reading
      immediately. Zero result indicates EOF.

      In future libuv versions, there might be [uv_read] and
      [uv_try_read] functions (it was discussed several times).
      If these changes got merged, {!Stream.read} will wrap them - even
      if there will be small semantic differences.

      It is currently not possible to start several read threads
      in parallel, you must serialize the requests manually. In the
      following example [t2] will fail with EBUSY:

      {[
        let t1 = Uwt.Stream.read t ~buf:buf1 in
        let t2 = Uwt.Stream.read t ~buf:buf2 in
        (* ... *)
      ]}

      Calling the function with [~len:0] has a dubious, system dependent
      semantic. *)

  val read_ba : ?pos:int -> ?len:int -> t -> buf:buf -> int Lwt.t

  val write_queue_size : t -> int
  (** Returns the amount of queued bytes waiting to be sent *)

  val try_write : ?pos:int -> ?len:int -> t -> buf:bytes -> Int_result.int
  (** Write data to stream, but won't queue a write request if it
      can't be completed immediately.*)

  val try_write_ba: ?pos:int -> ?len:int -> t -> buf:buf -> Int_result.int
  val try_write_string: ?pos:int -> ?len:int -> t -> buf:string -> Int_result.int

  val write : ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t
  (** Write data to stream *)

  val write_string : ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t
  val write_ba : ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  val write_raw : ?pos:int -> ?len:int -> t -> buf:bytes -> unit Lwt.t
  (** {!write} is eager - like the counterparts inside [Lwt_unix].  It
      first calls {!try_write} internally to check if it can return
      immediately (without the overhead of creating a sleeping thread
      and waking it up later). If it can't write everything instantly,
      it will call {!write_raw} internally. {!write_raw} is exposed
      here mainly in order to write unit tests for it. But you can also
      use it, if you your [~buf] is very large or you know for another
      reason, that try_write will fail. *)

  val write_raw_string : ?pos:int -> ?len:int -> t -> buf:string -> unit Lwt.t
  val write_raw_ba : ?pos:int -> ?len:int -> t -> buf:buf -> unit Lwt.t

  val try_writev: t -> Iovec_write.t list -> Int_result.int
  (** Windows doesn't support writing multiple buffers with a single
      syscall for some HANDLEs (e.g. it's supported for tcp handles,
      but not pipes). uwt then writes the buffers one by one

      If the number of buffers is greater than IOV_MAX, libuv already
      contains the necessary workarounds *)

  val writev: t -> Iovec_write.t list -> unit Lwt.t
  (** See comment to {try_writev}! This function will fail with
      [Unix.EOPNOTSUPP] on Windows for e.g. pipe handles *)

  val writev_emul: t -> Iovec_write.t list -> unit Lwt.t
  (** Similar to {!writev}, but if passing several buffers at
      once is not supported by the OS, the buffers will be written
      one by one. Please note that as a consequence you should not start several
      {!writev_emul} threads in parallel. The writing order would be surprising
      in this case. If you don't use windows, this function is identic to
      {!writev} *)

  val writev_raw: t -> Iovec_write.t list -> unit Lwt.t

  val listen:
    t -> max:int -> cb:( t -> Int_result.unit -> unit ) -> Int_result.unit
  (** Start listening for incoming connections. [~max] indicates the
      number of connections the kernel might queue, same as
      [listen(2)]. When a new incoming connection is received [~cb] is
      called. *)

  val listen_exn :
    t -> max:int -> cb:( t -> Int_result.unit -> unit ) -> unit

  val shutdown: t -> unit Lwt.t
  (** Shutdown the outgoing (write) side of a duplex stream. It waits
      for pending write requests to complete. *)

  val set_blocking: t -> bool -> Int_result.unit
  (** Just don't use this function. It will only cause trouble. *)
end

module Tcp : sig
  (** TCP handles are used to represent both TCP streams and servers. *)

  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  include module type of Handle_fileno with type t := t
  val to_stream : t -> Stream.t

  val init : unit -> t
  (** See comment to {!Pipe.init} *)


  val init_ipv4 : unit -> t uv_result
  (** wrappers around [uv_tcp_init_ex]. A socket of the given type will be
      created immediately instead of lazy (as with {!init}) *)

  val init_ipv4_exn : unit -> t
  val init_ipv6 : unit -> t uv_result
  val init_ipv6_exn : unit -> t

  val opentcp : Unix.file_descr -> t uv_result
  (** See comment to {!Pipe.openpipe} *)

  val opentcp_exn : Unix.file_descr -> t

  type mode = Ipv6_only

  val bind : ?mode:mode list -> t -> addr:sockaddr -> unit -> Int_result.unit
  (** Bind the handle to an address and port.

      When the port is already taken, you can expect to see an
      [EADDRINUSE] error from either {!bind}, {!listen} or
      {!connect}. That is, a successful call to this function does not
      guarantee that the call to {!listen} or {!connect} will succeed
      as well.

      @param mode: default is the empty list *)

  val bind_exn : ?mode:mode list -> t -> addr:sockaddr -> unit -> unit

  val nodelay : t -> bool -> Int_result.unit
  (** Enable TCP_NODELAY, which disables Nagle's algorithm. *)

  val nodelay_exn : t -> bool -> unit

  val enable_keepalive : t -> int -> Int_result.unit
  (** [enable_keepalive tcp delay] enables keep-alive, the delay is the
      initial delay in seconds *)

  val enable_keepalive_exn : t -> int -> unit

  val disable_keepalive : t -> Int_result.unit
  val disable_keepalive_exn : t -> unit

  val simultaneous_accepts : t -> bool -> Int_result.unit
  (** Enable / disable simultaneous asynchronous accept requests that
      are queued by the operating system when listening for new TCP
      connections.

      This setting is used to tune a TCP server for the desired
      performance. Having simultaneous accepts can significantly
      improve the rate of accepting connections (which is why it is
      enabled by default) but may lead to uneven load distribution in
      multi-process setups. *)

  val simultaneous_accepts_exn : t -> bool -> unit

  val getsockname : t -> sockaddr uv_result
  (** Get the current address to which the handle is bound. *)

  val getsockname_exn : t -> sockaddr

  val getpeername : t -> sockaddr uv_result
  (** Get the address of the peer connected to the handle. *)

  val getpeername_exn : t -> sockaddr

  val connect : t -> addr:sockaddr -> unit Lwt.t
  (** Establish an IPv4 or IPv6 TCP connection. *)

  val accept: t -> t uv_result
  (** initializes a new client, accepts and returns it.

      This call is used in conjunction with {!listen} to accept
      incoming connections. Call this function after receiving a
      {!listen} callback to accept the connection.

      When the {!listen} callback is called it is guaranteed
      that this function will complete successfully the first time. If
      you attempt to use it more than once, it may fail. It is
      suggested to only call this function once per {!listen} callback
      call. *)

  val accept_exn: t -> t

  val with_tcp: (t -> 'a Lwt.t) -> 'a Lwt.t
  (** See comments to {!Pipe.with_pipe} *)

  val with_connect: addr:sockaddr -> (t -> 'a Lwt.t) -> 'a Lwt.t
  val with_open: Unix.file_descr -> (t -> 'a Lwt.t) -> 'a Lwt.t
  val with_accept: t -> (t -> 'a Lwt.t) -> 'a Lwt.t

  val accept_raw: server:t -> client:t -> Int_result.unit
  val accept_raw_exn: server:t -> client:t -> unit
end

module Udp : sig
  (** UDP handles encapsulate UDP communication for both clients and servers. *)

  type t
  include module type of Handle with type t := t
  include module type of Handle_ext with type t := t
  include module type of Handle_fileno with type t := t
  val to_handle : t -> Handle.t

  val send_queue_size: t -> int
  (** Number of bytes queued for sending; strictly shows how much
      information is currently queued. *)

  val send_queue_count: t -> int
  (** Number of send requests currently in the queue awaiting to be processed.*)

  val init : unit -> t
  (** See comment to {!Pipe.init} *)

  val init_ipv4 : unit -> t uv_result
  (** wrappers around [uv_udp_init_ex]. A socket of the given type will be
      created immediately instead of lazy ({!init}) *)

  val init_ipv4_exn: unit -> t
  val init_ipv6 : unit -> t uv_result
  val init_ipv6_exn : unit -> t

  val openudp : Unix.file_descr -> t uv_result
  (** See comment to {!Pipe.openpipe} *)

  val openudp_exn : Unix.file_descr -> t

  type mode =
    | Ipv6_only
    | Reuse_addr

  val bind : ?mode:mode list -> t -> addr:sockaddr -> unit -> Int_result.unit
  (** Bind the UDP handle to an IP address and port. @param mode
      default mode is the empty list *)

  val bind_exn : ?mode:mode list -> t -> addr:sockaddr -> unit -> unit

  val getsockname : t -> sockaddr uv_result
  (** Get the local IP and port of the UDP handle. *)

  val getsockname_exn : t -> sockaddr

  type membership =
    | Leave_group
    | Join_group

  val set_membership :
    ?interface:string -> t -> multicast:string -> membership -> Int_result.unit
  (** Set membership for a multicast address *)

  val set_membership_exn :
    ?interface:string -> t -> multicast:string -> membership -> unit

  val set_multicast_loop : t -> bool -> Int_result.unit
  (** Set IP multicast loop flag. Makes multicast packets loop back to
      local sockets *)

  val set_multicast_loop_exn : t -> bool -> unit

  val set_multicast_ttl : t -> int -> Int_result.unit
  (** Set the multicast ttl, ttl - 1 through 255. *)

  val set_multicast_ttl_exn : t -> int -> unit

  val set_multicast_interface : t -> string option -> Int_result.unit
  (** Set the multicast interface to send or receive data on *)

  val set_multicast_interface_exn : t -> string option -> unit

  val set_broadcast : t -> bool -> Int_result.unit
  (** Set broadcast on or off. *)

  val set_broadcast_exn : t -> bool -> unit

  val set_ttl : t -> int -> Int_result.unit
  (** Set the time to live. ttl - 1 through 255.*)

  val set_ttl_exn : t -> int -> unit

  val send : ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> unit Lwt.t
  (** Send data over the UDP socket. If the socket has not previously
      been bound with uv_udp_bind() it will be bound to 0.0.0.0 (the
      "all interfaces" IPv4 address) and a random port number. *)

  val send_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> unit Lwt.t

  val send_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> unit Lwt.t

  val send_raw :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> unit Lwt.t
  (** See comment to {!Stream.write_raw} *)

  val send_raw_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> unit Lwt.t

  val send_raw_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> unit Lwt.t

  val try_send :
    ?pos:int -> ?len:int -> buf:bytes -> t -> sockaddr -> Int_result.int
  (** Same as {!send}, but won't queue a send request if it can't be
      completed immediately. *)

  val try_send_ba :
    ?pos:int -> ?len:int -> buf:buf -> t -> sockaddr -> Int_result.int

  val try_send_string :
    ?pos:int -> ?len:int -> buf:string -> t -> sockaddr -> Int_result.int

  val try_sendv :
    t -> Iovec_write.t list -> sockaddr -> Int_result.int

  val sendv_raw:
    t -> Iovec_write.t list -> sockaddr -> unit Lwt.t

  val sendv:
    t -> Iovec_write.t list -> sockaddr -> unit Lwt.t

  type recv_result =
    | Data of Bytes.t * sockaddr option
    | Partial_data of Bytes.t * sockaddr option
    | Empty_from of sockaddr
    | Transmission_error of error
    (** The type definition will likely be changed.
        Don't use fragile pattern matching for it *)

  val recv_start : t -> cb:(recv_result -> unit) -> Int_result.unit
  (** Prepare for receiving data. If the socket has not previously been
      bound with uv_udp_bind() it is bound to 0.0.0.0 (the "all
      interfaces" IPv4 address) and a random port number.*)

  val recv_start_exn : t -> cb:(recv_result -> unit) -> unit

  val recv_stop : t -> Int_result.unit
  (** Stop listening for incoming datagrams. *)

  val recv_stop_exn : t -> unit

  type recv = {
    recv_len: int;
    is_partial: bool;
    sockaddr: sockaddr option;
  }

  val recv : ?pos:int -> ?len:int -> buf:bytes -> t -> recv Lwt.t
  (** Wrappers around {!recv_start} and {!recv_stop} for you convenience,
      no callback soup. ~len should be greater than zero.

      See also the comments to {!Stream.read}.
      Don't pass [~len:0] or an empty [buf] to recv. This case is
      captured by uwt/libuv, not your operating system :D *)

  val recv_ba : ?pos:int -> ?len:int -> buf:buf -> t -> recv Lwt.t

end

module Tty : sig
  (** TTY handles represent a stream for the console. *)

  type t
  include module type of Stream with type t := t
  include module type of Handle_fileno with type t := t
  val to_stream: t -> Stream.t

  val init : file -> read:bool -> t uv_result
  (** Initialize a new TTY stream with the given file
      descriptor. Usually the file descriptor will be:
      {!stdin},{!stdout} or {!stderr} [~read] specifies if you plan on
      calling {!read_start} with this stream. [stdin] is readable,
      [stdout] is not.

      On Unix this function will determine the path of the fd of the
      terminal using ttyname_r(3), open it, and use it if the passed
      file descriptor refers to a TTY. This lets libuv put the tty in
      non-blocking mode without affecting other processes that share
      the tty.

      This function is not thread safe on systems that don't support
      ioctl TIOCGPTN or TIOCPTYGNAME, for instance OpenBSD and Solaris.

      Note: If reopening the TTY fails, libuv falls back to blocking
      writes for non-readable TTY streams. *)

  val init_exn : file -> read:bool -> t

  type mode =
    | Normal
    | Raw
    | Io

  val set_mode : t -> mode:mode -> Int_result.unit
  (** Set the TTY using the specified terminal mode *)

  val set_mode_exn : t -> mode:mode -> unit

  val reset_mode : unit -> Int_result.unit
  (** To be called when the program exits. Resets TTY settings to
      default values for the next process to take over.

      This function is async signal-safe on Unix platforms but can
      fail with error code UV_EBUSY if you call it when execution is
      inside uv_tty_set_mode(). *)

  val reset_mode_exn : unit -> unit

  type winsize = {
    width: int;
    height: int;
  }

  val get_winsize : t -> winsize uv_result
  (** Gets the current Window size. *)

  val get_winsize_exn : t -> winsize
end


module Pipe : sig
  (** Pipe handles provide an abstraction over local domain sockets on
      Unix and named pipes on Windows. *)

  type t
  include module type of Stream with type t := t
  include module type of Handle_ext with type t := t
  include module type of Handle_fileno with type t := t
  val to_stream: t -> Stream.t

  val init : ?ipc:bool -> unit -> t
  (** The only thing that can go wrong, is memory allocation.
      In this case the ordinary exception [Out_of_memory] is thrown.
      The function is not called init_exn, because this exception can
      be thrown by nearly all functions.

      @param ipc is false by default
  *)

  val openpipe : ?ipc:bool -> Unix.file_descr -> t uv_result
  (**
     Be careful with open* functions. They exists, so you can re-use
     system dependent libraries. But if you pass a file descriptor
     to openpipe (or opentcp,...), that is not really a file descriptor of a
     pipe (or tcp socket,...) you can trigger assert failures inside libuv.
     @param ipc is false by default
  *)

  val openpipe_exn : ?ipc:bool -> Unix.file_descr -> t

  val bind: t -> path:string -> Int_result.unit
  (** Bind the pipe to a file path (Unix) or a name (Windows). *)

  val bind_exn : t -> path:string -> unit

  val getsockname: t -> string uv_result
  (** Get the name of the Unix domain socket or the named pipe.*)

  val getsockname_exn : t -> string


  val getpeername: t -> string uv_result
  (** Get the name of the Unix domain socket or the named pipe to
      which the handle is connected. *)

  val getpeername_exn : t -> string

  val pending_instances: t -> int -> Int_result.unit
  (** Set the number of pending pipe instance handles when the pipe
      server is waiting for connections.

      Note: This setting applies to Windows only.*)

  val pending_instances_exn : t -> int -> unit


  val accept: t -> t uv_result
  (** initializes a new client, accepts and returns it.

      This call is used in conjunction with {!listen} to accept
      incoming connections. Call this function after receiving a
      {!listen} callback to accept the connection.

      When the {!listen} callback is called it is guaranteed
      that this function will complete successfully the first time. If
      you attempt to use it more than once, it may fail. It is
      suggested to only call this function once per {!listen} callback
      call.

      Don't use this function for pipes that have been initialized with
      [~ipc:true]. It's not portable.
  *)

  val accept_exn: t -> t

  val accept_raw: server:t -> client:t -> Int_result.unit
  val accept_raw_exn: server:t -> client:t -> unit

  val pending_count: t -> Int_result.int
  val pending_count_exn : t -> int
  (** how many handles are waiting to be accepted with {!accept_ipc} *)

  type ipc_result =
    | Ipc_error of error (** internal call to accept failed *)
    | Ipc_none (** no pending handles *)
    | Ipc_tcp of Tcp.t
    | Ipc_udp of Udp.t
    | Ipc_pipe of t

  val accept_ipc: t -> ipc_result
  (** Used to receive handles over IPC pipes.

      It call's {!pending_count}, if it's > 0 then it initializes a
      handle of the appropriate type, accepts and returns it *)


  val write2 : ?pos:int -> ?len:int -> buf:bytes -> send:Tcp.t -> t -> unit Lwt.t
  (** Extended write function for sending handles over a pipe. The
      pipe must be initialized with [~ipc:true].

      Note: send_handle must be a TCP socket or pipe, which is a server
      or a connection (listening or connected state). Bound sockets or
      pipes will be assumed to be servers. *)

  val write2_ba : ?pos:int -> ?len:int -> buf:buf -> send:Tcp.t -> t ->
    unit Lwt.t
  val write2_string : ?pos:int -> ?len:int -> buf:string -> send:Tcp.t -> t ->
    unit Lwt.t


  val write2_pipe : ?pos:int -> ?len:int -> buf:bytes -> send:t -> t -> unit Lwt.t
  (** Similar to {!write2}, but the send handle is a pipe.
      Note: This is not supported on Windows *)

  val write2_pipe_ba : ?pos:int -> ?len:int -> buf:buf -> send:t -> t ->
    unit Lwt.t
  val write2_pipe_string : ?pos:int -> ?len:int -> buf:string -> send:t -> t ->
    unit Lwt.t

  val write2_udp : ?pos:int -> ?len:int -> buf:bytes -> send:Udp.t -> t
    -> unit Lwt.t
  (** Similar to {!write2}, but the send handle is an udp socket.
      Note: This is not portable at all *)

  val write2_udp_ba : ?pos:int -> ?len:int -> buf:buf -> send:Udp.t -> t ->
    unit Lwt.t
  val write2_udp_string : ?pos:int -> ?len:int -> buf:string -> send:Udp.t ->
    t -> unit Lwt.t

  val connect: t -> path:string -> unit Lwt.t
  (** Connect to the Unix domain socket or the named pipe.*)

  val with_pipe: ?ipc:bool -> (t -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_pipe ?ipc f] creates a new handle and passes
      the pipe to [f]. It is ensured that the pipe is closed when [f t]
      terminates (even if it fails).

      You can also close the pipe manually inside [f] without further
      consequences. *)

  val with_connect: path:string -> (t -> 'a Lwt.t) -> 'a Lwt.t
  val with_open: ?ipc:bool -> Unix.file_descr -> (t -> 'a Lwt.t) -> 'a Lwt.t

  type chmod =
    | Pipe_readable
    | Pipe_writeable
    | Pipe_readable_writeable

  val chmod : t -> chmod -> Int_result.unit
  (** Alters pipe permissions, allowing it to be accessed from
      processes run by different users. Makes the pipe writable or
      readable by all users. This function is blocking. *)

  val chmod_exn : t -> chmod -> unit
end


module Timer : sig
  (** Timer handles are used to schedule callbacks to be called in the future.*)

  type t
  include module type of Handle with type t := t
  val to_handle: t -> Handle.t

  val sleep : int -> unit Lwt.t
  (** [sleep d] is a thread that remains suspended for [d] milliseconds
      and then terminates. *)

  val start : repeat:int -> timeout:int -> cb:(t -> unit) -> t uv_result
  (** Timers, that are executed only once (repeat=0), are automatically closed.
      After their callback have been executed, their handles are invalid.
      Call {!close} to stop a repeating timer *)

  val start_exn : repeat:int -> timeout:int -> cb:(t -> unit) -> t
end

module Signal : sig
  (**
     Signal handles implement Unix style signal handling on a
     per-event loop bases.

     Reception of some signals is emulated on Windows:

     - SIGINT is normally delivered when the user presses
     CTRL+C. However, like on Unix, it is not generated when terminal
     raw mode is enabled.

     - SIGBREAK is delivered when the user pressed CTRL + BREAK.

     - SIGHUP is generated when the user closes the console window. On
     SIGHUP the program is given approximately 10 seconds to perform
     cleanup. After that Windows will unconditionally terminate it.

     Watchers for other signals can be successfully created, but these
     signals are never received. These signals are: SIGILL, SIGABRT,
     SIGFPE, SIGSEGV, SIGTERM and SIGKILL.

     Calls to raise() or abort() to programmatically raise a signal
     are not detected by libuv; these will not trigger a signal watcher.
  *)

  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  val sigbreak: int
  (** {!sigwinch} and {!sigbreak} are windows and libuv specific.
      Don't use them in any other context, don't pass them to
      {!Unix.kill} or similar functions. *)

  val sigwinch: int

  val start : int -> cb:(t -> int -> unit) -> t uv_result
  (** use {!Sys.sigterm}, {!Sys.sigstop}, etc *)

  val start_exn : int -> cb:(t -> int -> unit) -> t

  val oneshot : int -> unit Lwt.t
  (** Wait for the signal once and reset the moment the signal is received.
      Use Lwt.cancel to stop the signal handler *)
end

module Poll : sig
  (**
     Poll handles are used to watch file descriptors for readability,
     writability and disconnection similar to the purpose of poll(2).

     The purpose of poll handles is to enable integrating external
     libraries that rely on the event loop to signal it about the
     socket status changes, like c-ares or libssh2. Using [Poll.t] for
     any other purpose is not recommended; {!Tcp.t}, {!Udp.t},
     etc. provide an implementation that is faster and more scalable
     than what can be achieved with {!t}, especially on Windows.

     It is possible that poll handles occasionally signal that a file
     descriptor is readable or writable even when it isn't. The user
     should therefore always be prepared to handle EAGAIN or
     equivalent when it attempts to read from or write to the fd.

     It is not okay to have multiple active poll handles for the same
     socket, this can cause libuv to busyloop or otherwise
     malfunction.

     The user should not close a file descriptor while it is being
     polled by an active poll handle. This can cause the handle to
     report an error, but it might also start polling another
     socket. However the fd can be safely closed immediately after a
     call to {!close}.

     Note: On windows only sockets can be polled with poll handles. On
     Unix any file descriptor that would be accepted by poll(2) can be
     used.

     Note: On AIX, watching for disconnection is not supported.
  *)

  type t
  include module type of Handle with type t := t
  include module type of Handle_fileno with type t := t
  val to_handle : t -> Handle.t

  type event =
    | Readable
    | Writable
    | Disconnect
    | Prioritized

  val start : Unix.file_descr -> event list -> cb:(t -> event list uv_result -> unit) -> t uv_result
  (**

     Starts polling the file descriptor. events is a list of
     {!Readable}, {!Writable} and {!Disconnect}.  As soon as an event
     is detected the callback will be called with status set to 0, and
     the detected events set on the events field.

     The {!Disconnect} event is optional in the sense that it may not be
     reported and the user is free to ignore it, but it can help
     optimize the shutdown path because an extra read or write call
     might be avoided.
  *)

  val start_exn : Unix.file_descr -> event list -> cb:(t -> event list uv_result -> unit) -> t
end

module Fs_event : sig
  (**

     FS Event handles allow the user to monitor a given path for
     changes, for example, if the file was renamed or there was a
     generic change in it. This handle uses the best backend for the
     job on each platform. *)

  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type event =
    | Rename
    | Change

  type flags =
    | Entry   (** By default, if the fs event watcher is given a
                  directory name, we will watch for all events in that
                  directory. This flags overrides this behavior and
                  makes fs_event report only changes to the directory
                  entry itself. This flag does not affect individual
                  files watched. This flag is currently not implemented
                  yet on any backend. *)
    | Stat (** By default [Fs_event] will try to use a kernel
               interface such as inotify or kqueue to detect
               events. This may not work on remote filesystems such as
               NFS mounts. This flag makes fs_event fall back to
               calling stat() on a regular interval. This flag is
               currently not implemented yet on any backend.*)
    | Recursive (** By default, event watcher, when watching
                    directory, is not registering (is ignoring) changes
                    in it's subdirectories. This flag will override
                    this behaviour on platforms that support it. *)

  type cb = t -> (string * event list) uv_result -> unit

  val start : string -> flags list -> cb:cb -> t uv_result
  (** Start the handle with the given callback, which will watch the
      specified path for changes *)

  val start_exn : string -> flags list -> cb:cb -> t
end

module Fs_poll : sig
  (**
     FS Poll handles allow the user to monitor a given path for
     changes. Unlike {!Fs_event.t}, fs poll handles use stat to detect
     when a file has changed so they can work on file systems where fs
     event handles can't.
  *)

  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type report = {
    prev : Fs.stats;
    curr : Fs.stats;
  }

  val start : string -> int -> cb:(t -> report uv_result -> unit) -> t uv_result
  (** Check the file at path for changes every interval milliseconds.

      Note: For maximum portability, use multi-second
      intervals. Sub-second intervals will not detect all changes on many
      file systems. *)

  val start_exn : string -> int -> cb:(t -> report uv_result -> unit) -> t
end

module Process : sig
  (**
     Process handles will spawn a new process and allow the user to
     control it and establish communication channels with it using
     streams.
  *)

  type t
  include module type of Handle with type t := t
  val to_handle : t -> Handle.t

  type stdio =
    | Inherit_file of file
    | Create_pipe of Pipe.t
    | Inherit_pipe of Pipe.t
    | Inherit_stream of Stream.t
    | Create_pipe_read of Pipe.t
    | Create_pipe_write of Pipe.t
    | Create_pipe_duplex of Pipe.t
    (** [Create_pipe_read],[Create_pipe_write], etc. determine the
        direction of flow from the child process' perspective.
        Previously there was only [Create_pipe], which is identic to
        [Create_pipe_read] for [stdin] and [Create_pipe_write] for
        [stdout] and [stderr].

        This however doesn't allow you the specify a duplex data stream
        for inter-process communication (and the interface for it is not exposed
        otherwise for windows). Therefore [Create_pipe_duplex], etc. were
        added later. [Create_pipe] is now redundant, but left in place in order
        to not break existing code. *)

  type exit_cb = t -> exit_status:int -> term_signal:int -> unit

  val spawn :
    ?stdin:stdio ->
    ?stdout:stdio ->
    ?stderr:stdio ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?hide:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list ->
    t uv_result
  (**

     Initializes the process handle and starts the process.

     Possible reasons for failing to spawn would include (but not be
     limited to) the file to execute not existing, not having permissions
     to use the setuid or setgid specified, or not having enough memory
     to allocate for the new process.

     The first element of your argument list is supposed to be the
     path to the program.

     @param verbatim_arguments default false
     @param detach default false
     @param hide default true *)

  val spawn_exn :
    ?stdin:stdio ->
    ?stdout:stdio ->
    ?stderr:stdio ->
    ?uid:int ->
    ?gid:int ->
    ?verbatim_arguments:bool ->
    ?detach:bool ->
    ?hide:bool ->
    ?env:string list ->
    ?cwd:string ->
    ?exit_cb:exit_cb ->
    string ->
    string list ->
    t

  val disable_stdio_inheritance : unit -> unit
  (**
     Disables inheritance for file descriptors / handles that this
     process inherited from its parent. The effect is that child
     processes spawned by this process don't accidentally inherit
     these handles.

     It is recommended to call this function as early in your program
     as possible, before the inherited file descriptors can be closed
     or duplicated.

     Note: This function works on a best-effort basis: there is no
     guarantee that libuv can discover all file descriptors that were
     inherited. In general it does a better job on Windows than it
     does on Unix.
  *)

  val pid : t -> Int_result.int
  (** returns the PID of the spawned process *)

  val pid_exn : t -> int

  val process_kill : t -> int -> Int_result.unit
  (** Sends the specified signal to the given process handle. Check
      the documentation on {!Signal} for signal support, specially on
      Windows. *)

  val process_kill_exn : t -> int -> unit

  val kill : pid:int -> signum:int -> Int_result.unit
  (** Sends the specified signal to the given PID. Check the
      documentation on {!Signal} for signal support, specially on
      Windows. *)

  val kill_exn : pid:int -> signum:int -> unit
end

module Dns : sig
  (** libuv provides asynchronous variants of getaddrinfo and
      getnameinfo. *)

  (* these lines are only included because of ppx_import.
     Remove them later *)
  (**/**)
  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type getaddrinfo_option = Unix.getaddrinfo_option

  type addr_info = Unix.addr_info  = {
    ai_family : socket_domain;
    ai_socktype : socket_type;
    ai_protocol : int;
    ai_addr : sockaddr;
    ai_canonname : string;
  }
  (**/**)

  val getaddrinfo :
    host:string -> service:string ->
    getaddrinfo_option list -> Unix.addr_info list uv_result Lwt.t
  (**
     Asynchronous getaddrinfo(3).

     Either node or service may be NULL but not both.

     Be careful: {!getaddrinfo} returns raw error codes,
     whereas {!Unix.getaddrinfo} returns the empty list
  *)

  type getnameinfo_option = Unix.getnameinfo_option

  val getnameinfo : sockaddr -> getnameinfo_option list ->
    Unix.name_info uv_result Lwt.t
    (** Asynchronous getnameinfo(3). *)
end


module Unix : sig
  (**
     Popular functions from the standard Unix module, but all calls
     are executed in work threads.

     They are usually part of uwt, not libuv. They were added to
     simplify testing uwt with existing code that originally used
     [lwt.unix].

     But be careful: Functions like {!gethostbyname} don't fail with
     [Not_found], but with [Unix_error(Unix.ENOENT,"function_name",x)]
     or other appropriate error codes. {!Uwt_compat.Lwt_unix} provides
     functions under the same name that also fails with [Not_found].
  *)

  val gethostname : unit -> string Lwt.t
  (** Return the name of the local host. *)

  val gethostbyname: string -> Unix.host_entry Lwt.t
  (** Find an entry in [hosts] with the given name.*)

  val gethostbyaddr: Unix.inet_addr -> Unix.host_entry Lwt.t
  (** Find an entry in [hosts] with the given address.*)

  val getservbyname: name:string -> protocol:string -> Unix.service_entry Lwt.t
  (** Find an entry in [services] with the given name.*)

  val getservbyport: int -> string -> Unix.service_entry Lwt.t
  (** Find an entry in [services] with the given service number.*)

  val getprotobyname: string -> Unix.protocol_entry Lwt.t
  (** Find an entry in [protocols] with the given name.*)

  val getprotobynumber: int -> Unix.protocol_entry Lwt.t
  (** Find an entry in [protocols] with the given protocol number.*)

  val getlogin: unit -> string Lwt.t
  (** Return the login name of the user executing the process. *)

  val getpwnam: string -> Unix.passwd_entry Lwt.t
  (** Find an entry in [passwd] with the given name.*)

  val getpwuid: int -> Unix.passwd_entry Lwt.t
  (** Find an entry in [passwd] with the given user id.*)

  val getgrnam: string -> Unix.group_entry Lwt.t
  (** Find an entry in [group] with the given name.*)

  val getgrgid: int -> Unix.group_entry Lwt.t
  (** Find an entry in [group] with the given group id.*)

  val lseek: file -> int64 -> Unix.seek_command -> int64 Lwt.t
  (** Set the current position for a file descriptor, and return the
      resulting offset (from the beginning of the file). *)

  val getcwd: unit -> string Lwt.t
  (** Return the name of the current working directory. *)

  val chdir: string -> unit Lwt.t
  (** Change the process working directory. *)

  val chroot: string -> unit Lwt.t
  (** Change the process root directory. *)

  val lockf : file -> Unix.lock_command -> int64 -> unit Lwt.t
  (** See the long comment at the standard [Unix] module *)

  val sleep : float -> unit Lwt.t
  (** [sleep d] is a thread that remains suspended for [d] seconds
      and then terminates. *)

  val pipe : ?cloexec: bool -> unit -> (Pipe.t * Pipe.t) uv_result
  (** [pipe ()] creates pipes similar to [Unix.pipe], but as {Pipe.t}

      @param cloexec is true by default *)

  val pipe_exn : ?cloexec: bool -> unit -> Pipe.t * Pipe.t

  val realpath: string -> string Lwt.t
  [@@ocaml.deprecated "Use Uwt.Fs.realpath instead"]
  (** wrapper around realpath under Unix and GetFinalPathNameByHandleW
      or GetFullPathName under windows.
      Deprecated. Use Uwt.Fs.realpath instead, it will also work for
      older libuv versions. *)
end

module C_worker : sig
  (**

     You only need this module, if you intend to write
     C stub code that get's executed in libuv's threadprool.

     [examples/glob.ml] gives an example.
  *)
  type t
  type 'a u
  val call: ('a -> 'b u -> t) -> 'a -> 'b Lwt.t
end

module Async : sig
  (**
     Async handles allow the user to "wakeup" the event loop and get a callback
     called from another (system) thread.
  *)

  type t
  include module type of Handle with type t := t
  val to_handle: t -> Handle.t

  val create: ( t -> unit ) -> t uv_result
  (** Creates a new async handle. The handle is {b not} active immediately. You
      have to use {!start} *)

  val start: t -> Int_result.unit
  (** This will increase the reference count of the async handle. {!Main.run}
      will wait until [send] is called, if there are no other pendings tasks. *)

  val stop: t -> Int_result.unit
  (** Decrease the reference count again *)

  val send: t -> Int_result.unit
  (**
     Wakeup the event loop and call the async handle's callback.

     It's safe to call this function from any system thread. The callback
     will be called on the loop thread. {!create}, {!start}, and {!stop}
     however must be called from the main thread.

     Warning: libuv will coalesce calls to {!send}, that is, not every
     call to it will yield an execution of the callback. For example:
     if {!send} is called 5 times in a row before the callback is
     called, the callback will only be called once. If {!send} is
     called again after the callback was called, it will be called
     again. *)
end

module Debug : sig
  (**

     Common helper for ad hoc debugging. Don't use them inside
     production code.
  *)

  val print_all_handles: file -> Int_result.unit
  (** Prints all handles associated with the given loop to the given stream. *)

  val print_active_handles: file -> Int_result.unit
  (** This is the same as {!print_all_handles} except only active
      handles are printed. *)

  (**/**)
  (* Only for debugging.
     - Don't call it, while Main.run is active.
     - After you've called it, you can't run any uwt related functions *)
  val valgrind_happy : unit -> unit
  (**/**)
end

(**/**)
