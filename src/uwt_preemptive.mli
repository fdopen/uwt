(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(** This module allows to mix preemptive threads with [Lwt]
    cooperative threads. It maintains an extensible pool of preemptive
    threads to which you can detach computations. *)

val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
  (** [detach f x] runs the computation [f x] in a separate preemptive thread.
      [detach] evaluates to an Lwt thread, which waits for the preemptive thread
      to complete.

      Note that Lwt thread-local storage (i.e., [Lwt.with_key]) cannot be safely
      used from within [f x]. The same goes for most of the rest of Lwt. If you
      need to run an Lwt thread in [f x], use {!run_in_main}. *)

val run_in_main : (unit -> 'a Lwt.t) -> 'a
  (** [run_in_main f] can be called from a detached computation to execute
      [f ()] in the main preemptive thread, i.e. the one executing
      {!Lwt_main.run}. [run_in_main f] blocks until [f ()] completes, then
      returns its result. If [f ()] raises an exception, [run_in_main f] raises
      the same exception.

      [Lwt.with_key] may be used inside [f ()]. [Lwt.get] can correctly retrieve
      values set inside [f ()], but not values set outside the [detach]
      invocation that is calling [run_in_main]. *)

val init : int -> int -> (string -> unit) -> unit
  (** [init min max log] initialises this module. i.e. it launches the
      minimum number of preemptive threads and starts the {b
      dispatcher}.

      @param min is the minimum number of threads
      @param max is the maximum number of threads
      @param log is used to log error messages

      If {!Lwt_preemptive} has already been initialised, this call
      only modify bounds and the log function, and return the
      dispatcher thread. *)

val simple_init : unit -> unit
  (** [simple_init ()] does a {i simple initialization}. i.e. with
      default parameters if the library is not yet initialised.

      Note: this function is automatically called {!detach}. *)

val get_bounds : unit -> int * int
  (** [get_bounds ()] returns the minimum and the maximum number of
      preemptive threads. *)

val set_bounds : int * int -> unit
  (** [set_bounds (min, max)] set the minimum and the maximum number
      of preemptive threads. *)

val set_max_number_of_threads_queued : int -> unit
  (** Sets the size of the waiting queue, if no more preemptive
      threads are available. When the queue is full, {!detach} will
      sleep until a thread is available. *)

val get_max_number_of_threads_queued : unit -> int
  (** Returns the size of the waiting queue, if no more threads are
      available *)

(**/**)
val nbthreads : unit -> int
val nbthreadsbusy : unit -> int
val nbthreadsqueued : unit -> int
