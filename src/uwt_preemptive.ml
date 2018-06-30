(* This file is part of uwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/fdopen/uwt/blob/master/LICENSE.md. *)

(* [Lwt_sequence] is deprecated - we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

open Lwt.Infix

(* Informations about a notifier *)
type notifier = {
  notify_handler : unit -> unit;
  (* The callback *)

  notify_once : bool;
  (* Whether to remove the notifier after the reception of the first
     notification *)
}

module Notifiers = Hashtbl.Make(struct
                                  type t = int
                                  let equal (x : int) (y : int) = x = y
                                  let hash (x : int) = x
                                end)

let notifiers = Notifiers.create 64
let current_notification_id = ref 0

let rec find_free_id id =
  if Notifiers.mem notifiers id then
    find_free_id (id + 1)
  else
    id

let make_notification ?(once=false) f =
  let id = find_free_id (!current_notification_id + 1) in
  current_notification_id := id;
  Notifiers.add notifiers id { notify_once = once; notify_handler = f };
  id

let notification_mutex = Mutex.create ()
let notification_queue = Queue.create ()

let rec call_notifications async_handle =
  let module T = struct
    type t =  | Non_s | Non_c | Ok_s of (unit -> unit) | Ok_c of (unit -> unit)
              | Exn of exn end
  in
  let open T in
  Mutex.lock notification_mutex;
  let f =
    try
      if Queue.is_empty notification_queue then
        Non_s
      else
        let id = Queue.pop notification_queue in
        let stop = Queue.is_empty notification_queue in
        match Notifiers.find notifiers id with
        | notifier ->
          if notifier.notify_once then
            Notifiers.remove notifiers id;
          if stop then
            Ok_s notifier.notify_handler
          else
            Ok_c notifier.notify_handler
        | exception Not_found -> if stop then Non_s else Non_c
    with
    | e -> Exn e
  in
  Mutex.unlock notification_mutex;
  match f with
  | Exn e -> raise e
  | Non_s -> ()
  | Non_c -> call_notifications async_handle
  | Ok_s f -> f ()
  | Ok_c f ->
    (match f () with
    | exception x -> !Lwt.async_exception_hook x
    | () -> ());
    call_notifications async_handle

let async_handle =
  match Uwt.Async.create call_notifications with
  | Error _ -> failwith "can't create async handle for uwt_preemptive"
  | Ok x -> x

(*
let () =
  Uwt.Main.at_exit ( fun () -> Uwt.Async.close_noerr async_handle;
                     Lwt.return_unit )
*)

let send_notification (a:int) =
  Mutex.lock notification_mutex;
  Queue.push a notification_queue;
  (* It won't fail, async_handle is never closed. *)
  let _ : Uwt.Int_result.unit = Uwt.Async.send async_handle in
  Mutex.unlock notification_mutex

(* +-----------------------------------------------------------------+
   | Parameters                                                      |
   +-----------------------------------------------------------------+ *)

(* Minimum number of preemptive threads: *)
let min_threads : int ref = ref 0

(* Maximum number of preemptive threads: *)
let max_threads : int ref = ref 0

(* Size of the waiting queue: *)
let max_thread_queued = ref 1000

let get_max_number_of_threads_queued _ =
  !max_thread_queued

let set_max_number_of_threads_queued n =
  if n < 0 then invalid_arg "Uwt_preemptive.set_max_number_of_threads_queued";
  max_thread_queued := n

(* The total number of preemptive threads currently running: *)
let threads_count = ref 0

(* +-----------------------------------------------------------------+
   | Preemptive threads management                                   |
   +-----------------------------------------------------------------+ *)

module CELL :
sig
  type 'a t

  val make : unit -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end =
struct
  type 'a t = {
    m  : Mutex.t;
    cv : Condition.t;
    mutable cell : 'a option;
  }

  let make () = { m = Mutex.create (); cv = Condition.create (); cell = None }

  let get t =
    let rec await_value t =
      match t.cell with
      | None ->
        Condition.wait t.cv t.m;
        await_value t
      | Some v ->
        t.cell <- None;
        Mutex.unlock t.m;
        v
    in
    Mutex.lock t.m;
    await_value t

  let set t v =
    Mutex.lock t.m;
    t.cell <- Some v;
    Mutex.unlock t.m;
    Condition.signal t.cv
end

type thread = {
  task_cell: (int * (unit -> unit)) CELL.t;
  (* Channel used to communicate notification id and tasks to the
     worker thread. *)

  mutable thread : Thread.t;
  (* The worker thread. *)

  mutable reuse : bool;
  (* Whether the thread must be readded to the pool when the work is
     done. *)
}

(* Pool of worker threads: *)
let workers : thread Queue.t = Queue.create ()

(* Queue of clients waiting for a worker to be available: *)
let waiters : thread Lwt.u Lwt_sequence.t = Lwt_sequence.create ()

(* Code executed by a worker: *)
let rec worker_loop worker =
  let id, task = CELL.get worker.task_cell in
  task ();
  (* If there is too much threads, exit. This can happen if the user
       decreased the maximum: *)
  if !threads_count > !max_threads then worker.reuse <- false;
  (* Tell the main thread that work is done: *)
  send_notification id;
  if worker.reuse then worker_loop worker

(* create a new worker: *)
let make_worker () =
  incr threads_count;
  let worker = {
    task_cell = CELL.make ();
    thread = Thread.self ();
    reuse = true;
  } in
  worker.thread <- Thread.create worker_loop worker;
  worker

(* Add a worker to the pool: *)
let add_worker worker =
  match Lwt_sequence.take_opt_l waiters with
  | None ->
    Queue.add worker workers
  | Some w ->
    Lwt.wakeup w worker

(* Wait for worker to be available, then return it: *)
let get_worker () =
  if not (Queue.is_empty workers) then
    Lwt.return (Queue.take workers)
  else if !threads_count < !max_threads then
    Lwt.return (make_worker ())
  else
    (Lwt.add_task_r [@ocaml.warning "-3"]) waiters

(* +-----------------------------------------------------------------+
   | Initialisation, and dynamic parameters reset                    |
   +-----------------------------------------------------------------+ *)

let get_bounds () = (!min_threads, !max_threads)

let set_bounds (min, max) =
  if min < 0 || max < min then invalid_arg "Uwt_preemptive.set_bounds";
  let diff = min - !threads_count in
  min_threads := min;
  max_threads := max;
  (* Launch new workers: *)
  for _i = 1 to diff do
    add_worker (make_worker ())
  done

let initialized = ref false

let init min max _errlog =
  initialized := true;
  set_bounds (min, max)

let simple_init () =
  if not !initialized then begin
    initialized := true;
    set_bounds (0, 4)
  end

let nbthreads () = !threads_count
let nbthreadsqueued () = Lwt_sequence.fold_l (fun _ x -> x + 1) waiters 0
let nbthreadsbusy () = !threads_count - Queue.length workers

(* +-----------------------------------------------------------------+
   | Detaching                                                       |
   +-----------------------------------------------------------------+ *)

let init_result = Error (Failure "Uwt_preemptive.detach")

let detached_cnt = ref 0
let detach f args =
  simple_init ();
  let result = ref init_result in
  (* The task for the worker thread: *)
  let task () =
    try
      result := Ok (f args)
    with exn ->
      result := Error exn
  in
  get_worker () >>= fun worker ->
  let waiter, wakener = Lwt.wait () in
  let id =
    make_notification ~once:true
      (fun () -> Lwt.wakeup_result wakener !result) in
  Lwt.finalize
    (fun () ->
       incr detached_cnt;
       let _ : Uwt.Int_result.unit = Uwt.Async.start async_handle in
       (* Send the id and the task to the worker: *)
       CELL.set worker.task_cell (id, task);
       waiter)
    (fun () ->
       let erg =
         try
           if worker.reuse then
             (* Put back the worker to the pool: *)
             add_worker worker
           else begin
             decr threads_count;
             (* Or wait for the thread to terminates, to free its associated
                  resources: *)
             Thread.join worker.thread
           end;
           None
         with
         | e -> Some e
       in
       decr detached_cnt;
       if !detached_cnt = 0 then (
         let _ : Uwt.Int_result.unit = Uwt.Async.stop async_handle in
         () );
       match erg with
       | None -> Lwt.return_unit
       | Some x -> Lwt.fail x)

(* +-----------------------------------------------------------------+
   | Running Lwt threads in the main thread                          |
   +-----------------------------------------------------------------+ *)

(* Queue of [unit -> unit Lwt.t] functions. *)
let jobs = Queue.create ()

(* Mutex to protect access to [jobs]. *)
let jobs_mutex = Mutex.create ()

let job_notification =
  make_notification
    (fun () ->
       (* Take the first job. The queue is never empty at this
          point. *)
       Mutex.lock jobs_mutex;
       let thunk = Queue.take jobs in
       Mutex.unlock jobs_mutex;
       ignore (thunk ()))

let run_in_main f =
  let cell = CELL.make () in
  (* Create the job. *)
  let job () =
    (* Execute [f] and wait for its result. *)
    Lwt.try_bind f
      (fun ret -> Lwt.return (Ok ret))
      (fun exn -> Lwt.return (Error exn)) >>= fun result ->
    (* Send the result. *)
    CELL.set cell result;
    Lwt.return_unit
  in
  (* Add the job to the queue. *)
  Mutex.lock jobs_mutex;
  Queue.add job jobs;
  Mutex.unlock jobs_mutex;
  (* Notify the main thread. *)
  send_notification job_notification;
  (* Wait for the result. *)
  match CELL.get cell with
  | Ok ret -> ret
  | Error exn -> raise exn
