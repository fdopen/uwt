open OUnit2
open Lwt.Infix

module Req = struct
  type t
  external create: Uwt.loop -> int -> t = "uwt_req_create"
  external register_resource: t -> 'a -> unit = "uwt_test_req_leak"
end

let l = [
  ("alloc_handle">::
   fun _ctx ->
     let n = 8192 in
     ignore ( Array.init n ( fun _i -> Uwt.Udp.init () ));
     ignore (Array.init n ( fun i ->
         let t = Uwt.Tcp.init () in
         if i mod 2 = 0 then
           Uwt.Tcp.nodelay_exn t true
         else
           Uwt.Tcp.enable_keepalive_exn t 3;
       ));
     ignore (Array.init n ( fun _i -> Uwt.Pipe.init () ));
     assert_equal true true);
  ("alloc_req">::
   fun _ctx ->
     let result = ref None in
     let worker_thread () =
       let alloc () =
         let len = int_of_float (2. ** 18.) in
         let t =
           Array.init len ( fun _x ->
               Random.int 1024 |> Bytes.create )
         in
         ignore t
       in
       Thread.delay 0.05;
       Gc.compact ();
       alloc ();
       Gc.compact ()
     in
     for _i = 0 to 8192 ; do
       let r = Req.create Uwt.loop 3 in
       let t = Uwt_bytes.create 128 in
       Req.register_resource r t
     done;
     let t1 = Uwt.Timer.sleep 1_000_000 >|= fun () -> result:= Some false in
     let t2 =
       Uwt_preemptive.detach worker_thread ()
       >|= fun () -> result:= Some true in
     let t = Lwt.pick [ t1 ; t2 ] in
     Uwt.Main.run t;
     (* must be verified manually with tools like valgrind *)
     assert_equal !result (Some true);
  );
]

let l = "Gc">:::l
