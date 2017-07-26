open OUnit2

let l = [
  ("alloc_handle">::
   fun _ctx ->
     let n = 8192 in
     ignore ( Array.init n ( fun _i -> Uwt.Udp.init () ));
     ignore (Array.init n ( fun i ->
         let t = Uwt.Tcp.init () in
         Uwt.Tcp.nodelay_exn t (i mod 2 = 0);
       ));
     ignore (Array.init n ( fun _i -> Uwt.Pipe.init () ));
     assert_equal true true);
]

let l = "Gc">:::l
