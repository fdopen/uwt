open OUnit2

let l = [
  ("alloc">::
   fun _ctx ->
     let n = 8192 in
     ignore ( Array.init n ( fun _i -> Uwt.Udp.init () ));
     ignore (Array.init n ( fun i ->
         let t = Uwt.Tcp.init () in
         if i mod 2 = 0 then
           Uwt.Tcp.nodelay_exn t true
         else
           Uwt.Tcp.keepalive_exn t true;
       ));
     ignore (Array.init n ( fun _i -> Uwt.Pipe.init () ));
     assert_equal true true)
]

let l = "Gc">:::l

