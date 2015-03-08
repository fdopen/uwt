open OUnit2

let l = [
  ("alloc">::
   fun _ctx ->
     let n = 8192 in
     let _b1 = Array.init n ( fun _i -> Uwt.Udp.init () ) in
     let _b2 = Array.init n ( fun i ->
         let t = Uwt.Tcp.init () in
         if i mod 2 = 0 then
           Uwt.Tcp.nodelay_exn t true
         else
           Uwt.Tcp.keepalive_exn t true;
       )
     in
     let _b3 = Array.init n ( fun _i -> Uwt.Pipe.init () ) in
     assert_equal true true)
]

let l = "Gc">:::l

