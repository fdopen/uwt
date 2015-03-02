open OUnit2
open Uwt.Compat

let l = [
  ("unix_sockaddr_ip4">::
   fun _ctx ->
     let i4 = Unix.inet_addr_of_string "127.0.2.9" in
     let sock4 = Unix.ADDR_INET (i4,99) in
     assert_equal sock4 (of_unix_sockaddr sock4 |> to_unix_sockaddr));
  ("unix_sockaddr_ip6">::
   fun _ctx ->
     let i6 = Unix.inet_addr_of_string "2001:ab9::fc00:41:9421" in
     let sock6 = Unix.ADDR_INET (i6,99) in
     assert_equal sock6 (of_unix_sockaddr sock6 |> to_unix_sockaddr));
  ("unix_sockaddr_path">::
   fun _ctx ->
     Common.no_win ();
     let sock = Unix.ADDR_UNIX "/tmp/uwt" in
     assert_equal sock ( of_unix_sockaddr sock |> to_unix_sockaddr));
  ("socket_of_file_descr">::
   fun ctx ->
     let l = ref [] in
     let close_all do_raise =
       let rec iter accu = function
       | [] -> accu
       | hd::tl ->
         match Unix.close hd with
         | () -> iter accu tl
         | exception x ->
           match accu with
            | Some _ -> iter accu tl
            | None -> iter (Some x) tl
       in
       let l' = !l in
       l:= [];
       match iter None l' with
       | Some x when do_raise = true -> raise x
       | _ -> ()
     in
     try
       for i = 0 to 32 do
         let x =
           if Common.has_ip6 ctx && i mod 2 = 0 then
             Unix.socket Unix.PF_INET6 Unix.SOCK_STREAM 0
           else
             Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
         in
         l:= x::!l
       done;
       List.iter ( fun s ->
           assert_equal true ( match socket_of_file_descr s with
             | Some x -> Obj.is_int (Obj.magic x) | None -> false )) !l;
       close_all true;
     with
     | exn -> close_all false ; raise exn );
]

let l = "Compat">:::l
