open OUnit2
open Uwt.Conv

let l = [
  ("unix_sockaddr_ip4">::
   fun _ctx ->
     let i4 = Unix.inet_addr_of_string "127.0.2.9" in
     let sock4 = Unix.ADDR_INET (i4,99) in
     assert_equal sock4 (of_unix_sockaddr_exn sock4 |>
                         to_unix_sockaddr_exn));
  ("unix_sockaddr_ip6">::
   fun ctx ->
     Common.ip6_only ctx;
     let i6 = Unix.inet_addr_of_string "2001:ab9::fc00:41:9421" in
     let sock6 = Unix.ADDR_INET (i6,99) in
     assert_equal sock6 (of_unix_sockaddr_exn sock6 |>
                         to_unix_sockaddr_exn));
  ("unix_sockaddr_path">::
   fun ctx ->
     Common.no_win ctx;
     let sock = Unix.ADDR_UNIX "/tmp/uwt" in
     assert_equal sock (of_unix_sockaddr_exn sock |>
                         to_unix_sockaddr_exn));
]

let l = "Conv">:::l
