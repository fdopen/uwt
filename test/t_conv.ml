open OUnit2
open Uwt.Conv

let l = [
  ("unix_sockaddr_ip4">::
   fun _ctx ->
     let i4 = Unix.inet_addr_of_string "127.0.2.9" in
     let sock4 = Unix.ADDR_INET (i4,99) in
     assert_equal sock4 (sockaddr_of_unix_sockaddr sock4 |>
                         unix_sockaddr_of_sockaddr));
  ("unix_sockaddr_ip6">::
   fun _ctx ->
     let i6 = Unix.inet_addr_of_string "2001:ab9::fc00:41:9421" in
     let sock6 = Unix.ADDR_INET (i6,99) in
     assert_equal sock6 (sockaddr_of_unix_sockaddr sock6 |>
                         unix_sockaddr_of_sockaddr));
  ("unix_sockaddr_path">::
   fun _ctx ->
     Common.no_win ();
     let sock = Unix.ADDR_UNIX "/tmp/uwt" in
     assert_equal sock (sockaddr_of_unix_sockaddr sock |>
                         unix_sockaddr_of_sockaddr));
]

let l = "Conv">:::l
