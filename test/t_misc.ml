open Common
open OUnit2
open Uv_misc
(* Worthwile tests are not possible in most cases, because the
   functions are not implemented completely on all systems - and I
   don't know anything about the target system :) So I just access at
   least any generated value in each case, so I know, the c-stubs
   don't return garbage *)
let l = [
  ("resident_set_memory">:: fun _ ->
      assert_equal true (resident_set_memory_exn () > 64n ));
  ("uptime">:: fun _ -> assert_equal true (uptime_exn () > 120. ));
  ("getrusage">:: fun _ ->
      let x = getrusage_exn () |> D.show_rusage |> String.length in
      assert_equal true ( x > 3 ));
  ("cpu_info">:: fun _ ->
      let x = cpu_info_exn () |> Array.map D.show_cpu_info |>
              Array.to_list |> String.concat "\n" |> String.length
      in
      assert_equal true ( x > 30 ));
  ("interface_addresses">:: fun _ ->
      let x = interface_addresses_exn () |>
              Array.map D.show_interface_address |> Array.to_list |>
              String.concat "\n" |> String.length in
      assert_equal true ( x > 30 ));
  ("load_avg">:: fun _ ->
      let (x,y,z) = load_avg () in
      let t = match Sys.win32 with
      | true -> x >= 0. && y >= 0. && z >= 0.
      | false -> x > 0. && y > 0. && z > 0.
      in
      assert_equal true t);
  ("ip4_addr">:: fun _ ->
      let ip4 = "127.0.2.1" in
      let ip4' = ip4_addr_exn ip4 99 |> ip4_name_exn in
      assert_equal ip4 ip4');
  ("ip6_addr">:: fun _ ->
      let ip6 = "2231:1fa8:45a3:3121:1333:8a2e:237a:733a" in
      let ip6' = ip6_addr_exn ip6 2199 |> ip6_name_exn in
      assert_equal ip6 ip6');
  ("get_total_memory">:: fun _ ->
      let p = get_total_memory () > 134217728L in
      assert_equal true p );
  ("hrtime">:: fun _ ->
      let p = hrtime () > 128L in
      assert_equal true p );
  ("version">:: fun _ ->
      let {major;minor;patch} = version () in
      let p = major + minor + patch >= 1 in
      assert_equal true p );
  ("version_string">:: fun _ ->
      let p = version_string () |> String.length > 3 in
      assert_equal true p );
]

let l = "Misc">:::l
