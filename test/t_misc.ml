open Common
open OUnit2
open Uwt_base.Misc
(* Worthwile tests are not possible in most cases, because the
   functions are not implemented completely on all systems - and I
   don't know anything about the target system :) So I just access at
   least any generated value in each case, so I know, the c-stubs
   don't return garbage *)
let l = [
  ("resident_set_memory">:: fun _ ->
      assert_equal true (resident_set_memory_exn () > 64L ));
  ("uptime">:: fun _ -> assert_equal true (uptime_exn () > 20. ));
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
  ("os_homedir">:: fun _ ->
      let open Uwt in
      let p = match os_homedir () with
      | Ok "" -> false
      | Error UWT_EUNAVAIL ->
        let {major;minor;_} = version () in
        if major > 1 || minor >= 6 then
          false
        else
          true
      | Ok _ -> true
      | Error _ -> false
      in
      assert_equal true p);
  ("exepath">:: fun ctx ->
      (* doesn't work very well on various *nixes *)
      let do_skip = Uwt.Sys_info.(os <> Windows && os <> Linux) in
      skip_if_not_all ctx do_skip "exepath resolution differs";
      match exepath () with
      | Error x ->
        Uwt.err_name x |>
        Printf.sprintf "expath error:%s\n" |>
        failwith
      | Ok x ->
        let x1 = Filename.is_relative x
        and x2 = Filename.is_relative Sys.executable_name in
        skip_if_not_all ctx (x1 <> x2) "exepath not tracked";
        assert_equal Sys.executable_name x );
  ("win_version">:: fun _ ->
      let v = Uwt_base.Sys_info.win_version () in
      if not Sys.win32 then
        assert_equal v (Error Uwt.UWT_EUNAVAIL)
      else
        match v with
        | Error s ->
          let s = Uwt_base.err_name s in
          let msg = Printf.sprintf "win_version:%s" s in
          failwith msg
        | Ok x ->
          let slen = D.show_win_version x |> String.length in
          let open Uwt_base.Sys_info in
          assert_equal true ( slen > 30 && x.major_version >= 5 ));
]

let l = "Misc">:::l
