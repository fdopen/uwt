module U = Uwt
module UP = U.Fs_poll

let cb _t = function
| Uwt.Error x -> ignore ( Uwt.strerror x |> Uwt_io.printf "error: %s\n")
| Uwt.Ok x ->
  let open UP in
  let open Show_uwt in
  let (-) = Int64.sub in
  let atime_diff =
    Printf.sprintf "atime-diff: %Ld\n" (x.curr.st_atime - x.prev.st_atime)
  and mtime_diff =
    Printf.sprintf "mtime-diff: %Ld\n" (x.curr.st_mtime - x.prev.st_mtime)
  and ctime_diff =
    Printf.sprintf "ctime-diff: %Ld\n" (x.curr.st_ctime - x.prev.st_ctime)
  in
  let msg =  "previous:\n" ^ (show_stats x.prev) ^ "\ncurrent:\n" ^
             (show_stats x.curr) ^ "\n" ^ atime_diff ^ mtime_diff ^
             ctime_diff
  in
  ignore (Uwt_io.printl msg)

let observe fln timeout =
  let l = UP.start_exn fln timeout ~cb in
  Lwt.finalize Help.wait ( fun () -> UP.close_noerr l ; Lwt.return_unit )

let () =
  let tmp_dir = Filename.get_temp_dir_name () in
  let t1 = observe tmp_dir 2_000 in
  let t2 = Help.write_something tmp_dir in
  U.Main.run (Lwt.pick [t1;t2])

let () = Help.clean ()
