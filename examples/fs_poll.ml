module U = Uwt
module UP = U.Fs_poll

type file_kind = [%import: Uwt.Fs.file_kind] [@@deriving show]
type stats = [%import: Uwt.Fs.stats] [@@deriving show]

let cb _t = function
| U.Error x -> U.strerror x |> Uwt_io.printf "error: %s\n" |> ignore
| U.Ok x ->
  let open UP in
  let atime_diff =
    Printf.sprintf "atime-diff: %f\n" (x.curr.st_atime -. x.prev.st_atime)
  and mtime_diff =
    Printf.sprintf "mtime-diff: %f\n" (x.curr.st_mtime -. x.prev.st_mtime)
  and ctime_diff =
    Printf.sprintf "ctime-diff: %f\n" (x.curr.st_ctime -. x.prev.st_ctime)
  in
  let msg =  "previous:\n" ^ (show_stats x.prev) ^ "\ncurrent:\n" ^
             (show_stats x.curr) ^ "\n" ^ atime_diff ^ mtime_diff ^
             ctime_diff
  in
  Uwt_io.printl msg |> ignore

let observe fln timeout =
  let l = UP.start_exn fln timeout ~cb in
  Lwt.finalize Help.wait ( fun () -> UP.close l )

let () = U.Main.run (observe "/tmp" 2_000)

let () = Help.clean ()
