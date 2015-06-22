let print_event_list l =
  let f = function
  | Uwt.Fs_event.Rename -> "Rename"
  | Uwt.Fs_event.Change -> "Change"
  in
  let s = List.map f l |> String.concat ", "  in
  "[ " ^ s ^ " ]"

let cb _t = function
| Uwt.Error x -> ignore (Uwt.strerror x |> Uwt_io.printf "error: %s\n")
| Uwt.Ok(fln,events) ->
  ignore (print_event_list events |> Uwt_io.printf "%s: %s\n%!" fln)

let () =
  let tmp_dir = Filename.get_temp_dir_name () in
  let l = Uwt.Fs_event.start_exn tmp_dir [] ~cb in
  let t = Lwt.finalize Help.wait (fun () -> Uwt.Fs_event.close_noerr l; Lwt.return_unit) in
  let t2 = Help.write_something tmp_dir in
  Uwt.Main.run (Lwt.pick [t ; t2])

let () = Help.clean ()
