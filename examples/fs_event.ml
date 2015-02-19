module U = Uwt
module UE = U.Fs_event

let print_event_list l =
  let f = function
  | UE.Rename -> "Rename"
  | UE.Change -> "Change"
  in
  let s = List.map f l |> String.concat ", "  in
  "[ " ^ s ^ " ]"

let cb _t = function
| U.Error x -> U.strerror x |> Uwt_io.printf "error: %s\n" |> ignore
| U.Ok(fln,events) ->
  print_event_list events |> Uwt_io.printf "%s: %s\n%!" fln |> ignore

let () =
  let l = UE.start_exn "/tmp" [] ~cb in
  let t = Lwt.finalize Help.wait (fun () -> UE.close l) in
  U.Main.run t

let () = Help.clean ()
