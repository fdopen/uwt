let () =
  let f =
    let open Lwt.Infix in
    Glob.glob Sys.argv.(1) >>= function
    | None -> Lwt.fail_with "error in glob"
    | Some x ->
      Array.length x |> Uwt_io.printf "entries:%d\n"
      >>= fun () ->
      let i = ref 0 in
      let f l =
        incr i;
        Uwt_io.printf "%d: %s\n" !i l
      in
      Array.to_list x |> Lwt_list.iter_s f
  in
  Uwt.Main.run f

let () = Uwt.valgrind_happy ()
