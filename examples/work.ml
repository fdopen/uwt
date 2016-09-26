open Lwt.Infix

let () =
  let t =
    Uwt.Unix.getservbyname ~name:"www" ~protocol:"tcp" >>= fun s ->
    Show_uwt.show_service_entry s |> Uwt_io.printl >>= fun () ->
    Uwt.Fs.openfile ~mode:[Uwt.Fs.O_RDONLY] Sys.executable_name >>= fun fd ->
    Uwt.Unix.lseek fd 0L Unix.SEEK_END >>= fun l ->
    Uwt_io.printf "%Ld\n" l >>= fun () ->
    Uwt.Unix.lseek fd 1_000L Unix.SEEK_SET >>= fun _l ->
    Uwt_io.printl "seek ok" >>= fun () ->
    let ch = Uwt_io.of_file ~mode:Uwt_io.input fd in
    Uwt_io.length ch >>= fun l ->
    Uwt_io.printf "%Ld\n" l >>= fun () ->
    Lwt.catch ( fun () ->
        Uwt.Unix.lseek (Obj.magic 1000) 0L Unix.SEEK_END >>= fun _l ->
        Uwt_io.printl "magic seek ok!"
      ) ( function
      | Unix.Unix_error(a,b,c) ->
        let a = Uwt.of_unix_error a in
        Uwt_io.printf "Unix_error(%s,%s,%s)\n" (Uwt.strerror a) b c
      | x -> Lwt.fail x )
  in
  Uwt.Main.run t

let () =
  flush stdout;
  flush stderr

let () = Uwt.Debug.valgrind_happy ()
