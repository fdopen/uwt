open Lwt.Infix
module UF = Uwt.Fs
open Uv.Fs

let copy ~src ~dst =
  UF.openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      UF.openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          let b_len = 65_536 in
          let buf = Bytes.create b_len in
          let rec read () =
            UF.read fd_read ~buf ~pos:0 ~len:b_len >>= fun n ->
            if n = 0 then
              Lwt.return_unit
            else
              write ~offset:0 ~len:n
          and write ~offset ~len =
            UF.write fd_write ~buf ~pos:offset ~len >>= fun n ->
            let len' = len - n in
            if len' <= 0 then
              read ()
            else
              write ~offset:(offset+n) ~len:len'
          in
          read ()
        ) ( fun () -> UF.close fd_write )
    ) ( fun () -> UF.close fd_read )

let copy_ba ~src ~dst =
  UF.openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      Uwt.Fs.openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          let b_len = 65_536 in
          let buf = Uv_bytes.create b_len in
          let rec read () =
            Uwt.Fs.read_ba fd_read ~buf ~pos:0 ~len:b_len >>= fun n ->
            if n = 0 then
              Lwt.return_unit
            else
              write ~offset:0 ~len:n
          and write ~offset ~len =
            Uwt.Fs.write_ba fd_write ~buf ~pos:offset ~len >>= fun n ->
            let len' = len - n in
            if len' <= 0 then
              read ()
            else
              write ~offset:(offset+n) ~len:len'
          in
          read ()
        ) ( fun () -> Uwt.Fs.close fd_write )
    ) ( fun () -> Uwt.Fs.close fd_read )

let copy_sendfile ~src ~dst =
  UF.openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      Uwt.Fs.openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          Uwt.Fs.sendfile ~dst:fd_write ~src:fd_read () >>= fun _i ->
          Lwt.return_unit
        ) ( fun () -> Uwt.Fs.close fd_write )
    ) ( fun () -> Uwt.Fs.close fd_read )

let () =
  let files = ref []
  and use_ba = ref false
  and use_sendfile = ref false in
  let speclist = [
    ("-b", Arg.Set use_ba, ": use bigarray as buffer");
    ("-s", Arg.Set use_sendfile, ": use sendfile") ]
  and usage = Sys.executable_name ^ " -o file1 file2"
  and anonf x = files:= x :: !files in
  Arg.parse speclist anonf usage;
  match List.rev !files with
  | src::dst::[] ->
    let cp =
      if !use_ba then
        copy_ba ~src ~dst
      else if !use_sendfile then
        copy_sendfile ~src ~dst
      else
        copy ~src ~dst
    in
    Uwt.Main.run cp
  | _ ->
    prerr_endline usage;
    exit 1

let () = Uwt.valgrind_happy ()
