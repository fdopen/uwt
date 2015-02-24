open OUnit2
open Lwt.Infix
open Common
open Uwt.Fs

let rec really_write ?(pos=0) ?len buf fd =
  let len = match len with
  | None -> Bytes.length buf
  | Some x -> x
  in
  Uwt.Fs.write fd ~buf ~pos ~len >>= fun n ->
  let len' = len - n in
  if len' <= 0 then
    Lwt.return_unit
  else
    really_write ~pos:(pos+n) ~len:len' buf fd

let file_to_bytes s =
  openfile ~mode:[ O_RDONLY ] s >>= fun fd ->
  Uwt.Unix.lseek fd 0L Unix.SEEK_END >>= fun file_len ->
  Uwt.Unix.lseek fd 0L Unix.SEEK_SET >>= fun _ ->
  let file_len = Int64.to_int file_len in
  let b = Buffer.create file_len in
  let buf = Bytes.create 8192 in
  let rec iter () =
    Uwt.Fs.read fd ~buf >>= fun n ->
    if n = 0 then
      close fd
    else (
      Buffer.add_subbytes b buf 0 n;
      iter ()
    )
  in
  iter () >>= fun () ->
  assert ( file_len = Buffer.length b);
  return (Buffer.to_bytes b)


let copy ~src ~dst =
  openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          let b_len = 65_536 in
          let buf = Bytes.create b_len in
          let rec read () =
            Uwt.Fs.read fd_read ~buf ~pos:0 ~len:b_len >>= fun n ->
            if n = 0 then
              Lwt.return_unit
            else
              write ~offset:0 ~len:n
          and write ~offset ~len =
            Uwt.Fs.write fd_write ~buf ~pos:offset ~len >>= fun n ->
            let len' = len - n in
            if len' <= 0 then
              read ()
            else
              write ~offset:(offset+n) ~len:len'
          in
          read ()
        ) ( fun () -> close fd_write )
    ) ( fun () -> close fd_read )

let copy_ba ~src ~dst =
  openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      Uwt.Fs.openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          let b_len = 65_536 in
          let buf = Uwt_bytes.create b_len in
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
  openfile ~mode:[ O_RDONLY ] src >>= fun fd_read ->
  Lwt.finalize ( fun () ->
      Uwt.Fs.openfile
        ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst >>= fun fd_write ->
      Lwt.finalize ( fun () ->
          Uwt.Fs.sendfile ~dst:fd_write ~src:fd_read () >>= fun _i ->
          Lwt.return_unit
        ) ( fun () -> Uwt.Fs.close fd_write )
    ) ( fun () -> Uwt.Fs.close fd_read )

let random_bytes = Bytes.init 262144 ( fun _i -> Random.int 256 |> Char.chr )
let tmpdir = ref "/tmp/invalid/invalid/invalid/invalid"

let (//) = Filename.concat
let l = [
  ("mkdtemp">::
   fun _ctx ->
     m_true (mkdtemp "uwt-test.XXXXXX" >>= fun s ->
             tmpdir:= s;
             at_exit ( fun () ->
                 let cmd = "rm -rf " ^ (Filename.quote s) in
                 Sys.command cmd |> ignore );
             return (s <> "")));
  ("write">::
   fun _ctx ->
     let fln = !tmpdir // "a" in
     m_equal () (
       openfile ~mode:[ O_WRONLY ; O_CREAT ; O_EXCL ] fln >>= fun fd ->
       really_write random_bytes fd >>= fun () ->
       close fd );
     m_equal random_bytes (file_to_bytes fln));
  ("read_ba/write_ba">::
   fun _ctx ->
     let fln = !tmpdir // "a" in
     let fln2 = !tmpdir // "b" in
     m_equal random_bytes (copy_ba ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("read/write">::
   fun _ctx ->
     let fln = !tmpdir // "a" in
     let fln2 = !tmpdir // "c" in
     m_equal random_bytes (copy ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("sendfile">::
   fun _ctx ->
     let fln = !tmpdir // "a" in
     let fln2 = !tmpdir // "d" in
     m_equal random_bytes (copy_sendfile ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("mkdir">::
   fun _ctx ->
     m_equal () (mkdir (!tmpdir // "f")));
  ("rmdir">::
   fun _ctx ->
     m_equal () (rmdir (!tmpdir // "f")));
  ("unlink">::
   fun _ctx ->
     m_equal () (unlink (!tmpdir // "d")));
  ("link">::
   fun _ctx ->
     skip_if Sys.win32 "win32";
     m_equal () (link ~target:(!tmpdir // "a") (!tmpdir // "f"));
     m_equal () (unlink (!tmpdir // "f")));
  ("scandir">::
   fun _ctx ->
     (* It's broken on windows, but fixed in trunk:
        https://github.com/libuv/libuv/issues/196 *)
     let files = [| S_REG, "a" ; S_REG, "b" ; S_REG, "c" |] in
     m_equal files (scandir !tmpdir >>= fun s -> Array.sort compare s ;
                    return s));
  ("symlink/lstat">::
   fun _ctx ->
     skip_if Sys.win32 "win32";
     let a = !tmpdir // "a"
     and d = !tmpdir // "d" in
     m_equal () (symlink ~target:a d);
     m_equal S_LNK (lstat d >>= fun s -> return s.st_kind );
     m_equal a (readlink d);
     m_equal () (unlink d));
]

let l = "Fs">:::l
