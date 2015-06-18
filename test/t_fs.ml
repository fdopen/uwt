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
            Uwt.Fs.fsync fd_write >>= fun () ->
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
            Uwt.Fs.fdatasync fd_write >>= fun () ->
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

let random_bytes_length = 262144
let random_bytes =
  Bytes.init random_bytes_length ( fun _i -> Random.int 256 |> Char.chr )
let invalid = "/tmp/invalid/invalid/invalid/invalid"
let tmpdir = ref invalid

let remove_dir ?(keep_root=false) orig_file =
  let rec iter file =
    let module U = Unix in
    match (U.stat file).U.st_kind with
    | exception _ -> ()
    | U.S_DIR ->
      Sys.readdir file |> Array.iter ( fun name ->
          Filename.concat file name |> iter );
      if keep_root = false || file <> orig_file then
        Unix.rmdir file |> ignore
    | U.S_REG | U.S_CHR | U.S_BLK | U.S_LNK | U.S_FIFO | U.S_SOCK ->
      if keep_root = false || file <> orig_file then
        Sys.remove file
  in
  iter orig_file

let () =
  let delme () =
    let d = !tmpdir in
    if d == invalid then
      ()
    else
      remove_dir d
  in
  Pervasives.at_exit delme

let clean_tmp_dir () =
  let d = !tmpdir in
  if d == invalid then
    ()
  else
    remove_dir ~keep_root:true d

let tmpdir () =
  if !tmpdir != invalid then
    !tmpdir
  else
    let dir = Filename.temp_file "uwt" ".tmp" in
    Unix.unlink dir;
    Unix.mkdir dir 0o755;
    let dir =
      if Filename.is_relative dir = false then
        dir
      else
        Filename.concat (Sys.getcwd ()) dir
    in
    tmpdir := dir;
    dir

let with_file ~mode fln f =
  openfile ~mode fln >>= fun fd ->
  Lwt.finalize
    ( fun () -> f fd )
    ( fun () -> close fd)

let (//) = Filename.concat
let l = [
  ("mkdtemp">::
   fun _ctx ->
     let () = clean_tmp_dir () in
     let fln = "uwt-test.XXXXXX" in
     m_true (mkdtemp fln >>= fun s ->
             remove_dir s;
             return (s <> "")));
  ("write">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     m_equal () (
       with_file ~mode:[ O_WRONLY ; O_CREAT ; O_EXCL ] fln @@ fun fd ->
       really_write random_bytes fd );
     m_equal random_bytes (file_to_bytes fln));
  ("read_ba/write_ba">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "b" in
     m_equal random_bytes (copy_ba ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("read/write">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "c" in
     m_equal random_bytes (copy ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("sendfile">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "d" in
     m_equal random_bytes (copy_sendfile ~src:fln ~dst:fln2 >>= fun () ->
                           file_to_bytes fln2));
  ("stat">::
   fun _ctx ->
     let fln = tmpdir () // "d" in
     m_true (stat fln >>= fun s -> Lwt.return (
         D.qstat s && s.st_kind = S_REG &&
         s.st_size = Int64.of_int random_bytes_length )));
  ("mkdir">::
   fun _ctx ->
     m_equal () @@ mkdir (tmpdir () // "f"));
  ("rmdir">::
   fun _ctx ->
     m_equal () @@ rmdir @@ tmpdir () // "f");
  ("unlink">::
   fun _ctx ->
     m_equal () @@ unlink @@ tmpdir () // "d");
  ("link">::
   fun _ctx ->
     no_win ();
     m_equal () @@ link ~target:(tmpdir() // "a") ~link_name:(tmpdir () // "f");
     m_equal () @@ unlink @@ tmpdir () // "f");
  ("scandir">::
   fun _ctx ->
     (* It's currently broken on windows, but fixed in trunk:
        https://github.com/libuv/libuv/issues/196 *)
     let files = [| S_REG, "a" ; S_REG, "b" ; S_REG, "c" |] in
     m_equal files (scandir (tmpdir()) >>= fun s -> Array.sort compare s ;
                    return s));
  ("symlink/lstat">::
   fun _ctx ->
     no_win ();
     let a = tmpdir () // "a"
     and d = tmpdir () // "d" in
     m_equal () (symlink ~src:a ~dst:d ());
     m_equal true (lstat d >>= fun s -> return (
         D.qstat s && s.st_kind = S_LNK));
     m_equal a (readlink d);
     m_equal () (unlink d));
  ("rename">::
   fun _ctx ->
     let a = tmpdir () // "a"
     and z = tmpdir () // "z" in
     m_equal () (rename ~src:a ~dst:z));
  ("utime">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     let itime = (int_of_float (Unix.time ())) - 99_000 in
     let time = float_of_int itime in
     m_true (utime z ~access:time ~modif:time >>= fun () ->
             stat z >>= fun s ->
             let time = Int64.of_int itime in
             let d1 = Int64.sub s.st_atime time |> Int64.abs
             and d2 = Int64.sub s.st_mtime time |> Int64.abs in
             return ( d1 = 0L && d2 = 0L ) ));
  ("futime/fstat">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     m_true ( with_file ~mode:[O_RDWR] z @@ fun fd ->
              let itime = (int_of_float (Unix.time ())) + 99_000 in
              let time = float_of_int itime in
              futime fd ~access:time ~modif:time >>= fun () ->
              fstat fd >>= fun s ->
              let time = Int64.of_float time in
              let d1 = Int64.sub s.st_atime time |> Int64.abs
              and d2 = Int64.sub s.st_mtime time |> Int64.abs in
              return ( D.qstat s && d1 = 0L && d2 = 0L ) ));
  ("chmod">::
   fun _ctx ->
     no_win ();
     let z = tmpdir () // "z" in
     m_true (chmod z ~perm:0o751 >>= fun () ->
             stat z >>= fun s -> return (s.st_perm = 0o751)));
  ("fchmod">::
   fun _ctx ->
     no_win ();
     let z = tmpdir () // "z" in
     m_true (with_file ~mode:[O_WRONLY] z @@ fun fd ->
             fchmod fd ~perm:0o621 >>= fun () ->
             fstat fd >>= fun s ->
             return (s.st_perm = 0o621)));
  ("access">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     let x = tmpdir () // "zz" in
     m_raises (Uwt.ENOENT,"access",x) (access x [Read]);
     m_equal () (access z [Read]);
     m_equal () (access Sys.executable_name [Exec]);
     no_win ();
     skip_if (Unix.getuid () = 0) "not for root";
     let invalid = "\000" in
     let shadow =
       if Sys.file_exists "/etc/shadow" then
         "/etc/shadow"
       else if Sys.file_exists "/etc/master.passwd" then
         "/etc/master.passwd"
       else
         invalid
     in
     skip_if (shadow == invalid) "no shadow";
     m_raises (Uwt.EACCES,"access",shadow) (access shadow [Read]));
  ("ftruncate">::
   fun _ctx ->
     let z = tmpdir() // "z" in
     m_true ( with_file ~mode:[O_RDWR] z @@ fun fd ->
              ftruncate fd ~len:777L >>= fun () ->
              fstat fd >>= fun s -> s.st_size = 777L |> return ));
]

let l = "Fs">:::l
