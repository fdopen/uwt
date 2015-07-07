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

let with_file ~mode fln f =
  openfile ~mode fln >>= fun fd ->
  Lwt.finalize ( fun () -> f fd ) ( fun () -> close fd)

let file_to_bytes s =
  with_file ~mode:[ O_RDONLY ] s @@ fun fd ->
  Uwt.Unix.lseek fd 0L Unix.SEEK_END >>= fun file_len ->
  Uwt.Unix.lseek fd 0L Unix.SEEK_SET >>= fun _ ->
  let file_len = Int64.to_int file_len in
  let b = Buffer.create file_len in
  let buf = Bytes.create 8192 in
  let rec iter () =
    Uwt.Fs.read fd ~buf >>= fun n ->
    if n = 0 then
      Lwt.return_unit
    else (
      Buffer.add_subbytes b buf 0 n;
      iter ()
    )
  in
  iter () >|= fun () ->
  assert ( file_len = Buffer.length b);
  Buffer.to_bytes b

let copy ~src ~dst =
  with_file ~mode:[ O_RDONLY ] src @@ fun fd_read ->
  with_file ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst @@ fun fd_write ->
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

let copy_ba ~src ~dst =
  with_file ~mode:[ O_RDONLY ] src @@ fun fd_read ->
  with_file ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst @@ fun fd_write ->
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

let copy_sendfile ~src ~dst =
  with_file ~mode:[ O_RDONLY ] src @@ fun fd_read ->
  with_file ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst @@ fun fd_write ->
  Uwt.Fs.sendfile ~dst:fd_write ~src:fd_read () >>= fun _i ->
  Lwt.return_unit

let random_bytes_length = 228_409
let random_bytes = rbytes_create random_bytes_length

let (//) = Filename.concat
let l = [
  ("mkdtemp">::
   fun _ctx ->
     let () = clean_tmp_dir () in
     let fln = "uwt-test.XXXXXX" in
     m_true (mkdtemp fln >|= fun s -> remove_dir s; s <> ""));
  ("write">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let t =
       with_file ~mode:[ O_WRONLY ; O_CREAT ; O_EXCL ] fln @@ fun fd ->
       really_write random_bytes fd
     in
     m_equal () t;
     m_equal random_bytes (file_to_bytes fln));
  ("read_ba/write_ba">::
   fun _ctx ->
     let src = tmpdir () // "a"
     and dst = tmpdir () // "b" in
     let t = copy_ba ~src ~dst >>= fun () -> file_to_bytes dst in
     m_equal random_bytes t);
  ("read/write">::
   fun _ctx ->
     let src = tmpdir () // "a"
     and dst = tmpdir () // "c" in
     let t = copy ~src ~dst >>= fun () -> file_to_bytes dst in
     m_equal random_bytes t);
  ("sendfile">::
   fun _ctx ->
     let src = tmpdir () // "a"
     and dst = tmpdir () // "d" in
     let t = copy_sendfile ~src ~dst >>= fun () -> file_to_bytes dst in
     m_equal random_bytes t);
  ("stat">::
   fun _ctx ->
     let fln = tmpdir () // "d" in
     let t =
       stat fln >|= fun s ->
       D.qstat s && s.st_kind = S_REG &&
       s.st_size = Int64.of_int random_bytes_length
     in
     m_true t);
  ("mkdir">:: fun _ctx -> m_equal () @@ mkdir (tmpdir () // "f"));
  ("rmdir">:: fun _ctx -> m_equal () @@ rmdir @@ tmpdir () // "f");
  ("unlink">:: fun _ctx -> m_equal () @@ unlink @@ tmpdir () // "d");
  ("link">::
   fun ctx ->
     no_win ctx;
     m_equal () @@ link ~target:(tmpdir() // "a") ~link_name:(tmpdir () // "f");
     m_equal () @@ unlink @@ tmpdir () // "f");
  ("scandir">::
   fun _ctx ->
     (* It's currently broken on windows, but fixed in trunk:
        https://github.com/libuv/libuv/issues/196 *)
     let files = [| S_REG, "a" ; S_REG, "b" ; S_REG, "c" |] in
     m_equal files (scandir (tmpdir()) >|= fun s -> Array.sort compare s ; s));
  ("symlink/lstat">::
   fun ctx ->
     no_win ctx;
     let a = tmpdir () // "a"
     and d = tmpdir () // "d" in
     m_equal () (symlink ~src:a ~dst:d ());
     m_equal true (lstat d >|= fun s -> D.qstat s && s.st_kind = S_LNK);
     m_equal a (readlink d);
     m_equal () (unlink d));
  ("rename">::
   fun _ctx ->
     let src = tmpdir () // "a"
     and dst = tmpdir () // "z" in
     m_equal () (rename ~src ~dst));
  ("utime">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     let time = Unix.time () -. 99_000. in
     let t =
       utime z ~access:time ~modif:time >>= fun () ->
       stat z >|= fun s ->
       let time = Int64.of_float time in
       let d1 = Int64.sub s.st_atime time |> Int64.abs
       and d2 = Int64.sub s.st_mtime time |> Int64.abs in
       d1 = 0L && d2 = 0L
     in
     m_true t);
  ("futime/fstat">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     let t =
       with_file ~mode:[O_RDWR] z @@ fun fd ->
       let time = Unix.time () +. 99_000. in
       futime fd ~access:time ~modif:time >>= fun () ->
       fstat fd >|= fun s ->
       let time = Int64.of_float time in
       let d1 = Int64.sub s.st_atime time |> Int64.abs
       and d2 = Int64.sub s.st_mtime time |> Int64.abs in
       D.qstat s && d1 = 0L && d2 = 0L
     in
     m_true t);
  ("chmod">::
   fun ctx ->
     no_win ctx;
     let z = tmpdir () // "z"
     and p = 0o751 in
     m_true (chmod z ~perm:p >>= fun () -> stat z >|= fun s -> s.st_perm = p));
  ("fchmod">::
   fun ctx ->
     no_win ctx;
     let z = tmpdir () // "z"
     and perm = 0o621 in
     let t =
       with_file ~mode:[O_WRONLY] z @@ fun fd ->
       fchmod fd ~perm >>= fun () ->
       fstat fd >|= fun s ->
       s.st_perm = perm
     in
     m_true t);
  ("access">::
   fun ctx ->
     let z = tmpdir () // "z" in
     let x = tmpdir () // "zz" in
     m_raises (Uwt.ENOENT,"access",x) (access x [Read]);
     m_equal () (access z [Read]);
     m_equal () (access Sys.executable_name [Exec]);
     no_win ctx;
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
     let z = tmpdir() // "z"
     and len = 777L in
     let t =
       with_file ~mode:[O_RDWR] z @@ fun fd ->
       ftruncate fd ~len >>= fun () ->
       fstat fd >|= fun s ->
       s.st_size = len
     in
     m_true t);
]

let l = "Fs">:::l
