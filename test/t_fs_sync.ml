open OUnit2
open Uwt_base
module US = Uv_fs_sync
open Uv_fs_sync

let bind v f = match v with Ok v -> f v | Error _ as e -> e
let ( >>= ) = bind

let no_win = Common.no_win

let rec really_write ?(pos=0) ?len buf fd =
  let len = match len with
  | None -> Bytes.length buf
  | Some x -> x
  in
  US.write fd ~buf ~pos ~len >>= fun n ->
  let len' = len - n in
  if len' <= 0 then
    Ok ()
  else
    really_write ~pos:(pos+n) ~len:len' buf fd

let with_file ~mode fln f =
  openfile ~mode fln >>= fun fd ->
  let close_called = ref false in
  try
    let erg = f fd in
    close_called:= true;
    close fd >>= fun () ->
    erg
  with
  | exn when !close_called = false ->
    (try ignore (close fd) with Uwt_base.Uwt_error _ -> ());
    raise exn

let file_to_bytes s =
  with_file ~mode:[ O_RDONLY ] s @@ fun fd ->
  let fd' : Unix.file_descr = match Uwt.Conv.file_descr_of_file fd with
  | None -> assert false;
  | Some ok -> ok
  in
  let file_len = Unix.LargeFile.lseek fd' 0L Unix.SEEK_END in
  let _ : int64 = Unix.LargeFile.lseek fd' 0L Unix.SEEK_SET in
  let file_len = Int64.to_int file_len in
  let b = Buffer.create file_len in
  let buf = Bytes.create 8192 in
  let rec iter () =
    US.read fd ~buf >>= fun n ->
    if n = 0 then
      Ok ()
    else (
      Buffer.add_subbytes b buf 0 n;
      iter ()
    )
  in
  iter () >>= fun () ->
  assert ( file_len = Buffer.length b);
  Ok (Buffer.to_bytes b)

let copy ~src ~dst =
  with_file ~mode:[ O_RDONLY ] src @@ fun fd_read ->
  with_file ~mode:[ O_WRONLY ; O_CREAT ; O_TRUNC ] dst @@ fun fd_write ->
  let b_len = 65_536 in
  let buf = Bytes.create b_len in
  let rec read () =
    US.read fd_read ~buf ~pos:0 ~len:b_len >>= fun n ->
    if n = 0 then
      Ok ()
    else
      write ~offset:0 ~len:n
  and write ~offset ~len =
    US.write fd_write ~buf ~pos:offset ~len >>= fun n ->
    US.fsync fd_write >>= fun () ->
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
    US.read_ba fd_read ~buf ~pos:0 ~len:b_len >>= fun n ->
    if n = 0 then
      Ok ()
    else
      write ~offset:0 ~len:n
  and write ~offset ~len =
    US.write_ba fd_write ~buf ~pos:offset ~len >>= fun n ->
    US.fdatasync fd_write >>= fun () ->
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
  US.sendfile ~dst:fd_write ~src:fd_read () >>= fun _i ->
  Ok ()

let random_bytes_length = 88_411
let random_bytes = Common.rbytes_create random_bytes_length

let to_exn = function
| Ok x -> x
| Error s -> raise (Uwt_error(s,"",""))

let return s = Ok s
let m_equal s t = assert_equal s (t () |> to_exn )
let m_true t = m_equal true t

let m_raises a (t: unit -> 'a) =
  assert_raises
    (Uwt.Uwt_error(a,"",""))
    ( fun () -> t () |> to_exn )

let tmpdir = Common.tmpdir

let (//) = Filename.concat
let l = [
  ("mkdtemp">::
   fun _ctx ->
     let () = Common.clean_tmp_dir () in
     let fln = "uwt-test.XXXXXX" in
     m_true ( fun () -> mkdtemp fln >>= fun s ->
              Common.remove_dir s;
              return (s <> "")));
  ("write">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     m_equal () ( fun () ->
       with_file  ~mode:[ O_WRONLY ; O_CREAT ; O_EXCL ] fln @@ fun fd ->
       really_write random_bytes fd );
     m_equal random_bytes (fun () -> file_to_bytes fln));
  ("read_ba/write_ba">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "b" in
     m_equal random_bytes ( fun () -> copy_ba ~src:fln ~dst:fln2 >>= fun () ->
                            file_to_bytes fln2));
  ("read/write">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "c" in
     m_equal random_bytes ( fun () -> copy ~src:fln ~dst:fln2 >>= fun () ->
                            file_to_bytes fln2));
  ("sendfile">::
   fun _ctx ->
     let fln = tmpdir () // "a" in
     let fln2 = tmpdir () // "d" in
     m_equal random_bytes ( fun () -> copy_sendfile ~src:fln ~dst:fln2 >>= fun () ->
                            file_to_bytes fln2));
  ("stat">::
   fun _ctx ->
     let fln = tmpdir () // "d" in
     m_true (fun () -> stat fln >>= fun s -> Ok (
         Common.D.qstat s && s.st_kind = S_REG &&
         s.st_size = Int64.of_int random_bytes_length )));
  ("mkdir">::
   fun _ctx ->
     m_equal () (fun () -> mkdir (tmpdir () // "f")));
  ("rmdir">::
   fun _ctx ->
     m_equal () (fun () -> rmdir @@ tmpdir () // "f"));
  ("unlink">::
   fun _ctx ->
     m_equal () (fun () -> unlink @@ tmpdir () // "d"));
  ("link">::
   fun ctx ->
     no_win ctx;
     m_equal () (fun () -> link ~target:(tmpdir () // "a") ~link_name:(tmpdir () // "f"));
     m_equal () (fun () -> unlink (tmpdir () // "f")));
  ("scandir">::
   fun _ctx ->
     (* It's currently broken on windows, but fixed in trunk:
        https://github.com/libuv/libuv/issues/196 *)
     let files = [| S_REG, "a" ; S_REG, "b" ; S_REG, "c" |] in
     m_equal files (fun () -> scandir (tmpdir()) >>= fun s -> Array.sort compare s ;
                    return s));
  ("symlink/lstat">::
   fun ctx ->
     no_win ctx;
     let a = tmpdir () // "a"
     and d = tmpdir () // "d" in
     m_equal () (fun () -> symlink ~src:a ~dst:d ());
     m_equal true (fun () -> lstat d >>= fun s -> return (
         Common.D.qstat s && s.st_kind = S_LNK));
     m_equal a (fun () -> readlink d);
     m_equal () (fun () -> unlink d));
  ("rename">::
   fun _ctx ->
     let a = tmpdir () // "a"
     and z = tmpdir () // "z" in
     m_equal () (fun () -> rename ~src:a ~dst:z));
  ("utime">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     let time = Unix.time () -. 99_000. in
     m_true (fun () -> utime z ~access:time ~modif:time >>= fun () ->
             stat z >>= fun s ->
             let time = Int64.of_float time in
             let d1 = Int64.sub s.st_atime time |> Int64.abs
             and d2 = Int64.sub s.st_mtime time |> Int64.abs in
             return ( d1 = 0L && d2 = 0L ) ));
  ("futime/fstat">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     m_true ( fun () -> with_file ~mode:[O_RDWR] z @@ fun fd ->
              let time = Unix.time () +. 99_000. in
              futime fd ~access:time ~modif:time >>= fun () ->
              fstat fd >>= fun s ->
              let time = Int64.of_float time in
              let d1 = Int64.sub s.st_atime time |> Int64.abs
              and d2 = Int64.sub s.st_mtime time |> Int64.abs in
              return ( Common.D.qstat s && d1 = 0L && d2 = 0L ) ));
  ("chmod">::
   fun ctx ->
     no_win ctx;
     let z = tmpdir () // "z" in
     m_true ( fun () -> chmod z ~perm:0o751 >>= fun () ->
             stat z >>= fun s -> return (s.st_perm = 0o751)));
  ("fchmod">::
   fun ctx ->
     no_win ctx;
     let z = tmpdir () // "z" in
     m_true ( fun () -> with_file ~mode:[O_WRONLY] z @@ fun fd ->
             fchmod fd ~perm:0o621 >>= fun () -> fstat fd >>= fun s ->
             return (s.st_perm = 0o621)));
  ("access">::
   fun ctx ->
     let z = tmpdir () // "z" in
     let x = tmpdir () // "zz" in
     m_raises
       Uwt.ENOENT
       (fun () -> access x [Read]);
     m_equal () (fun () -> access z [Read]);
     m_equal () (fun () -> access Sys.executable_name [Exec]);
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
     m_raises
       Uwt.EACCES
       (fun () -> access shadow [Read]) );
  ("ftruncate">::
   fun _ctx ->
     let z = tmpdir () // "z" in
     m_true ( fun () -> with_file ~mode:[O_RDWR] z @@ fun fd ->
              ftruncate fd ~len:777L >>= fun () ->
              fstat fd >>= fun s -> s.st_size = 777L |> return ));
  ("realpath">::
   fun _ctx ->
     m_true ( fun () ->
         let p1 = Sys.getcwd () in
         realpath "." >>= fun p2 -> return (Common.fln_cmp p1 p2 = 0 )));
]

let l = "Fs_sync">:::l
