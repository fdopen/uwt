open OUnit
let return = Lwt.return
let m_equal s t =
  assert_equal s (Uwt.Main.run t)
let m_raises (a,b,c) t =
  assert_raises
    (Unix.Unix_error(a,b,c))
    (fun () -> Uwt.Main.run t)
let m_true t = m_equal true t
let assert_canceled t =
  assert_raises Lwt.Canceled (fun () -> Uwt.Main.run t)

let nm_try_finally f x finallly' y =
  let res =
    try f x with exn -> finallly' y; raise exn in
  finallly' y;
  res

let has_ip6 =
  Uwt_base.Misc.interface_addresses_exn () |> Array.to_list |>
  List.exists ( fun x ->
      match x.Uwt_base.Misc.address with
      | None -> false
      | Some x -> Unix.domain_of_sockaddr x = Unix.PF_INET6 )

let ip6_option =
  OUnit2.Conf.make_bool "no_ip6" false "force ignoring of ip6 related tests"

let multiplicand =
  OUnit2.Conf.make_int
    "multiplicand"
    1
    "control how much data is written in stress tests"

let contingent =
  OUnit2.Conf.make_bool
    "skip_contingent"
    true
    "skip test cases which results are highly contingent"

let all =
  OUnit2.Conf.make_bool
    "all"
    false
    "enable all possible test cases"

let is_contingent ctx =
  OUnit2.skip_if
    ( contingent ctx && all ctx = false )
    "skip contingent test"

let ip6_only ctx =
  let n = ip6_option ctx in
  OUnit2.skip_if
    ((not has_ip6 || n) && all ctx = false )
    "no ip6"

let has_ip6 ctx =
  has_ip6 && not (ip6_option ctx)

let no_win ctx =
  OUnit2.skip_if (all ctx = false && Sys.win32) "no windows support (yet)"


let is_winxp =
  Uwt.(match Sys_info.win_version () with
    | Error _ -> false
    | Ok x -> x.Sys_info.major_version < 6)

let no_win_xp ctx =
  OUnit2.skip_if (all ctx = false && is_winxp) "no windows xp support"

let skip_if_not_all ctx cond msg =
  OUnit2.skip_if (all ctx = false && cond ) msg

module D = struct
  include Show_uwt
  let qstat x = show_stats x |> String.length > 50
end

let random_char =
  let seed = ref 7817 in
  fun () ->
    seed := 214013 * !seed + 2531011;
    Char.chr ((!seed lsr 16 ) land 255)

let rbytes_create len =
    let b = Bytes.create len in
    for i = 0 to pred len do
      Bytes.unsafe_set b i (random_char ())
    done;
    b

let rba_create len =
    let b = Uwt_bytes.create len in
    for i = 0 to pred len do
      Uwt_bytes.unsafe_set b i (random_char ())
    done;
    b

let rstring_create len =
  rbytes_create len |> Bytes.unsafe_to_string

let invalid = "/tmp/invalid/invalid/invalid/invalid"
let tmpdir = ref invalid

let remove_dir ?(keep_root=false) orig_file =
  let no_exn f t =
    try
      ignore( f t )
    with
    | Unix.Unix_error _
    | Sys_error _ -> ()
  in
  let rec iter file =
    let module U = Unix in
    match (U.lstat file).U.st_kind with
    | exception _ -> ()
    | U.S_DIR ->
      Sys.readdir file |> Array.iter (function
        | "." | ".." -> () (* should not be included anyway,...*)
        | name -> Filename.concat file name |> iter );
      if keep_root = false || file <> orig_file then
        no_exn Unix.rmdir file
    | U.S_REG | U.S_CHR | U.S_BLK | U.S_LNK | U.S_FIFO | U.S_SOCK ->
      if keep_root = false || file <> orig_file then
        no_exn Sys.remove file
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

let fln_cmp =
  if not Sys.win32 then
    String.compare
  else
    fun p1 p2 ->
      let slen1 = String.length p1
      and slen2 = String.length p2
      and is_alpha = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
      in
      if slen1 <> slen2 || slen1 < 2 || p1.[1] <> ':' || p2.[1] <> ':' then
        String.compare p1 p2
      else
        let d1 = p1.[0]
        and d2 = p2.[0] in
        if is_alpha d1 = false || is_alpha d2 = false ||
           Char.lowercase d1 <> Char.lowercase d2 then
          String.compare p1 p2
        else
          let len = slen1 - 1 in
          String.compare
            (String.sub p1 1 len)
            (String.sub p2 1 len)

let stream_read_own_test t =
  (* test if emulation of classic lwt read works *)
  let open Uwt.Stream in
  let open Lwt.Infix in
  let blen = 262_144 in
  let wbuf = rba_create blen
  and rbuf = Uwt_bytes.create blen
  and bt_read = ref 0 in
  let wt = write_ba t ~buf:wbuf in
  let rec rt ~cnt pos =
    if pos >= blen then Lwt.return_unit else
    let len = min (blen - pos) 128 in
    read_ba ~buf:rbuf ~pos ~len t >>= fun i ->
    bt_read := !bt_read + i;
    (if cnt mod 13 = 0 then Uwt.Timer.sleep 10 else
       Uwt.Main.yield () )
    >>= fun () ->
    rt ~cnt:(succ cnt) (pos + i)
  in
  Lwt.join [rt ~cnt:0 0; wt] >|= fun () ->
  wbuf = rbuf && !bt_read = blen

let uv_version = Uwt.Misc.version ()
let uv_minor = uv_version.Uwt.Misc.minor
let uv_major = uv_version.Uwt.Misc.major

let no_symlinks_rights = ref false
let skip_no_symlinks_rights ctx =
  OUnit2.skip_if
    (all ctx = false && !no_symlinks_rights)
    "no rights to create symlinks"
