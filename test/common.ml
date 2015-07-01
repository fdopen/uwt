open OUnit
open Lwt.Infix
let return = Lwt.return
let m_equal s t =
  assert_equal s (Uwt.Main.run t)
let m_raises (a,b,c) t =
  assert_raises
    (Uwt.Uwt_error(a,b,c))
    (fun () -> Uwt.Main.run t)
let m_true t = m_equal true t

let nm_try_finally f x finallly' y =
  let res =
    try f x with exn -> finallly' y; raise exn in
  finallly' y;
  res

let try_finally fnormal finalizer =
  let module T = struct type 'a t = Ok of 'a | Error of exn end in
  Lwt.catch ( fun () ->
      fnormal () >>= fun x ->
      Lwt.return (T.Ok x)
    ) ( fun exn -> Lwt.return (T.Error exn) ) >>= fun t ->
  finalizer () >>= fun () ->
  match t with
  | T.Ok x -> Lwt.return x
  | T.Error p -> Lwt.fail p


let has_ip6 =
  Uwt_base.Misc.interface_addresses_exn () |> Array.to_list |>
  List.exists ( fun x ->
      Uwt.Conv.to_unix_sockaddr_exn x.Uwt_base.Misc.address
      |> Unix.domain_of_sockaddr = Unix.PF_INET )

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

let skip_if_not_all ctx cond msg =
  OUnit2.skip_if (all ctx = false && cond ) msg

module D = struct
  include Show_uwt
  let qstat x = show_stats x |> String.length > 50
end

let rstring_create, rbytes_create, rba_create =
  let seed = ref 7817 in
  let rand () =
    seed := 214013 * !seed + 2531011;
    Char.chr ((!seed lsr 16 ) land 255)
  in
  let rec by len =
    let b = Bytes.create len in
    for i = 0 to pred len do
      Bytes.unsafe_set b i (rand ())
    done;
    b
  and ba len =
    let b = Uwt_bytes.create len in
    for i = 0 to pred len do
      Uwt_bytes.unsafe_set b i (rand ())
    done;
    b
  and str len =
    by len |> Bytes.unsafe_to_string
  in
  str,by,ba

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
        ignore(Unix.rmdir file)
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
