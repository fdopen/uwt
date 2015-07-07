open Common
open OUnit2
open Lwt.Infix

let random_line len =
  let rec iter buf i =
    if i = 0 then
      Bytes.unsafe_to_string buf
    else
      let c = random_char () in
      if c = '\n' || ( i = len && c = '\r') then
        iter buf i
      else
        let i' = pred i in
        Bytes.set buf i' c;
        iter buf i'
  in
  let b = Bytes.create len in
  iter b len

let random_lines ~max_line_len len =
  let rec iter accu i =
    if i = 0 then
      accu
    else
      let l = random_line (Random.int max_line_len) in
      iter (l::accu) (pred i)
  in
  iter [] len

let string_of_lines l =
  let rec iter accu = function
  | [] -> true,List.rev accu
  | ""::[] -> true, List.rev (""::accu)
  | a::[] -> false, List.rev (a::accu)
  | hd::tl ->
    let hd =
      if Random.int 2 = 1 then
        hd ^ "\r"
      else
        hd
    in
    iter (hd::accu) tl
  in
  let last_empty,l = iter [] l in
  let s = String.concat "\n" l in
  if last_empty then
    match Random.int 2 with
    | 0 -> s ^ "\n"
    | _ -> s ^ "\r\n"
  else
    match Random.int 3 with
    | 0 -> s ^ "\n"
    | 1 -> s ^ "\r\n"
    | _ -> s

let buffer_of_lines l =
  string_of_lines l |> Uwt_bytes.of_string |> Uwt_io.of_bytes ~mode:Uwt_io.input

let get_lines b =
  Uwt_io.read_lines b |> Lwt_stream.to_list

let line_test_memory ~max_line_len ~max_lines =
  let len = Random.int max_lines + 1 in
  let lines = random_lines ~max_line_len len in
  buffer_of_lines lines |> get_lines >|= fun lines' ->
  lines = lines'

let line_test_disk ~max_line_len ~max_lines =
  let len = Random.int max_lines + 1 in
  let lines = random_lines ~max_line_len len in
  let content = string_of_lines lines in
  let name = Filename.temp_file ~temp_dir:(tmpdir ()) "uwt_test" ".txt" in
  Lwt.finalize ( fun () ->
      T_lib.string2file ~name ~content >>= fun t ->
      assert t;
      let flags = [Uwt.Fs.O_RDONLY] in
      Uwt_io.with_file ~mode:Uwt_io.input ~flags name @@ fun ic ->
      get_lines ic >|= fun lines' ->
      lines = lines'
    ) ( fun () -> Uwt.Fs.unlink name )

let l = [
  ("lines_memory">::
   fun _ ->
     for _i = 0 to 10 do
       m_true (line_test_memory ~max_line_len:20 ~max_lines:200);
     done);
  ("short_lines_disk">::
   fun _ ->
     for _i = 0 to 4 do
       m_true (line_test_disk ~max_line_len:20 ~max_lines:200);
     done);
  ("long_lines_disk">::
   fun _ ->
     for _i = 0 to 3 do
       m_true (line_test_disk ~max_line_len:270_000 ~max_lines:3);
     done);
]

let l = "Io">:::l
