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
  ("read_write_types">::
   fun _ ->
     let rw ?(cmp=Pervasives.compare) read write random type_len =
       let len = 1024 in
       let ba = Uwt_bytes.create (len * type_len) in
       let bi = Uwt_io.of_bytes ~mode:Uwt_io.input ba
       and bo = Uwt_io.of_bytes ~mode:Uwt_io.output ba in
       let rec iter i =
         if i = 0 then Lwt.return_true else
         let v = random () in
         write bo v >>= fun () ->
         read bi >>= fun v' ->
         assert_equal 0 (cmp v v');
         iter (pred i)
       in
       iter len
     in
     let test (module X : Uwt_io.NumberIO) =
       let random () =
         let max =
           if Sys.word_size > 32 then
             Int32.max_int
           else
             Int32.of_int max_int
         in
         let x = Random.int32 max |> Int32.to_int in
         match Random.int 2 with
         | 0 -> x
         | _ -> x * (-1)
       in
       m_true (rw X.read_int X.write_int random 4);

       let random () =
         let x = Random.int 0x7fff in
         match Random.int 2 with
         | 0 -> x
         | _ -> x * (-1)
       in
       m_true (rw X.read_int16 X.write_int16 random 2);

       let random () =
         let x = Random.int32 Int32.max_int in
         match Random.int 2 with
         | 0 -> x
         | _ -> Int32.mul x (Int32.of_int (-1))
       in
       m_true (rw X.read_int32 X.write_int32 random 4);

       let random () =
         let x = Random.int64 Int64.max_int in
         match Random.int 2 with
         | 0 -> x
         | _ -> Int64.mul x (Int64.of_int (-1))
       in
       m_true (rw X.read_int64 X.write_int64 random 8);

       let random x () =
         let x = Random.float x in
         match Random.int 2 with
         | 0 -> x
         | _ -> x *. (-1.)
       in
       let cmp a b =
         if a = b then 0 else
         let abs_a = abs_float a in
         let abs_b = abs_float b in
         let diff = abs_float ( a -. b ) in
         let epsilon = 1.1e-7 in
         if diff /. (min (abs_a +. abs_b) max_float) < epsilon then 0
         else compare a b
       in
       m_true (rw ~cmp X.read_float64 X.write_float64 (random max_float) 8);
       m_true (rw ~cmp X.read_float64 X.write_float64 (random 1.e7) 8);
       m_true (rw ~cmp X.read_float64 X.write_float64 (random 1.e-3) 8);

       let fmax = 3.40282346638528859812e+38 in
       m_true (rw ~cmp X.read_float32 X.write_float32 (random fmax) 4);
       m_true (rw ~cmp X.read_float32 X.write_float32 (random 1.e7) 4);
       m_true (rw ~cmp X.read_float32 X.write_float32 (random 1.e-3) 4);
     in
     test (module Uwt_io.LE);
     test (module Uwt_io.BE);
  );
]

let l = "Io">:::l
