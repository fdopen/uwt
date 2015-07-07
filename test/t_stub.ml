open OUnit2
open Lwt.Infix
open Common

let (//) = Filename.concat
let l = [
  ("stub_test">::
   fun _ctx ->
     let name = Filename.temp_file  "uwt_stub" ".txt" in
     let content = "Hello World" in
     let f =
       Lwt.finalize ( fun () ->
           T_lib.string2file ~name ~content >>= function
           | false -> Lwt.return_false
           | true ->
             T_fs.file_to_bytes name >|= fun content' ->
             content = Bytes.to_string content'
         ) ( fun () -> Sys.remove name ; Lwt.return_unit )
     in
     m_true f);
]

let l = "Work_stub">:::l
