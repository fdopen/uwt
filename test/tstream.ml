open Lwt.Infix

let testv ?min_elems ?max_elems ?max_elem_length writev t_orig t =
  let open Common in
  let buf_read = Bytes.create 65_536 in
  let iovecs = iovecs_create ?min_elems ?max_elems ?max_elem_length () in
  let iovecs_len = iovecs_length iovecs in
  let res_read = ref [] in
  let rec read len =
    if len = 0 then Lwt.return_unit else
      Uwt.Stream.read t ~buf:buf_read >>= fun len' ->
      if len' = 0 || len' > len then
        let () = Uwt.Stream.close_noerr t in
        Lwt.return_unit
      else
      let s = Bytes.sub buf_read 0 len' in
      res_read := s :: !res_read ;
      let len'' = len - len' in
      if len'' = 0 then
        Lwt.return_unit
      else
        read len''
  in
  Lwt.join [ read iovecs_len ; writev t_orig iovecs ] >|= fun () ->
  iovecs_to_bytes iovecs = bytes_rev_concat !res_read
