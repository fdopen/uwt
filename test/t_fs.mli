val l: OUnitTest.test
val file_to_bytes: string -> bytes Lwt.t
val with_file:
  mode:Uwt.Fs.uv_open_flag list ->
  string ->
  (Uwt.file -> 'a Lwt.t) ->
  'a Lwt.t
