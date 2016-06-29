val l: OUnitTest.test
val with_file:
  mode:Uwt.Fs.uv_open_flag list ->
  string ->
  (Uwt.file -> 'a Uwt.uv_result) ->
  'a Uwt.uv_result
