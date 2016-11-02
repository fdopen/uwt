0.0.4 2016-11-04
---------------------------
- API CHANGE: `Uwt_error` removed. Functions that possibly fail with
  no-unix error codes will now always return `('a, Uwt.error) result
  Lwt.t`, all other functions will `Lwt.fail` with
  `Unix_error`. `Unix.ECANCELED` is unfortunately missing, you have to
  use `Uwt.of_unix_error x = Uwt.ECANCELED` instead.
  
- uwt now compiles with Microsoft Visual Studio (14.0 only)
  
- internal libuv version updated to 1.10.0

- update lwt.unix compatibility layer
