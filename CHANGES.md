0.0.5 2017-03-03
---------------------------
- new functions: Uwt.Stream.writev and Uwt.Fs.writev

- the opam package will now always use the internal copy of
  libuv. (you can link uwt against the libuv version shipped by your
  distro with `BUILD_LIBUV=true opam install uwt`)

- prepare OCaml 4.05 support

- internal libuv version updated to 1.11.0

- avoid memcpy calls inside Uwt.Stream.read and Uwt.Udp.recv
  (*nix only)

- Set up Travis for OS X testing

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
