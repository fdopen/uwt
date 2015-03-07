Uwt provides OCaml bindings for libuv - on top of lwt.

* Requests are translated to lwt-threads.

* Callbacks that are called continually are most of the time not
  translated to the usual lwt semantic.

* naming conventions mirror the conventions of libuv, so you can easily
  consult the official libuv manual. Only the differences are explained
  inside `uwt.mli`

* Uwt is **not** compatible with `lwt.unix`. It's not a further
  `Lwt_engine` in addition to `select` and `libev`. There are however
  alternatives to the higher level modules of `lwt.unix` with a nearly
  identic interface (Uwt_io, Uwt_preemptive, Uwt_log). You can also open
  the module `Uwt_compat` that provides aliases with module name as found
  in lwt (`Lwt_main` instead of `Uwt.Main`). This is necessary, if you want
  to use the camlp4 syntax extension of lwt.

* Uwt is not thread safe. All uwt functions should be called from your
  main thread.

Uwt is in an early alpha stage. The interface is likely to
change. Feel free to open an issue an make suggestions about it :)

## Installation

Dependencies:

* OCaml 4.02.1 (earlier versions are not supported)
* libuv 1.0 or later (0.x versions are not supported)
* lwt 2.4.7 or later

Build dependencies:

* autoconf
* pkg-config / pkgconf
* findlib
* omake
* cppo
* ppx_deriving
* ppx_import
* ounit

```
$ omake all
$ omake install
```
