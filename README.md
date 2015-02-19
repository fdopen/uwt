Uwt provides OCaml bindings for libuv - on top of lwt.

* Requests are translated to lwt-threads.

* Callbacks that are called continually are most of the time not
  translated to the usual lwt semantic.

* naming conventions mirror the conventions of libuv, so you can easily
  consult the official libuv manual. Only the differences are explained
  inside `uwt.mli`

* Uwt is **not** compatible with `lwt.unix`. It's not a further
  `Lwt_engine` in addition to `select` and `libev`.

* Uwt is not thread safe. All uwt functions should be called from your
  main thread.

Uwt is in an early alpha stage. The interface is likely to
change. Feel free to open an issue an make suggestions about it :)

## Installation

Dependencies:

* OCaml 4.02.1 (earlier versions are not supported)
* libuv 1.2 or later (0.x versions are not supported)
* lwt 2.4.7 or later

Build dependencies:

* autoconf
* pkg-config / pkgconf
* findlib
* omake
* cppo
* ppx_deriving
* ppx_import

```
$ omake all
$ omake install
```
