# Uwt

Uwt provides OCaml bindings
for [libuv](https://github.com/libuv/libuv) - on top
of [lwt](https://github.com/ocsigen/lwt).

[![Travis build Status](https://travis-ci.org/fdopen/uwt.svg?branch=master)](https://travis-ci.org/fdopen/uwt)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/fdopen/uwt?svg=true)](https://ci.appveyor.com/project/fdopen/uwt)

* Requests are translated to lwt-threads.

* Callbacks that are called continually are most of the time not
  translated to the usual lwt semantic.

* naming conventions mirror the conventions of libuv, so you can easily
  consult the official [libuv manual](http://docs.libuv.org/en/v1.x/).
  Only the differences are explained inside `uwt.mli`

* Uwt is **not** compatible with `lwt.unix`. It's not a further
  `Lwt_engine` in addition to `select` and `libev`. However, the
  findlib package `uwt.ext` provides alternatives to the higher level
  modules of `lwt.unix` with a nearly identic interface (Uwt_io,
  Uwt_process, Uwt_log). You can also open the module `Uwt_compat`
  (package `uwt.compat`) that provides aliases with module names as
  found in lwt (e.g. `Lwt_main` instead of `Uwt.Main`). This is
  necessary, if you want to use the camlp4 syntax extension of lwt.

* Uwt is not thread safe. All uwt functions should be called from your
  main thread.

## Installation

### Quick install guide

```bash
$ opam install uwt
# or for the latest development version
$ opam pin add uwt --dev-repo
```

### Manual install

Dependencies:

* OCaml 4.02.1 or later
* lwt 2.6.0 or later
* libuv 1.8 or later

Build dependencies:

* pkg-config / pkgconf
* findlib
* omake
* cppo
* ppx_deriving (test only)
* ppx_import (test only)
* ounit (test only)
* autoconf (repo pinning only)

```bash
$ omake all
$ omake install
```

libuv will be compiled locally, if is not already installed on your system.
You can also explicity enforce one option:

```bash
$ omake all BUILD_LIBUV=true
# or
$ omake all BUILD_LIBUV=false
```
