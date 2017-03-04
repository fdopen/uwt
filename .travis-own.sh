#!/bin/sh

set -ex

eval $(opam config env)

unset OPAMBUILDTEST
unset OPAMVERBOSE
export OPAMYES=1
tmpfile="$(mktemp)"
opam remove uwt || true
opam install -t --show-actions --json "$tmpfile" uwt uwt-random uwt-ssl uwt-tls uwt-conduit uwt-cohttp
opam depext -i $(jq -r '.[]? | .[]? | .install? | select(. != null) | [.name, .version] | join(".")' "$tmpfile" | grep -v uwt)
opam install -t -v uwt uwt-random uwt-ssl uwt-tls uwt-conduit uwt-cohttp
