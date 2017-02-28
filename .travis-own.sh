#!/bin/sh

set -ex

eval $(opam config env)

if [ -f /tmp/uwt_additons_already_installed_without_t ]; then
    opam install -y -t -v uwt-random uwt-ssl uwt-tls uwt-conduit uwt-cohttp
else
    touch /tmp/uwt_additons_already_installed_without_t
    opam install -y -v uwt-random uwt-ssl uwt-tls uwt-conduit uwt-cohttp
fi

opam remove -y uwt-random uwt-ssl uwt-tls uwt-conduit uwt-cohttp
