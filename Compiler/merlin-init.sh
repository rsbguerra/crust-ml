#!/bin/sh

if test -f .merlin; then

    echo ".merlin already exists, bailing out ..." >&2
    exit 1

else

    # You could add your default EXT's and such to this list:
    cat >.merlin <<EOF
S .
B _build
EOF

    # Add PKG's:
    ocamlfind list \
	| awk '{ print "PKG "$1 }' >>.merlin

    # See https://github.com/the-lambda-church/merlin/wiki/Letting-merlin-locate-go-to-stuff-in-.opam
    find ~/.opam/4.11.1 -name '*.cmt' -print0 \
	| xargs -0 -I{} dirname '{}' \
	| sort -u \
	| awk '{ print "S "$0"\nB "$0 }' >> .merlin

fi