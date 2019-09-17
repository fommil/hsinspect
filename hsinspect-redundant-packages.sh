#!/bin/bash

# List all redundant packages in this project.

# To be run after compiling the project and having generated .ghc.version / .ghc.flags files.

# Known caveats:
#
# 1. some packages appear unused but compilation fails without them (e.g. protolens-runtime)
# 2. only one source dir per local package is supported

if [ ! -f .ghc.version ] ; then
    echo ".ghc.version must exist"
    exit 1
fi

BASE=$PWD

HSINSPECT=hsinspect-ghc-$(cat .ghc.version)

for P in $(find . -path ./dist-newstyle -prune -o -name "*.cabal" -print) ; do
    cd "$BASE"
    cd "$(dirname $P)"
    echo "$PWD"

    for C in $(find . -name .ghc.flags) ; do
        S="$(dirname $C)"
        echo "$S"
        $HSINSPECT packages "$S" --json -- $(cat "$C") | jq '.unused'
    done
done
