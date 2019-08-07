#!/bin/bash

set -e -x -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

if [ -n "$1" ] ; then
    GHC_VERSION=$1
else
    GHC_VERSION=ghc-8.4.4
fi

cabal v2-build -w $GHC_VERSION
EXEC="cabal v2-exec -v0 -w $GHC_VERSION --"
HSINSPECT=$($EXEC which hsinspect)

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"
    cabal v2-build -w $GHC_VERSION -O0 all || true
    # passes some parameters for testing...
    find library -name "*.hs" -print0 | xargs -0 -L1 -I {} sh -c "$EXEC $HSINSPECT imports {} -XLambdaCase -XNoLambdaCase > {}.$GHC_VERSION.imports.sexp"
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi
