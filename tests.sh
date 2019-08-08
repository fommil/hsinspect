#!/bin/bash

set -e -x -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

if [ -n "$1" ] ; then
    GHC_VERSION=$1
else
    GHC_VERSION=ghc-8.4.4
fi

# use -O0 and --enable-tests to WORKAROUND https://github.com/haskell/cabal/issues/6182
cabal v2-build -w $GHC_VERSION -O0
EXEC="cabal v2-exec -v0 -w $GHC_VERSION -O0 --enable-tests --"
HSINSPECT=$($EXEC which hsinspect)

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"
    # this `-O0 --enable-tests' is intentional, it simulates users
    cabal v2-build -w $GHC_VERSION -O0 --enable-tests all || true
    # passes some parameters for testing...
    # $EXEC sh -c 'cat $GHC_ENVIRONMENT' > env.$GHC_VERSION
    find library -name "*.hs" -print0 | xargs -0 -L1 -I {} sh -c "$EXEC $HSINSPECT imports {} -XLambdaCase -XNoLambdaCase > {}.$GHC_VERSION.imports.sexp"
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi
