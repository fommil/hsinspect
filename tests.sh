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
HSINSPECT=$(cabal v2-exec -w $GHC_VERSION -v0 -- which hsinspect)

# test for exceptions...
$HSINSPECT --help
$HSINSPECT --version

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"

    # See the README for reasons why we have to manually create an env file from
    # a good build.
    cabal v2-build -w $GHC_VERSION --constraint="medley -uncompilable" :all:libraries
    cabal v2-exec -w $GHC_VERSION --constraint="medley -uncompilable" -- sh -c 'cat $GHC_ENVIRONMENT > .hsinspect.env'

    cabal v2-build -w $GHC_VERSION all > /dev/null 2>&1 || true
    export GHC_ENVIRONMENT="$PWD/.hsinspect.env"
    # LambdaCase is to test user-provided lang extensions
    find library -name "*.hs" -print0 | xargs -0 -L1 -I {} sh -c "$HSINSPECT imports {} -- -XLambdaCase > {}.$GHC_VERSION.imports.sexp"
    find library -name "*.hs" -print0 | xargs -0 -L1 -I {} sh -c "$HSINSPECT imports {} --json -- -XLambdaCase | python -m json.tool > {}.$GHC_VERSION.imports.json"
    unset GHC_ENVIRONMENT
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi
