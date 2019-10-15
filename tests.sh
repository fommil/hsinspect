#!/bin/bash

set -e -x -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

if [ -n "$1" ] ; then
    GHC_VERSION=$1
else
    GHC_VERSION=ghc-8.4.4
fi

HSINSPECT="cabal v2-run -w $GHC_VERSION -v0 hsinspect --"

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"

    cabal v2-clean
    rm -rf .ghc.version library/.ghc.flags || true

    # needs a successful compile for .hi files to be written
    cabal v2-build -w $GHC_VERSION --constraint="medley -uncompilable"
    if [ ! -f .ghc.version ] ; then
        echo "library/.ghc.version was not created, HsInspect.Plugin failed"
        exit 1
    fi
    if [ ! -f library/.ghc.flags ] ; then
        echo "library/.ghc.flags was not created, HsInspect.Plugin failed"
        exit 1
    fi
    GHC_FLAGS=$(cat library/.ghc.flags)
    for f in $(find library -name "*.hs") ; do
        echo "TEST $f"
        $HSINSPECT imports "$f" -- $GHC_FLAGS > "$f.$GHC_VERSION.imports.sexp"
        $HSINSPECT imports "$f" --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "$f.$GHC_VERSION.imports.json"
        $HSINSPECT modules "$f" --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "$f.$GHC_VERSION.modules.json"
    done
    $HSINSPECT packages library --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "library/$GHC_VERSION.packages.json"
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi

# test for exceptions...
$HSINSPECT --help
$HSINSPECT --version
