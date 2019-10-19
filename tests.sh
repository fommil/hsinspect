#!/bin/bash

set -e -x -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# use cabal v2-configure to change ghc version
HSINSPECT="cabal v2-run -v0 hsinspect --"
cabal v2-build --only-dependencies
GHC_VERSION=ghc-$(cabal v2-exec -v0 ghc -- --numeric-version)

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"

    cabal v2-clean
    rm -rf .ghc.version library/.ghc.flags || true
    touch .ghc.version library/.ghc.flags # tests that overwriting works

    # a successful compile is not necessary for .hi files to be written!
    # cabal v2-build --constraint="medley -uncompilable"
    cabal v2-build 2>/dev/null || true
    if [ ! -s .ghc.version ] ; then
        echo ".ghc.version was not overwritten, GhcFlags.Plugin failed"
        exit 1
    fi
    if [ ! -s library/.ghc.flags ] ; then
        echo "library/.ghc.flags was not overwritten, GhcFlags.Plugin failed"
        exit 1
    fi
    GHC_FLAGS=$(cat library/.ghc.flags)
    for f in $(find library -name "*.hs") ; do
        echo "TEST $f"
        $HSINSPECT imports "$f" -- $GHC_FLAGS > "$f.$GHC_VERSION.imports.sexp"
        $HSINSPECT imports "$f" --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "$f.$GHC_VERSION.imports.json"
    done
    $HSINSPECT packages library --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "library/$GHC_VERSION.packages.json"
    $HSINSPECT index --json -- $GHC_FLAGS | python -m json.tool --sort-keys > "library/$GHC_VERSION.index.json"
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi

# test for exceptions...
$HSINSPECT --help
$HSINSPECT --version
