#!/bin/bash

set -e -x -o pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# use cabal v2-configure to change ghc version
cabal v2-build exe:hsinspect
HSINSPECT=$(cabal v2-exec -v0 which -- hsinspect)
GHC_VERSION=ghc-$(cabal v2-exec -v0 ghc -- --numeric-version)

rm -rf dist-newstyle/build/*/*/medley-0.0.1

cd tests

for t in * ; do
    echo "testing $t"
    cd "$SCRIPT_DIR/tests/$t"

    rm -rf library/.ghc.flags || true
    touch library/.ghc.flags # tests that overwriting works

    # a successful compile is not necessary for .hi files to be written!
    # cabal v2-build --constraint="medley -uncompilable"
    cabal v2-build 2>/dev/null || true
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
    $HSINSPECT index --json -- $GHC_FLAGS | sed "s|${HOME}[^\"]*\"|REDACTED\"|g" | python -m json.tool --sort-keys > "library/$GHC_VERSION.index.json"
done

cd "$SCRIPT_DIR"
if ! git diff --quiet -- tests ; then
    echo "FAILED"
fi

# test for exceptions...
$HSINSPECT --help
$HSINSPECT --version
