#!/usr/bin/env bash

set -e

if ! test -d "SwiftHaskell.xcodeproj"; then
    echo "Run this from the SwiftHaskell XCode project root!"
    exit 1
fi

pushd . >/dev/null
cd haskell-framework/
cabal build all
./scripts/test-haskell-foreign-lib.sh
popd >/dev/null
./haskell-framework/scripts/gen-dynamic-settings.sh

echo "Done."


