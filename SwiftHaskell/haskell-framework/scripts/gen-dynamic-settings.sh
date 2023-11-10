#!/usr/bin/env bash

set -e
if ! test -f "haskell-framework/haskell-framework.cabal"; then
    echo "Run this script from the root of your XCode project!"
    exit 1
fi

pushd . > /dev/null
cd haskell-framework
FLIB_PATH=$(cabal list-bin haskell-foreign-framework)
popd > /dev/null

echo "
HEADER_SEARCH_PATHS=\$(inherit) $(ghc-pkg field rts include-dirs --simple-output | tr ' ' '\n' | tail -n1)
LIBRARY_SEARCH_PATHS=\$(inherit) $(dirname $FLIB_PATH)
" > DynamicBuildSettings.xcconfig

echo "Created DynamicBuildSettings.xcconfig!"

