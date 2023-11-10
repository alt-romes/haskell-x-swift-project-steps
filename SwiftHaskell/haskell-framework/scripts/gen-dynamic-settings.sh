#!/usr/bin/env bash

set -e
if ! test -f "haskell-framework/haskell-framework.cabal"; then
    echo "Run this script from the root of your XCode project!"
    exit 1
fi

echo "
HEADER_SEARCH_PATHS=\$(inherit) $(ghc-pkg field rts include-dirs --simple-output | tr ' ' '\n' | tail -n1)
" > DynamicBuildSettings.xcconfig

echo "Created DynamicBuildSettings.xcconfig!"

