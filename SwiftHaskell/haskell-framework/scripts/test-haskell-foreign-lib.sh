#!/usr/bin/env bash

set -e

if ! test -f "haskell-framework.cabal"; then
    echo "Run this script from the root of your project!"
    exit 1
fi

# Alternatively, run `cabal list-bin haskell-foreign-framework`
HS_FLIB=$(find . -name libhaskell-foreign-framework.dylib)

if test -z $HS_FLIB; then
    echo "Shared library not found! Did you run 'cabal build'?"
    exit 1
fi

HS_FLIB_PATH=$(dirname $HS_FLIB)
HS_HEADERS_PATH=haskell-framework-include

echo "
#include <stdio.h>
#include <MyForeignLib_stub.h>
#include <HsFFI.h>
int main(void) {
    hs_init(NULL, NULL);
    printf(\"%d\n\", hs_factorial(5));
    hs_exit();
    return 0;
}
" > conftestmain.c

# We use `ghc` instead of `gcc` because otherwise we also need to provide the
# include and lib path of the runtime system (Rts)
ghc -no-hs-main -o conftest conftestmain.c \
    -lhaskell-foreign-framework \
    -I"$HS_HEADERS_PATH" \
    -L"$HS_FLIB_PATH" \
    -optl-Wl,-rpath,"$HS_FLIB_PATH"

RESULT=$(./conftest)

if [ 120 -eq $RESULT ]; then
    echo "Foreign library successfully called!"
else
    echo "Bad bad foreign library!"
    exit 1
fi

rm -f conftest*

