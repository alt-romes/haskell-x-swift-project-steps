#!/usr/bin/env bash

set -e

if ! test -f "haskell-framework.cabal"; then
    echo "Run this script from the root of your project!"
    exit 1
fi

HS_FLIB_PATH=$(dirname $(find . -name libhaskell-foreign-framework.dylib))
HS_HEADERS_PATH=haskell-framework-include

echo "
#include <stdio.h>
#include <MyForeignLib_stub.h>
#include <HsFFI.h>
int main(void) {
    int argc = 0;
    char* argv[] = { NULL };
    char** argp = argv;
    hs_init(&argc, &argp);
    printf(\"%d\n\", hs_double(4));
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

if [ 8 -eq $(./conftest) ]; then
    echo "Foreign library successfully called!"
else
    echo "Bad bad foreign library!"
    exit 1
fi

rm -f conftest*

