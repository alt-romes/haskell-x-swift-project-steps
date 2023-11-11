#include <stdlib.h>
#include <stdio.h>
#include <HsFFI.h>

HsBool flib_init() {

    printf("Initialising flib\n");

    // Initialise Haskell runtime
    hs_init(NULL, NULL);

    // Do other library initialisations here

    return HS_BOOL_TRUE;
}

void flib_end() {
    printf("Terminating flib\n");
    hs_exit();
}

