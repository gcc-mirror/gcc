#include <stdlib.h>
#include <stdio.h>
#include <ffi.h>

#define MAX_ARGS 256

#define CHECK(x) (!(x) ? abort() : (void)0)

