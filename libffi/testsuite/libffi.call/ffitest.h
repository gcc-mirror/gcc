#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ffi.h>

#define MAX_ARGS 256

#define CHECK(x) !(x) ? abort() : 0 

