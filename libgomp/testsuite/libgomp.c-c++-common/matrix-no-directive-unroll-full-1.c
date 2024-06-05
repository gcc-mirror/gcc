/* { dg-additional-options "-O2 -fdump-tree-original -Wall -Wno-unknown-pragmas" } */

#undef COMMON_DIRECTIVE
#define COMMON_TOP_TRANSFORM omp unroll full
#define COLLAPSE_1
#define COLLAPSE_2
#define COLLAPSE_3
#define IMPLEMENTATION_FILE "matrix-constant-iter.h"

#include "matrix-transform-variants-1.h"

/* A consistency check to prevent broken macro usage. */
/* { dg-final { scan-tree-dump-times "unroll full" 13 "original" } } */
