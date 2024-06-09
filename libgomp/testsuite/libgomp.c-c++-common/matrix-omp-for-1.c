/* { dg-additional-options "-fdump-tree-original -Wall -Wno-unknown-pragmas" } */

#define COMMON_DIRECTIVE omp for
#define COLLAPSE_1 collapse(1)
#define COLLAPSE_2 collapse(2)
#define COLLAPSE_3 collapse(3)

#include "matrix-transform-variants-1.h"


/* A consistency check to prevent broken macro usage. */
/* { dg-final { scan-tree-dump-times "omp for" 13 "original" } } */
/* { dg-final { scan-tree-dump-times "collapse" 12 "original" } } */
