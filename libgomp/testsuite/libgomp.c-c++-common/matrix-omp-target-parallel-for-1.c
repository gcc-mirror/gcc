/* This test appears to have too much parallelism to run without a GPU.  */
/* { dg-do run { target { offload_device } } } */
/* { dg-additional-options "-fdump-tree-original -Wall -Wno-unknown-pragmas" } */

#define COMMON_DIRECTIVE omp target parallel for map(tofrom:result[0:dim0 * dim1]) map(to:matrix1[0:dim0 * dim1], matrix2[0:dim0 * dim1])
#define COLLAPSE_1 collapse(1)
#define COLLAPSE_2 collapse(2)
#define COLLAPSE_3

#include "matrix-transform-variants-1.h"

/* A consistency check to prevent broken macro usage. */
/* { dg-final { scan-tree-dump-times "omp target" 13 "original" } } */
/* { dg-final { scan-tree-dump-times "collapse" 9 "original" } } */
/* { dg-final { scan-tree-dump-times "unroll partial" 12 "original" } } */
