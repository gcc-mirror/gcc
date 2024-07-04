/* This test appears to have too much parallelism to run without a GPU.  */
/* { dg-do run { target { offload_device } } } */
/* { dg-additional-options "-Wall -Wno-unknown-pragmas" } */

#define COMMON_DIRECTIVE omp target teams distribute parallel for map(tofrom:result[:dim0 * dim1]) map(to:matrix1[0:dim0 * dim1], matrix2[0:dim0 * dim1])
#define COLLAPSE_1 collapse(1)
#define COLLAPSE_2 collapse(2)
#define COLLAPSE_3

#include "matrix-transform-variants-1.h"
