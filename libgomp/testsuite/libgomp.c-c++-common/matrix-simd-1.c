/* { dg-additional-options "-Wall -Wno-unknown-pragmas" } */

#define COMMON_DIRECTIVE omp simd
#define COLLAPSE_1 collapse(1)
#define COLLAPSE_2 collapse(2)
#define COLLAPSE_3 collapse(3)

#include "matrix-transform-variants-1.h"
