/* { dg-require-effective-target size32plus } */
/* { dg-require-effective-target avx_runtime } */
/* { dg-additional-options "-O2 -fopenmp -fvect-cost-model=cheap -fdump-tree-vect-details -msse2 -mno-sse3" } */
/* { dg-final { scan-tree-dump-times "vectorized \[2-6] loops" 2 "vect" } } */

#include "scan-13.c"
