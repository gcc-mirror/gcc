/* { dg-require-effective-target omp_usm } */
// { dg-additional-options "-Wno-deprecated-openmp" }
#pragma omp requires unified_shared_memory self_maps
#define OMP_USM
#define MEM_SHARED

#include "target-std__deque-concurrent.C"
