/* { dg-require-effective-target omp_usm } */
// { dg-additional-options "-Wno-deprecated-openmp" }
#pragma omp requires unified_shared_memory self_maps

#define MEM_SHARED

#include "target-std__valarray-concurrent.C"
