/* { dg-require-effective-target omp_usm } */
#pragma omp requires unified_shared_memory self_maps

#define MEM_SHARED

#include "target-std__map-concurrent.C"
