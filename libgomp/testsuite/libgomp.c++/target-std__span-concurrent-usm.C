// { dg-additional-options "-std=c++20" }

#pragma omp requires unified_shared_memory self_maps

#define MEM_SHARED

#include "target-std__span-concurrent.C"
