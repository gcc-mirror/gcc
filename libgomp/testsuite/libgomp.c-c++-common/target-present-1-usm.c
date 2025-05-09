/* { dg-require-effective-target omp_usm } */
#pragma omp requires unified_shared_memory self_maps

#include "target-present-1.c"

/* No 'dg-shouldfail'.  */
