/* { dg-do link { target offload_target_any } } */
/* { dg-additional-options "-O0 -foffload=-fdump-tree-optimized -fopenmp-ompt=extended" } */

/* Check that, with -fopenmp-ompt=extended, _dispatch builtins are called along
   with the _start and _end variants.  */

#include "for-static.h"

/* { dg-final { scan-offload-tree-dump-times "GOMP_distribute_static_worksharing_start \\(\[^\\)\]" 2 "optimized" } } */
/* { dg-final { scan-offload-tree-dump-times "GOMP_distribute_static_worksharing_dispatch \\(\[^,\]+, \[^)\]+\\)" 2 "optimized" } } */
/* { dg-final { scan-offload-tree-dump-times "GOMP_distribute_static_worksharing_end \\(\\)" 2 "optimized" } } */
