/* { dg-additional-options -DOFFLOAD_DEVICE_NVPTX { target offload_device_nvptx } } */
/* { dg-additional-options {-fdump-tree-optimized -foffload-options=-fdump-tree-optimized} } */

#include "declare-variant-3.h"

/* { dg-final { scan-tree-dump "= f \\(\\);" "optimized" } }
   { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump "= f \\(\\);" "optimized" { target offload_target_amdgcn } } }
   { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump "= f\[0-9\]+ \\(\\);" "optimized" { target offload_target_nvptx } } } */
