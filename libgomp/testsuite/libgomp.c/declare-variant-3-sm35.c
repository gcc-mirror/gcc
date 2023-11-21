/* { dg-do link { target { offload_target_nvptx } } } */
/* { dg-additional-options -foffload=nvptx-none } */
/* { dg-additional-options "-foffload=-misa=sm_35 -foffload=-mptx=_" } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#include "declare-variant-3.h"

/* { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump "= f35 \\(\\);" "optimized" } } */
