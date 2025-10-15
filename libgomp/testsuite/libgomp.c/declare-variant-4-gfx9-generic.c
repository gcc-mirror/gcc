/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options -foffload=amdgcn-amdhsa } */
/* { dg-additional-options -foffload=-march=gfx9-generic } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#include "declare-variant-4.h"

/* { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump "= gfx9_generic \\(\\);" "optimized" } } */
