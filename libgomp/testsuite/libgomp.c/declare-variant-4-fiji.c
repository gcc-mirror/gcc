/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options -foffload=amdgcn-amdhsa } */
/* { dg-additional-options -foffload=-march=fiji } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

/* { dg-xfail-if "fiji/gfx803 is no longer enabled by default & deprectated in ROCm/LLVM/GCC" { *-*-* } } */

#define USE_FIJI_FOR_GFX803
#include "declare-variant-4.h"

/* { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump "= gfx803 \\(\\);" "optimized" } } */
