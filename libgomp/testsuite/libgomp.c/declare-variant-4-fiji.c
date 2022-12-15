/* { dg-do run { target { offload_target_amdgcn } } } */
/* { dg-skip-if "fiji/gfx803 only" { ! amdgcn-*-* } { "*" } { "-foffload=-march=fiji" } } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#define USE_FIJI_FOR_GFX803
#include "declare-variant-4.h"

/* { dg-final { scan-offload-tree-dump "= gfx803 \\(\\);" "optimized" } } */
