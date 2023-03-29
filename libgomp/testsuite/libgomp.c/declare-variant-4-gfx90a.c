/* { dg-do run { target { offload_target_amdgcn } } } */
/* { dg-skip-if "gfx90a only" { ! amdgcn-*-* } { "*" } { "-foffload=-march=gfx90a" } } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#include "declare-variant-4.h"

/* { dg-final { scan-offload-tree-dump "= gfx90a \\(\\);" "optimized" } } */
