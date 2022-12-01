/* { dg-do run { target { offload_target_amdgcn } } } */
/* { dg-skip-if "gfx908 only" { ! amdgcn-*-* } { "*" } { "-foffload=-march=gfx908" } } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#include "declare-variant-4.h"

/* { dg-final { scan-offload-tree-dump "= gfx908 \\(\\);" "optimized" } } */
