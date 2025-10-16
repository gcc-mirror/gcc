/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options -foffload=amdgcn-amdhsa } */
/* { dg-additional-options -foffload=-march=gfx1032 } */
/* { dg-additional-options "-foffload=-fdump-tree-optimized" } */

#include "declare-variant-4.h"

/* { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump "= gfx1032 \\(\\);" "optimized" } } */


/* This code will link nicely if the multilib for that GPU architecture
   has been build for GCC. In that case, scan-offload-tree-dump will
   PASS and the linking will yield an XPASS message due to following line: */

/* { dg-excess-errors "ld: error: unable to find library -lgomp|gcn mkoffload: fatal error" } */

/* If the multi-lib config is not available, there are two options:

   * If the generic multi-lib is available, mkoffload fails early,
     yielding UNRESOLVED for scan-offload-tree-dump and an XFAIL
     for the message:
       gcn mkoffload: fatal error: GCC was built without library support
       for '-march=gfx...'; consider compiling for the associated
       generic architecture '-march=gfx...-generic' instead

   * Or compling succeeds - then scan-offload-tree-dump will PASS -
     but linking fails with the following error (XFAIL):
       ld: error: unable to find library -lgomp
       collect2: error: ld returned 1 exit status
       gcn mkoffload: fatal error: ...-gnu-accel-amdgcn-amdhsa-gcc returned 1 exit status
       compilation terminated.
       lto-wrapper: fatal error: .../amdgcn-amdhsa/mkoffload returned 1 exit status
       compilation terminated. */
