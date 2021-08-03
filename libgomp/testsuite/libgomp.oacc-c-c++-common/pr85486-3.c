/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-additional-options "-DVECTOR_LENGTH=" } */
/* { dg-set-target-env-var "GOMP_OPENACC_DIM" "::128" } */

/* { dg-additional-options "-foffload=-fdump-tree-oaccloops" } */
/* { dg-set-target-env-var "GOMP_DEBUG" "1" } */

#include "pr85486.c"

/* { dg-final { scan-offload-tree-dump "__attribute__\\(\\(oacc function \\(1, 1, 32\\)" "oaccloops" } } */
/* { dg-output "nvptx_exec: kernel main\\\$_omp_fn\\\$0: launch gangs=1, workers=1, vectors=32" } */
