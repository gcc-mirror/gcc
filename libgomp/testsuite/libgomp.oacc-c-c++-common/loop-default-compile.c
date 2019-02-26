/* { dg-additional-options "-fopenacc-dim=16:16" } */
/* This code uses nvptx inline assembly guarded with acc_on_device, which is
   not optimized away at -O0, and then confuses the target assembler.
   { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-set-target-env-var "GOMP_OPENACC_DIM" "8:8" } */

#include "loop-default.h"

int main ()
{
  /* Environment should be ignored.  */
  return test_1 (16, 16, 32);
}
