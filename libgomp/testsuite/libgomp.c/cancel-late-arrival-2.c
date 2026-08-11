/* Verify late arrival at an already-cancelled tree barrier.  This runs the
   same controlled cancellation and checking-barrier sequence as the flat
   variant while selecting the tree arrival protocol.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include "cancel-late-arrival-1.c"
