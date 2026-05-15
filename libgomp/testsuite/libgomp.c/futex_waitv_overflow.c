/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_SPINCOUNT "0" } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "flat" } */
/* Regression test for bug seen during development.

   This exercises the case where `futex_waitv` is passed values that set the
   top bit of the futex word.  A futex word is defined to be 32 bits.  The
   generation is an unsigned value.  We delay all secondary threads before
   reaching the barrier on the 524289'th time.  This ensures the primary thread
   enters the barrier, does not see any threads arriving, then sleeps in the
   kernel on one of these threads arriving.  */
#include <omp.h>
#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int warmup = 524288;
  int delayed = 1;
  int delay_us = 100000;

  omp_set_dynamic (0);

#pragma omp parallel num_threads(4)
  {
    int tid = omp_get_thread_num ();
    for (int round = 0; round < warmup + delayed; ++round)
      {
	if (round >= warmup && tid != 0)
	  usleep (delay_us);
#pragma omp barrier
      }
  }

  return 0;
}
