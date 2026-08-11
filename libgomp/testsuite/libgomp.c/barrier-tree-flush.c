/* Verify that data is flushed through the radix-four tree barrier and through
   release of a retained team.  Team sizes 15, 16, and 17 exercise a partial
   second tree level, its exact boundary, and the first node beyond that
   boundary.

   Each thread writes ordinary, non-atomic data before an explicit barrier and
   every thread verifies that the complete payload has been flushed
   afterwards.  Consecutive parallel regions also verify that the primary
   thread's writes are flushed when workers held at the preceding final
   barrier are released for their next team.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <stdlib.h>

enum { max_threads = 17, rounds = 24 };

static int slots[max_threads];
static int next_epoch;

static void
run_size (int nthreads, int epoch)
{
  next_epoch = epoch;
  for (int i = 0; i < nthreads; ++i)
    slots[i] = 0;

#pragma omp parallel num_threads(nthreads) firstprivate(nthreads, epoch)
  {
    int tid = omp_get_thread_num ();

    if (next_epoch != epoch)
      abort ();

    slots[tid] = epoch;
#pragma omp barrier

    for (int i = 0; i < nthreads; ++i)
      if (slots[i] != epoch)
	abort ();
  }

  for (int i = 0; i < nthreads; ++i)
    if (slots[i] != epoch)
      abort ();
}

int
main (void)
{
  static const int sizes[] = { 15, 16, 17, 16, 15, 17 };

  omp_set_dynamic (0);
  for (int round = 0; round < rounds; ++round)
    run_size (sizes[round % 6], round + 1);
  return 0;
}
