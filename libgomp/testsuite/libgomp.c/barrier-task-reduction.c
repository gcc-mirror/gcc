/* Verify the additional team barrier used in
   GOMP_workshare_task_reduction_unregister.  Each loop iteration creates an
   in_reduction task, so the loop-end scheduling point must finish every task
   and reduction before GOMP_workshare_task_reduction_unregister synchronizes
   all logical IDs.

   Repeated 2 -> 4 -> 1 -> 3 transitions exercise task-reduction allocation,
   unregister, and retained-team reuse with the tree barrier.  This is a
   focused complement to broad task-reduction language tests.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */

#include <omp.h>
#include <stdlib.h>

enum { rounds = 8 };

static long long reduction_sum;
static int completed;

static void
run_region (int nthreads, int epoch)
{
  long long expected = (long long) nthreads * (nthreads + 1) / 2 + epoch;
  reduction_sum = 0;
  completed = 0;

#pragma omp parallel num_threads(nthreads) \
  firstprivate(epoch, expected, nthreads)
  {
#pragma omp for schedule(static, 1) reduction(task, + : reduction_sum)
    for (int i = 0; i < nthreads; ++i)
      {
	long long contribution = i + 1;
	if (i == 0)
	  contribution += epoch;
#pragma omp task firstprivate(contribution) in_reduction(+ : reduction_sum)
	{
	  reduction_sum += contribution;
#pragma omp atomic update
	  completed++;
	}
      }

    if (reduction_sum != expected || completed != nthreads)
      abort ();
  }

  if (reduction_sum != expected || completed != nthreads)
    abort ();
}

int
main (void)
{
  static const int sizes[] = { 2, 4, 1, 3 };
  int epoch = 0;

  omp_set_dynamic (0);
  for (int round = 0; round < rounds; ++round)
    for (int i = 0; i < 4; ++i)
      run_region (sizes[i], ++epoch);
  return 0;
}
