/* Verify logical barrier IDs while an outer worker is primary of a nested
   team.  Outer logical thread 1 becomes logical thread 0 in the nested team,
   exercises direct, work-share, copyprivate, tasking, and final barriers,
   then must regain outer ID 1 before using the outer barrier again.

   The test covers the ID arguments added to the barrier interfaces and the
   restoration of per-thread team state across active nesting.  */
/* { dg-do run { target linux_futex_waitv } } */
/* { dg-set-target-env-var GOMP_BARRIER_TYPE "tree" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS "2" } */

#include <omp.h>
#include <stdlib.h>

enum { rounds = 24 };

int
main (void)
{
  omp_set_dynamic (0);
  omp_set_max_active_levels (2);

  for (int round = 0; round < rounds; ++round)
    {
      int loop_total = 0;
      int task_total = 0;

#pragma omp parallel num_threads(3) firstprivate(round) \
  shared(loop_total, task_total)
      {
	int outer_id = omp_get_thread_num ();

	if (outer_id == 1)
	  {
	    int inner_threads = 2 + round % 4;

#pragma omp parallel num_threads(inner_threads) \
  firstprivate(inner_threads) \
  shared(loop_total, task_total)
	    {
	      int copied = -1;

	      if (omp_get_num_threads () != inner_threads)
		abort ();

#pragma omp barrier
#pragma omp for reduction(+:loop_total)
	      for (int i = 0; i < inner_threads; ++i)
		loop_total++;

#pragma omp single copyprivate(copied)
	      copied = 0x1234;
	      if (copied != 0x1234)
		abort ();

#pragma omp single nowait
#pragma omp task shared(task_total)
	      {
#pragma omp atomic update
		task_total++;
	      }
	    }

	    if (omp_get_thread_num () != outer_id)
	      abort ();
	  }

#pragma omp barrier
	if (omp_get_thread_num () != outer_id)
	  abort ();
      }

      if (loop_total != 2 + round % 4 || task_total != 1)
	abort ();
    }

  return 0;
}
