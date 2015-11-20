/* { dg-do run } */
/* { dg-set-target-env-var OMP_MAX_TASK_PRIORITY "10" } */

/* This test verifies that the "priority" clause of omp task works as
   advertised.

   Testing the OpenMP task scheduler is a bit tricky, especially when
   trying to determine what ran first (without explicitly calling
   time() and/or synchronizing between threads).  What we do here is
   run in single threaded mode which guarantees that we won't run into
   data races while accessing the "prio" array.

   We give each task a priority from 0..63, while setting
   OMP_MAX_TASK_PRIORITY to 10, which basically gives us 10 lower
   priority tasks, and the rest scheduled to run earlier.  We verify
   that the priority < 10 tasks run last.  */

#include <omp.h>
#include <stdlib.h>

#define N 64

int main()
{
  int tsknum=0, prio[N];
  int max_priority = omp_get_max_task_priority ();
  int saved_tsknum = -1;
  int i;

#pragma omp parallel num_threads(1)
#pragma omp single private (i)
  {
    for (i = 0; i < N; i++)
      #pragma omp task priority(i ^ 1)
      {
	int t;
	#pragma omp atomic capture seq_cst
	t = tsknum++;
	prio[t] = i ^ 1;
      }
    #pragma omp atomic read seq_cst
    saved_tsknum = tsknum;
  }

  /* If any of the tasks have run before all tasks were created, don't
     make any assumption on the task order.  Otherwise, we should have
     tasks with >= max_priority scheduled first in arbitrary order,
     followed by the rest of tasks in decreasing priority order, as
     there is only one thread that can schedule them.  */
  if (saved_tsknum == 0)
    {
      for (i = 0; i < N; i++)
	if (i < N - max_priority)
	  {
	    if (prio[i] < max_priority)
	      abort ();
	  }
	else if (i != N - prio[i] - 1)
	  abort ();
    }
  return 0;
}
