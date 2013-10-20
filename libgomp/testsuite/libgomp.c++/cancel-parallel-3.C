// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <omp.h>
#include <unistd.h>
#include "cancel-test.h"

static inline void
do_some_work (void)
{
  asm volatile ("" : : : "memory");
}

void
foo ()
{
  S a, b, c;
  omp_set_dynamic (0);
  omp_set_schedule (omp_sched_static, 1);
  #pragma omp parallel num_threads (16) private (b) firstprivate (c)
  {
    S d;
    int i, j;
    b.bump ();
    c.bump ();
    do_some_work ();
    #pragma omp barrier
    if (omp_get_thread_num () == 1)
      {
	sleep (2);
	#pragma omp cancellation point parallel
      }
    for (j = 3; j <= 16; j++)
      #pragma omp for schedule (runtime) nowait
      for (i = 0; i < j; i++)
	do_some_work ();
    if (omp_get_thread_num () == 0)
      {
	sleep (1);
	#pragma omp cancel parallel
      }
  }
}

int
main ()
{
  foo ();
  S::verify ();
}
