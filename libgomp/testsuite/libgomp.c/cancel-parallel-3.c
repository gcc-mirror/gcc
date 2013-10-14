/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <omp.h>
#include <unistd.h>

static inline void
do_some_work (void)
{
  asm volatile ("" : : : "memory");
}

int
main ()
{
  omp_set_dynamic (0);
  omp_set_schedule (omp_sched_static, 1);
  #pragma omp parallel num_threads (16)
  {
    int i, j;
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
  return 0;
}
