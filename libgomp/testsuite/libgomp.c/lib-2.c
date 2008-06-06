#include <stdlib.h>
#include <omp.h>

int
main (void)
{
  omp_sched_t kind;
  int modifier;

  omp_set_schedule (omp_sched_static, 32);
  omp_get_schedule (&kind, &modifier);
  if (kind != omp_sched_static || modifier != 32)
    abort ();
  omp_set_schedule (omp_sched_guided, 4);
  omp_get_schedule (&kind, &modifier);
  if (kind != omp_sched_guided || modifier != 4)
    abort ();
  if (omp_get_thread_limit () < 0)
    abort ();
  omp_set_max_active_levels (6);
  if (omp_get_max_active_levels () != 6)
    abort ();

  return 0;
}
