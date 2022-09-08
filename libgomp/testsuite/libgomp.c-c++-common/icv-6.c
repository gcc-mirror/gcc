/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "42" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV "43" } */
/* { dg-set-target-env-var OMP_SCHEDULE_ALL "guided,4" } */
/* { dg-set-target-env-var OMP_DYNAMIC_ALL "true" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "44" } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT_ALL "45" } */
/* { dg-set-target-env-var OMP_NUM_THREADS_ALL "46,3,2" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS_ALL "47" } */
/* { dg-set-target-env-var OMP_PROC_BIND_ALL "spread" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY_ALL "active" } */

/* This tests the hierarchical usage of ICVs on the device, i.e. if
   OMP_NUM_TEAMS_DEV_<device_num> is not configured, then the value of
   OMP_NUM_TEAMS_DEV should be used.  And if there is no environment variable
   without suffix, then the corresponding _ALL variant should be used.  */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  enum omp_sched_t kind;
  int chunk_size;
  omp_get_schedule(&kind, &chunk_size);

  if (omp_get_max_teams () != 42
      || !omp_get_dynamic ()
      || kind != 3 || chunk_size != 4
      || omp_get_teams_thread_limit () != 44
      || omp_get_thread_limit () != 45
      || omp_get_max_threads () != 46
      || omp_get_proc_bind () != omp_proc_bind_spread
      || omp_get_max_active_levels () != 47)
    abort ();

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i=0; i < num_devices; i++)
    #pragma omp target device (i)
      if (omp_get_max_teams () != 43)
	abort ();

  return 0;
}
