/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "3" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV "4" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "2" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV "3" } */
/* { dg-set-target-env-var OMP_SCHEDULE_ALL "guided,4" } */
/* { dg-set-target-env-var OMP_DYNAMIC_ALL "true" } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT_ALL "45" } */
/* { dg-set-target-env-var OMP_NUM_THREADS_ALL "46,3,2" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS_ALL "47" } */
/* { dg-set-target-env-var OMP_PROC_BIND_ALL "spread" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY_ALL "active" } */

/* This tests the hierarchical usage of ICVs on the device, i.e. if
   OMP_NUM_TEAMS_DEV_<device_num> is not configured, then the value of
   OMP_NUM_TEAMS_DEV should be used.  And if OMP_NUM_TEAMS (without suffix) is
   not defined, then OMP_NUM_TEAMS_ALL should be used for the host.  */

#include <omp.h>
#include <stdlib.h>
#include <string.h>

int
main ()
{
  enum omp_sched_t kind;
  int chunk_size;
  omp_get_schedule(&kind, &chunk_size);

  if ((!getenv ("OMP_NUM_TEAMS") && omp_get_max_teams () != 3)
      || (!getenv ("OMP_DYNAMIC") && !omp_get_dynamic ())
      || (!getenv ("OMP_SCHEDULE") && (kind != 3 || chunk_size != 4))
      || (!getenv ("OMP_TEAMS_THREAD_LIMIT") && omp_get_teams_thread_limit () != 2)
      || (!getenv ("OMP_THREAD_LIMIT") && omp_get_thread_limit () != 45)
      || (!getenv ("OMP_NUM_THREADS") && omp_get_max_threads () != 46)
      || (!getenv ("OMP_PROC_BIND") && omp_get_proc_bind () != omp_proc_bind_spread)
      || (!getenv ("OMP_MAX_ACTIVE_LEVELS") && omp_get_max_active_levels () != 47))
    abort ();

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i = 0; i < num_devices; i++)
    {
      char name[sizeof ("OMP_NUM_TEAMS_DEV_1")];
      strcpy (name, "OMP_NUM_TEAMS_DEV_1");
      name[sizeof ("OMP_NUM_TEAMS_DEV_1") - 2] = '0' + i;
      if (getenv (name))
	continue;

      #pragma omp target device (i)
      if (omp_get_max_teams () != 4
	  || omp_get_teams_thread_limit () != 3)
	abort ();
      #pragma omp target device (i)
      #pragma omp teams
      {
	if (omp_get_num_teams () > 4
	    || omp_get_team_num () >= 4)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 3
	    || omp_get_thread_num () >= 3)
	  abort ();
      }

      #pragma omp target device (i)
      {
	omp_set_num_teams (3 + i);
	omp_set_teams_thread_limit (2 + i);
	if (omp_get_max_teams () != 3 + i
	    || omp_get_teams_thread_limit () != 2 + i)
	  abort ();
      }

     /* omp_set_num_teams above set the value of nteams-var ICV on device 'i',
	 which has scope 'device' and should be avaible in subsequent target
	 regions.  */
      #pragma omp target device (i)
      if (omp_get_max_teams () != 3 + i
	  || omp_get_teams_thread_limit () != 2 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      {
	if (omp_get_num_teams () > 3 + i
	    || omp_get_team_num () >= 3 + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 2 + i
	    || omp_get_thread_num () >= 2 + i)
	  abort ();
      }
    }

  return 0;
}
