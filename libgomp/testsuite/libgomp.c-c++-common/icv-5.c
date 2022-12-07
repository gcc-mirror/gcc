/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "3" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV "4" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS "5" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_0 "6" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_1 "7" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_2 "8" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "2" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV "3" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT "4" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_0 "5" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_1 "6" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_2 "7" } */

#include <omp.h>
#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  if (omp_get_max_teams () != 5
      || omp_get_teams_thread_limit () != 4)
    abort ();

  #pragma omp teams
  {
    if (omp_get_num_teams () > 5
	|| omp_get_team_num () >= 5)
      abort ();
    #pragma omp parallel
    if (omp_get_thread_limit () > 4
	|| omp_get_thread_num () >= 4)
      abort ();
  }

  omp_set_num_teams (4);
  omp_set_teams_thread_limit (3);
  if (omp_get_max_teams () != 4
      || omp_get_teams_thread_limit () != 3)
    abort ();

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

  #pragma omp teams num_teams(3) thread_limit(2)
  {
    if (omp_get_num_teams () != 3
	|| omp_get_team_num () >= 3)
    abort ();
    #pragma omp parallel
    if (omp_get_thread_limit () > 2
	|| omp_get_thread_num () >= 2)
      abort ();
  }

  #pragma omp teams num_teams(5) thread_limit(4)
  {
    if (omp_get_num_teams () != 5
	|| omp_get_team_num () >= 5)
    abort ();
    #pragma omp parallel
    if (omp_get_thread_limit () > 4
	|| omp_get_thread_num () >= 4)
      abort ();
  }

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();

  for (int i = 0; i < num_devices; i++)
    {
      #pragma omp target device (i)
      if (omp_get_max_teams () != 6 + i
	  || omp_get_teams_thread_limit () != 5 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      #pragma omp parallel
      if (omp_get_thread_limit () > 5 + i
	  || omp_get_thread_num () >= 5 + i)
	abort ();

      #pragma omp target device (i)
      {
	omp_set_num_teams (5 + i);
	omp_set_teams_thread_limit (4 + i);
	if (omp_get_max_teams () != 5 + i
	    || omp_get_teams_thread_limit () != 4 + i)
	  abort ();
      }

      /* omp_set_num_teams and omp_set_teams_thread_limit above set the value
	 of nteams-var and teams-thread-limit-var ICVs on device 'i', which has
	 scope 'device' and should be avaible in subsequent target regions.  */
      #pragma omp target device (i)
      if (omp_get_max_teams () != 5 + i
	  || omp_get_teams_thread_limit () != 4 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      {
	if (omp_get_num_teams () > 5 + i
	    || omp_get_team_num () >= 5 + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 4 + i
	    || omp_get_thread_num () >= 4 + i)
	  abort ();
      }

      #pragma omp target device (i)
      #pragma omp teams num_teams(6 + i) thread_limit(5 + i)
      {
	if (omp_get_num_teams () > 6 + i
	    || omp_get_team_num () >= 6 + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 5 + i
	    || omp_get_thread_num () >= 5 + i
	    || omp_get_num_teams () > 6 + i
	    || omp_get_team_num () >= 6 + i)
	  abort ();
      }

      #pragma omp target device (i)
      #pragma omp teams num_teams(4 + i) thread_limit(3 + i)
      {
	if (omp_get_num_teams () > 4 + i
	    || omp_get_team_num () >= 4 + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 3 + i
	    || omp_get_thread_num () >= 3 + i
	    || omp_get_num_teams () > 4 + i
	    || omp_get_team_num () >= 4 + i)
	  abort ();
      }

      #pragma omp target device (i)
      #pragma omp teams thread_limit(3 + i) num_teams(4 + i)
      {
	if (omp_get_num_teams () > 4 + i
	    || omp_get_team_num () >= 4 + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > 3 + i
	    || omp_get_thread_num () >= 3 + i
	    || omp_get_num_teams () > 4 + i
	    || omp_get_team_num () >= 4 + i)
	  abort ();
      }

      /* The NUM_TEAMS and THREAD_LIMIT clauses should not change the values
	 of the corresponding ICVs.  */
      #pragma omp target device (i)
      if (omp_get_max_teams () != 5 + i
	  || omp_get_teams_thread_limit () != 4 + i)
	abort ();

      /* This tests a large number of teams and threads.  If it is larger than
	 2^15+1 then the according argument in the kernels arguments list
	 is encoded with two items instead of one.  */
      intptr_t large_num_teams = 66000;
      intptr_t large_threads_limit = 67000;
      #pragma omp target device (i)
      {
	omp_set_num_teams (large_num_teams + i);
	omp_set_teams_thread_limit (large_threads_limit + i);
	if (omp_get_max_teams () != large_num_teams + i
	    || omp_get_teams_thread_limit () != large_threads_limit + i)
	  abort ();
      }

      #pragma omp target device (i)
	if (omp_get_max_teams () != large_num_teams + i
	    || omp_get_teams_thread_limit () != large_threads_limit + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      {
	if (omp_get_num_teams () > large_num_teams + i
	    || omp_get_team_num () >= large_num_teams + i)
	  abort ();
	#pragma omp parallel
	if (omp_get_thread_limit () > large_threads_limit + i
	    || omp_get_thread_num () >= large_threads_limit + i)
	  abort ();
      }
    }

  return 0;
}
