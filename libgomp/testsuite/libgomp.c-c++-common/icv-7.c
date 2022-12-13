/* { dg-do run } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "7" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "2" } */

/* This tests the hierarchical usage of ICVs on the host and on devices, i.e. if
   OMP_NUM_TEAMS_DEV_<device_num>, OMP_NUM_TEAMS_DEV, and
   OMP_NUM_TEAMS are not configured, then the value of
   OMP_NUM_TEAMS_ALL should be used for the host as well as for the
   devices.  */

#include <omp.h>
#include <stdlib.h>
#include <string.h>

int
main ()
{
  if ((!getenv ("OMP_NUM_TEAMS") && omp_get_max_teams () != 7)
      || (!getenv ("OMP_TEAMS_THREAD_LIMIT") && omp_get_teams_thread_limit () != 2))
    abort ();

  #pragma omp teams
  if ((!getenv ("OMP_NUM_TEAMS"))
      && (omp_get_num_teams () > 7 || omp_get_team_num () >= 7))
    abort ();

  omp_set_num_teams (9);
  omp_set_teams_thread_limit (3);
  if (omp_get_max_teams () != 9
      || omp_get_teams_thread_limit () != 3)
    abort ();

  #pragma omp teams
  if (omp_get_num_teams () > 9
      || omp_get_team_num () >= 9)
    abort ();

  #pragma omp teams num_teams(5)
  if (omp_get_num_teams () > 5
      || omp_get_team_num () >= 5)
    abort ();

  if (getenv ("OMP_NUM_TEAMS_DEV") || getenv ("OMP_TEAMS_THREAD_LIMIT_DEV"))
    return 0;

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i = 0; i < num_devices; i++)
    {
      char nteams[sizeof ("OMP_NUM_TEAMS_DEV_1")];
      strcpy (nteams, "OMP_NUM_TEAMS_DEV_1");
      nteams[sizeof ("OMP_NUM_TEAMS_DEV_1") - 2] = '0' + i;
      char teams_thread_limit[sizeof ("OMP_TEAMS_THREAD_LIMIT_DEV_1")];
      strcpy (teams_thread_limit, "OMP_TEAMS_THREAD_LIMIT_DEV_1");
      teams_thread_limit[sizeof ("OMP_TEAMS_THREAD_LIMIT_DEV_1") - 2] = '0' + i;
      if (getenv (nteams) || getenv (teams_thread_limit))
	continue;

      #pragma omp target device (i)
      if (omp_get_max_teams () != 7
	  || omp_get_teams_thread_limit () != 2)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      if (omp_get_num_teams () > 7
	  || omp_get_team_num () >= 7)
	abort ();

      #pragma omp target device (i)
      {
	omp_set_num_teams (8 + i);
	omp_set_teams_thread_limit (4 + i);
	if (omp_get_max_teams () != 8 + i
	    || omp_get_teams_thread_limit () != 4 + i)
	  abort ();
      }

     /* omp_set_num_teams above set the value of nteams-var ICV on device 'i',
	 which has scope 'device' and should be avaible in subsequent target
	 regions.  */
      #pragma omp target device (i)
      if (omp_get_max_teams () != 8 + i
	  || omp_get_teams_thread_limit () != 4 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      if (omp_get_num_teams () > 8 + i
	  || omp_get_team_num () >= 8 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams num_teams(5 + i)
      if (omp_get_num_teams () != 5 + i)
	abort ();
    }

  return 0;
}
