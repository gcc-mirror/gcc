/* { dg-do run } */

/* This tests usage of ICVs on the host and on devices if no corresponding
   environment variables are configured.  */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_max_teams () != 0
      || omp_get_teams_thread_limit () != 0)
    abort ();

  omp_set_num_teams (9);
  omp_set_teams_thread_limit (2);
  if (omp_get_max_teams () != 9
      || omp_get_teams_thread_limit () != 2)
    abort ();

  #pragma omp teams
  if (omp_get_num_teams () > 9
      || omp_get_team_num () >= 9)
    abort ();

  #pragma omp teams num_teams(5)
  if (omp_get_num_teams () > 5
      || omp_get_team_num () >= 5)
    abort ();

  int num_devices = omp_get_num_devices () > 3 ? 3 : omp_get_num_devices ();
  for (int i = 0; i < num_devices; i++)
    {
      #pragma omp target device (i)
      if (omp_get_max_teams () != 0
	  || omp_get_teams_thread_limit () != 0)
	abort ();

      #pragma omp target device (i)
      {
	omp_set_num_teams (8 + i);
	omp_set_teams_thread_limit (3 + i);
	if (omp_get_max_teams () != 8 + i
	    || omp_get_teams_thread_limit () != 3 + i)
	  abort ();
      }

     /* omp_set_num_teams above set the value of nteams-var ICV on device 'i',
	 which has scope 'device' and should be avaible in subsequent target
	 regions.  */
      #pragma omp target device (i)
      if (omp_get_max_teams () != 8 + i
	  || omp_get_teams_thread_limit () != 3 + i)
	abort ();

      #pragma omp target device (i)
      #pragma omp teams
      if (omp_get_num_teams () > 8 + i
	  || omp_get_team_num () >= 8 + i)
	abort ();

      /* NUM_TEAMS clause has priority over previously set NUM_TEAMS value.  */
      #pragma omp target device (i)
      #pragma omp teams num_teams(5 + i)
      if (omp_get_num_teams () > 5 + i
	  || omp_get_team_num () >= 5 + i)
	abort ();
    }

  return 0;
}
