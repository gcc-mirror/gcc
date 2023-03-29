/* { dg-set-target-env-var OMP_NUM_TEAMS "6" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT "12" } */

#include <omp.h>
#include <stdlib.h>
#include <string.h>

int
main ()
{
  if (getenv ("OMP_NUM_TEAMS") != NULL
      && strcmp (getenv ("OMP_NUM_TEAMS"), "6") == 0)
    {
      if (omp_get_max_teams () != 6)
	abort ();
    }
  else
    omp_set_num_teams (6);
  if (getenv ("OMP_TEAMS_THREAD_LIMIT") != NULL
      && strcmp (getenv ("OMP_TEAMS_THREAD_LIMIT"), "12") == 0)
    {
      if (omp_get_teams_thread_limit () != 12)
	abort ();
    }
  else
    omp_set_teams_thread_limit (12);
  #pragma omp teams
  {
    #pragma omp parallel if(0)
    if (omp_get_max_teams () != 6
	|| omp_get_teams_thread_limit () != 12
	|| omp_get_num_teams () < 1
	|| omp_get_num_teams () > 6
	|| omp_get_team_num () < 0
	|| omp_get_team_num () >= omp_get_num_teams ()
	|| omp_get_thread_limit () < 1
	|| omp_get_thread_limit () > 12)
      abort ();
  }
  return 0;
}
