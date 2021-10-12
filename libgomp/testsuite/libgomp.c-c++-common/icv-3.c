#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (getenv ("OMP_NUM_TEAMS") == NULL
      && omp_get_max_teams () != 0)
    abort ();
  omp_set_num_teams (7);
  if (omp_get_max_teams () != 7)
    abort ();
  if (getenv ("OMP_TEAMS_THREAD_LIMIT") == NULL
      && omp_get_teams_thread_limit () != 0)
    abort ();
  omp_set_teams_thread_limit (15);
  if (omp_get_teams_thread_limit () != 15)
    abort ();
  #pragma omp teams
  {
    if (omp_get_max_teams () != 7
	|| omp_get_teams_thread_limit () != 15
	|| omp_get_num_teams () < 1
	|| omp_get_num_teams () > 7
	|| omp_get_team_num () < 0
	|| omp_get_team_num () >= omp_get_num_teams ()
	|| omp_get_thread_limit () < 1
	|| omp_get_thread_limit () > 15)
      abort ();
  }
  #pragma omp teams num_teams(5) thread_limit (13)
  {
    if (omp_get_max_teams () != 7
	|| omp_get_teams_thread_limit () != 15
	|| omp_get_num_teams () != 5
	|| omp_get_team_num () < 0
	|| omp_get_team_num () >= omp_get_num_teams ()
	|| omp_get_thread_limit () < 1
	|| omp_get_thread_limit () > 13)
      abort ();
  }
  #pragma omp teams num_teams(8) thread_limit (16)
  {
    if (omp_get_max_teams () != 7
	|| omp_get_teams_thread_limit () != 15
	|| omp_get_num_teams () != 8
	|| omp_get_team_num () < 0
	|| omp_get_team_num () >= omp_get_num_teams ()
	|| omp_get_thread_limit () < 1
	|| omp_get_thread_limit () > 16)
      abort ();
  }
  return 0;
}
