#include <omp.h>
#include <stdlib.h>

int
main ()
{
  #pragma omp teams num_teams (5)
  {
    if (omp_get_num_teams () != 5)
      abort ();
    #pragma omp distribute dist_schedule(static,1)
    for (int i = 0; i < 5; ++i)
      if (omp_get_team_num () != i)
	abort ();
  }
  #pragma omp teams num_teams (7 : 9)
  {
    if (omp_get_num_teams () < 7 || omp_get_num_teams () > 9)
      abort ();
    #pragma omp distribute dist_schedule(static,1)
    for (int i = 0; i < omp_get_num_teams (); ++i)
      if (omp_get_team_num () != i)
	abort ();
  }
  return 0;
}
