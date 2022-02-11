#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_num_teams () != 1 || omp_get_team_num () != 0)
    abort ();
  #pragma omp parallel num_threads (2)
  if (omp_get_num_teams () != 1 || omp_get_team_num () != 0)
    abort ();
  #pragma omp teams num_teams (4)
  {
    int team = omp_get_team_num ();
    if (omp_get_num_teams () != 4 || (unsigned) team >= 4U)
      abort ();
    #pragma omp parallel num_threads (3)
    if (omp_get_num_teams () != 4 || omp_get_team_num () != team)
      abort ();
    #pragma omp parallel if (0)
    #pragma omp target
    #pragma omp teams num_teams (2)
    if (omp_get_num_teams () != 2
	|| (unsigned) omp_get_team_num () >= 2U)
      abort ();
    if (omp_get_num_teams () != 4 || (unsigned) team >= 4U)
      abort ();
  }
  return 0;
}
