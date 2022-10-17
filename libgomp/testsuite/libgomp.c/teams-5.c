#include <omp.h>
#include <stdlib.h>

int
main ()
{
  #pragma omp teams num_teams (2)
  {
    if (omp_get_num_teams () != 2)
      abort ();
    #pragma omp parallel if (0)
    #pragma omp target
    if (omp_get_num_teams () != 1 || omp_get_team_num () != 0)
      abort ();
  }
  return 0;
}
