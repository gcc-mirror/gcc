#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_get_level ())
    abort ();
  #pragma omp target if (0)
  if (omp_get_level ())
    abort ();
  #pragma omp target if (0)
  #pragma omp teams
  #pragma omp distribute dist_schedule(static,1)
  for (int i = 0; i < 1; ++i)
    if (omp_get_level ())
      abort ();
  return 0;
}
