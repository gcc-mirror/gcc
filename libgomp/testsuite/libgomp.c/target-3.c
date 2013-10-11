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
  if (omp_get_level ())
    abort ();
  return 0;
}
