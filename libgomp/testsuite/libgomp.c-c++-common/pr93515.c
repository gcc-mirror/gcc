/* PR libgomp/93515 */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int i;
  int a = 42;
#pragma omp target teams distribute parallel for defaultmap(tofrom: scalar)
  for (i = 0; i < 64; ++i)
    if (omp_get_team_num () == 0)
      if (omp_get_thread_num () == 0)
	a = 142;
  if (a != 142)
    __builtin_abort ();
  a = 42;
#pragma omp target parallel for defaultmap(tofrom: scalar)
  for (i = 0; i < 64; ++i)
    if (omp_get_thread_num () == 0)
      a = 143;
  if (a != 143)
    __builtin_abort ();
  a = 42;
#pragma omp target firstprivate(a)
  {
    #pragma omp parallel for
    for (i = 0; i < 64; ++i)
      if (omp_get_thread_num () == 0)
	a = 144;
    if (a != 144)
      abort ();
  }
  return 0;
}
