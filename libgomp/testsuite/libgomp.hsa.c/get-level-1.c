#include <omp.h>

int
main ()
{
  int i;
  int level = -1;

#pragma omp target map(tofrom : level)
  {
    level = omp_get_level ();
  }

  if (level != 0)
    __builtin_abort ();

#pragma omp target teams map(tofrom : level)
#pragma omp distribute parallel for default(none) private(i) shared(level)
  for (i = 0; i < 1; ++i)
    level += omp_get_level ();

  if (level != 1)
    __builtin_abort ();

  return 0;
}
