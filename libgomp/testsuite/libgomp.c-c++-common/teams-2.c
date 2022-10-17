#include <omp.h>
#include <stdlib.h>

int
foo ()
{
  return 934;
}

int
main ()
{
  int a[934] = {};
  int k, e;
  #pragma omp target map(a)
  #pragma omp teams num_teams (foo ())
  {
    int i = omp_get_team_num ();
    if (omp_get_num_teams () != 934
	|| (unsigned) i >= 934U
	|| a[i] != 0)
      abort ();
    ++a[i];
  }
  #pragma omp target map(a)
  #pragma omp teams num_teams (foo () - 50 : foo ())
  {
    int i = omp_get_team_num ();
    int j = omp_get_num_teams ();
    if (j < 884
	|| j > 934
	|| (unsigned) i >= (unsigned) j
	|| a[i] != 1)
      abort ();
    ++a[i];
  }
  #pragma omp target teams map(a) num_teams (foo () / 2)
  {
    int i = omp_get_team_num ();
    if (omp_get_num_teams () != 467
	|| (unsigned) i >= 467U
	|| a[i] != 2)
      abort ();
    ++a[i];
  }
  #pragma omp target teams map(a) num_teams (foo () / 2 - 50 : foo () / 2)
  {
    int i = omp_get_team_num ();
    int j = omp_get_num_teams ();
    if (j < 417
	|| j > 467
	|| (unsigned) i >= (unsigned) j
	|| a[i] != 3)
      abort ();
    ++a[i];
  }
  e = 4;
  for (k = 0; k < 934; k++)
    {
      if (k >= 417 && k < 467 && a[k] == 3)
	e = 3;
      else if (k == 467)
	e = 2;
      else if (k >= 884 && a[k] == 1)
	e = 1;
      if (a[k] != e)
	abort ();
    }
  return 0;
}
