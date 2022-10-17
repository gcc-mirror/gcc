#include <unistd.h>
#include <stdlib.h>

int
main ()
{
  int a[128];
  #pragma omp teams num_teams(5)
  {
    #pragma omp loop bind(teams)
    for (int i = 0; i < 128; i++)
      {
	a[i] = i;
	if (i == 0)
	  usleep (20);
	else if (i == 17)
	  usleep (40);
      }
    #pragma omp loop bind(teams)
    for (int i = 0; i < 128; i++)
      a[i] += i;
  }
  for (int i = 0; i < 128; i++)
    if (a[i] != 2 * i)
      abort ();
  #pragma omp teams num_teams(5)
  {
    #pragma omp loop bind(teams) order(concurrent)
    for (int i = 0; i < 128; i++)
      {
	a[i] *= 2;
	if (i == 1)
	  usleep (20);
	else if (i == 13)
	  usleep (40);
      }
    #pragma omp loop bind(teams) order(concurrent)
    for (int i = 0; i < 128; i++)
      a[i] += i;
  }
  for (int i = 0; i < 128; i++)
    if (a[i] != 5 * i)
      abort ();
  #pragma omp teams num_teams(5)
  {
    #pragma omp loop bind(teams) order(reproducible:concurrent)
    for (int i = 0; i < 128; i++)
      {
	a[i] *= 2;
	if (i == 2)
	  usleep (20);
	else if (i == 105)
	  usleep (40);
      }
    #pragma omp loop bind(teams) order(reproducible:concurrent)
    for (int i = 0; i < 128; i++)
      a[i] += i;
  }
  for (int i = 0; i < 128; i++)
    if (a[i] != 11 * i)
      abort ();
  return 0;
}
