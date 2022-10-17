#include <unistd.h>
#include <stdlib.h>

int
main ()
{
  int a[128];
  #pragma omp parallel num_threads(8)
  {
    #pragma omp barrier
    #pragma omp for nowait schedule (dynamic, 2) order(reproducible:concurrent)
    for (int i = 0; i < 128; i++)
      {
	a[i] = i;
	if (i == 0)
	  usleep (20);
	else if (i == 17)
	  usleep (40);
      }
    #pragma omp for nowait schedule (dynamic, 2) order(reproducible:concurrent)
    for (int i = 0; i < 128; i++)
      a[i] += i;
  }
  for (int i = 0; i < 128; i++)
    if (a[i] != 2 * i)
      abort ();
  return 0;
}
