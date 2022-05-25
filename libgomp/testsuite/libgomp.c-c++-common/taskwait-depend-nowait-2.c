#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int a[48], b = 1;
  #pragma omp parallel num_threads (4)
  {
    #pragma omp barrier
    #pragma omp single
    {
      int i;
      for (i = 0; i < 48; ++i)
	#pragma omp task depend(in: a) shared(a)
	  a[i] = i;
      for (i = 0; i < 32; ++i)
	{
	  #pragma omp taskwait depend(inout: a) nowait
	}
      #pragma omp taskwait
      for (i = 0; i < 48; ++i)
	if (a[i] != i)
	  abort ();
      for (i = 0; i < 48; ++i)
	#pragma omp task depend(in: a) shared(a)
	  a[i] = 2 * i + 1;
      #pragma omp taskgroup
      {
	#pragma omp taskwait depend(inoutset: a) nowait
	#pragma omp taskgroup
	{
	  #pragma omp taskwait depend(inoutset: a) nowait
	}
      }
      for (i = 0; i < 48; ++i)
	if (a[i] != 2 * i + 1)
	  abort ();
      #pragma omp task depend(in: a) shared(a)
      usleep (5000);
      #pragma omp taskgroup
      {
	#pragma omp taskwait depend(inout: a) nowait
      }
    }
  }
  return 0;
}
