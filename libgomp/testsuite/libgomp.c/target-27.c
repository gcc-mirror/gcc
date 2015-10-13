#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int x = 0, y = 0, z = 0, err;
  int shared_mem = 0;
  #pragma omp target map(to: shared_mem)
  shared_mem = 1;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task depend(in: x)
    {
      usleep (5000);
      x = 1;
    }
    #pragma omp task depend(in: x)
    {
      usleep (6000);
      y = 2;
    }
    #pragma omp task depend(out: z)
    {
      usleep (7000);
      z = 3;
    }
    #pragma omp target enter data map(to: x, y, z) depend(inout: x, z) nowait
    #pragma omp task depend(inout: x, z)
    {
      x++; y++; z++;
    }
    #pragma omp target update to(x, y) depend(inout: x) nowait
    #pragma omp target enter data map(always, to: z) depend(inout: z) nowait
    #pragma omp target map (alloc: x, y, z) map (from: err) depend(inout: x, z)
    {
      err = x != 2 || y != 3 || z != 4;
      x = 5; y = 6; z = 7;
    }
    #pragma omp task depend(in: x)
    {
      usleep (5000);
      if (!shared_mem)
	x = 1;
    }
    #pragma omp task depend(in: x)
    {
      usleep (6000);
      if (!shared_mem)
	y = 2;
    }
    #pragma omp task depend(out: z)
    {
      usleep (3000);
      if (!shared_mem)
	z = 3;
    }
    #pragma omp target exit data map(release: z) depend(inout: z) nowait
    #pragma omp target exit data map(from: x, y) depend(inout: x) nowait
    #pragma omp target exit data map(from: z) depend(inout: z) nowait
    #pragma omp taskwait
    if (err || x != 5 || y != 6 || z != 7)
      abort ();
  }
  return 0;
}
