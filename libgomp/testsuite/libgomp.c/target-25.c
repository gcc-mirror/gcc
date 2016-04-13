#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int x = 0, y = 0, z = 0, s = 11, t = 12, u = 13, w = 7, err;
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
    #pragma omp target map(tofrom: x) map(from: err) map (to: y, z) depend(inout: x, z)
    err = (x != 1 || y != 2 || z != 3);
    if (err)
      abort ();
    #pragma omp task depend(in: x)
    {
      usleep (5000);
      x = 4;
    }
    #pragma omp task depend(in: x)
    {
      usleep (4000);
      y = 5;
    }
    #pragma omp task depend(in: z)
    {
      usleep (3000);
      z = 6;
    }
    #pragma omp target enter data nowait map (to: w)
    #pragma omp target enter data depend (inout: x, z) map (to: x, y, z)
    #pragma omp target map (alloc: x, y, z) map(from: err)
    {
      err = (x != 4 || y != 5 || z != 6);
      x = 7;
      y = 8;
      z = 9;
    }
    if (err)
      abort ();
    #pragma omp taskwait
    #pragma omp target map (alloc: w) map(from: err)
    {
      err = w != 7;
      w = 17;
    }
    if (err)
      abort (); 
    #pragma omp task depend(in: x)
    {
      usleep (2000);
      s = 14;
    }
    #pragma omp task depend(in: x)
    {
      usleep (3000);
      t = 15;
    }
    #pragma omp task depend(in: z)
    {
      usleep (4000);
      u = 16;
    }
    #pragma omp target exit data depend (inout: x, z) map (from: x, y, z, w)
    if (x != 7 || y != 8 || z != 9 || s != 14 || t != 15 || u != 16 || w != 17)
      abort ();
  }
  return 0;
}
