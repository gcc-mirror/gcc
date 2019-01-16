#include <stdlib.h>
#include <omp.h>

void
dep (void)
{
  int x = 1;
  omp_depend_t d1, d2;
  #pragma omp depobj (d1) depend(in: x)
  #pragma omp depobj (d2) depend(in: x)
  #pragma omp depobj (d2) update(out)
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared (x) depend(depobj: d2)
    x = 2;
    #pragma omp task shared (x) depend(depobj: d1)
    if (x != 2)
      abort ();
  }
  #pragma omp depobj (d2) destroy
  #pragma omp depobj (d1) destroy
}

void
dep2 (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    omp_depend_t d1, d2;
    #pragma omp depobj (d1) depend(out: x)
    #pragma omp depobj (*&d2) depend (in:x)
    #pragma omp depobj(d2)update(in)
    #pragma omp task shared (x) depend(depobj:d1)
    x = 2;
    #pragma omp task shared (x) depend(depobj : d2)
    if (x != 2)
      abort ();
    #pragma omp taskwait
    #pragma omp depobj(d1)destroy
    #pragma omp depobj((&d2)[0]) destroy
  }
}

void
dep3 (void)
{
  omp_depend_t d[2];
  #pragma omp parallel
  {
    int x = 1;
    #pragma omp single
    {
      #pragma omp depobj(d[0]) depend(out:x)
      #pragma omp depobj(d[1]) depend(in: x)
      #pragma omp task shared (x) depend(depobj: *d)
      x = 2;
      #pragma omp task shared (x) depend(depobj: *(d + 1))
      if (x != 2)
	abort ();
    }
  }
  #pragma omp depobj(d[0]) destroy
  #pragma omp depobj(d[1]) destroy
}

int xx;
omp_depend_t dd1, dd2;

void
antidep (void)
{
  xx = 1;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared(xx) depend(depobj:dd2)
    if (xx != 1)
      abort ();
    #pragma omp task shared(xx) depend(depobj:dd1)
    xx = 2;
  }
}

int
main ()
{
  dep ();
  dep2 ();
  dep3 ();
  #pragma omp depobj (dd1) depend (inout: xx)
  #pragma omp depobj (dd2) depend (in : xx)
  antidep ();
  #pragma omp depobj (dd2) destroy
  #pragma omp depobj (dd1) destroy
  return 0;
}
