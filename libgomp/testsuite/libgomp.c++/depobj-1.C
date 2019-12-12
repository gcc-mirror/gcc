#include <stdlib.h>
#include <omp.h>

void
dep (omp_depend_t &d1, omp_depend_t *d2)
{
  int x = 1;
  #pragma omp depobj (d1) depend(in: x)
  #pragma omp depobj (*d2) depend(in: x)

  #pragma omp depobj (d2[0]) update(out)
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared (x) depend(depobj:*d2)
    x = 2;
    #pragma omp task shared (x) depend(depobj : d1)
    if (x != 2)
      abort ();
  }
  #pragma omp depobj (d2[0]) destroy
  #pragma omp depobj (d1) destroy
}

template <typename T>
void
dep2 (T &d2)
{
  T d1;
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    #pragma omp depobj (d1) depend(out: x)
    #pragma omp depobj (*&d2) depend (in:x)
    #pragma omp depobj(d2)update(in)
    #pragma omp task shared (x) depend(depobj :d1)
    x = 2;
    #pragma omp task shared (x) depend(depobj: d2)
    if (x != 2)
      abort ();
    #pragma omp taskwait
    #pragma omp depobj(d1)destroy
    #pragma omp depobj((&d2)[0]) destroy
  }
}

template <typename T>
void
dep3 (void)
{
  T d[2];
  #pragma omp parallel
  {
    int x = 1;
    #pragma omp single
    {
      #pragma omp depobj(d[0]) depend(out:x)
      #pragma omp depobj(d[1]) depend(in: x)
      #pragma omp task shared (x) depend(depobj:*d)
      x = 2;
      #pragma omp task shared (x) depend(depobj:*(d + 1))
      if (x != 2)
	abort ();
    }
  }
  #pragma omp depobj(d[0]) destroy
  #pragma omp depobj(d[1]) destroy
}

int xx;
omp_depend_t dd1, dd2;

template <int N>
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
  omp_depend_t d1, d2, d3;
  dep (d1, &d2);
  dep2 <omp_depend_t> (d3);
  dep3 <omp_depend_t> ();
  #pragma omp depobj (dd1) depend (inout: xx)
  #pragma omp depobj (dd2) depend (in : xx)
  antidep <0> ();
  #pragma omp depobj (dd2) destroy
  #pragma omp depobj (dd1) destroy
  return 0;
}
