#include <stdlib.h>

void
dep (void)
{
  int x = 1;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared (x) depend(out: x)
    x = 2;
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
  }
}

void
dep2 (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    #pragma omp task shared (x) depend(out: x)
    x = 2;
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp taskwait
  }
}

void
dep3 (void)
{
  #pragma omp parallel
  {
    int x = 1;
    #pragma omp single
    {
      #pragma omp task shared (x) depend(out: x)
      x = 2;
      #pragma omp task shared (x) depend(in: x)
      if (x != 2)
	abort ();
    }
  }
}

void
firstpriv (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    #pragma omp task depend(out: x)
    x = 2;
    #pragma omp task depend(in: x)
    if (x != 1)
      abort ();
  }
}

void
antidep (void)
{
  int x = 1;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared(x) depend(in: x)
    if (x != 1)
      abort ();
    #pragma omp task shared(x) depend(out: x)
    x = 2;
  }
}

void
antidep2 (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    #pragma omp taskgroup
    {
      #pragma omp task shared(x) depend(in: x)
      if (x != 1)
	abort ();
      #pragma omp task shared(x) depend(out: x)
      x = 2;
    }
  }
}

void
antidep3 (void)
{
  #pragma omp parallel
  {
    int x = 1;
    #pragma omp single
    {
      #pragma omp task shared(x) depend(in: x)
      if (x != 1)
	abort ();
      #pragma omp task shared(x) depend(out: x)
      x = 2;
    }
  }
}


void
outdep (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 0;
    #pragma omp task shared(x) depend(out: x)
    x = 1;
    #pragma omp task shared(x) depend(out: x)
    x = 2;
    #pragma omp taskwait
    if (x != 2)
      abort ();
  }
}

void
concurrent (void)
{
  int x = 1;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared (x) depend(out: x)
    x = 2;
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
  }
}

void
concurrent2 (void)
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1;
    #pragma omp task shared (x) depend(out: x)
    x = 2;
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp task shared (x) depend(in: x)
    if (x != 2)
      abort ();
    #pragma omp taskwait
  }
}

void
concurrent3 (void)
{
  #pragma omp parallel
  {
    int x = 1;
    #pragma omp single
    {
      #pragma omp task shared (x) depend(out: x)
      x = 2;
      #pragma omp task shared (x) depend(in: x)
      if (x != 2)
	abort ();
      #pragma omp task shared (x) depend(in: x)
      if (x != 2)
	abort ();
      #pragma omp task shared (x) depend(in: x)
      if (x != 2)
	abort ();
    }
  }
}

int
main ()
{
  dep ();
  dep2 ();
  dep3 ();
  firstpriv ();
  antidep ();
  antidep2 ();
  antidep3 ();
  outdep ();
  concurrent ();
  concurrent2 ();
  concurrent3 ();
  return 0;
}
