#include <stdlib.h>
#include <unistd.h>
#include "usleep.h"

int main ()
{
  int a = 0, b = 0, c = 0, d[7];

  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task depend(out: d[0])
      a = 2;

    #pragma omp target enter data nowait map(to: a,b,c) depend(in: d[0]) depend(out: d[1])

    #pragma omp target nowait map(alloc: a) depend(in: d[1]) depend(out: d[2])
      a++;

    #pragma omp target nowait map(alloc: b) depend(in: d[2]) depend(out: d[3])
    {
      tgt_usleep (1000);
      #pragma omp atomic update
      b |= 4;
    }

    #pragma omp target nowait map(alloc: b) depend(in: d[2]) depend(out: d[4])
    {
      tgt_usleep (5000);
      #pragma omp atomic update
      b |= 1;
    }

    #pragma omp target nowait map(alloc: c) depend(in: d[3], d[4]) depend(out: d[5])
    {
      tgt_usleep (5000);
      #pragma omp atomic update
      c |= 8;
    }

    #pragma omp target nowait map(alloc: c) depend(in: d[3], d[4]) depend(out: d[6])
    {
      tgt_usleep (1000);
      #pragma omp atomic update
      c |= 2;
    }

    #pragma omp target exit data map(always,from: a,b,c) depend(in: d[5], d[6])
  }

  if (a != 3 || b != 5 || c != 10)
    abort ();

  return 0;
}
