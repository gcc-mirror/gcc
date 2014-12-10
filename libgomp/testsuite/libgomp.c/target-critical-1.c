/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

#define N 2000

#pragma omp declare target
int foo ()
{
  int A[N];
  int i, nthreads;
  int res = 0;

  #pragma omp parallel shared (A, nthreads)
    {
      #pragma omp master
	nthreads = omp_get_num_threads ();

      #pragma omp for
	for (i = 0; i < N; i++)
	  A[i] = 0;

      #pragma omp critical (crit1)
        for (i = 0; i < N; i++)
	  A[i]++;
    }

  for (i = 0; i < N; i++)
    if (A[i] != nthreads)
      res = 1;

  return res;
}
#pragma omp end declare target

int main ()
{
  int res1, res2;

  #pragma omp target map (from: res1, res2)
    {
      int B[N];
      int i, nthreads;

      res1 = foo ();

      #pragma omp parallel shared (B, nthreads)
	{
	  #pragma omp master
	    nthreads = omp_get_num_threads ();

	  #pragma omp for
	    for (i = 0; i < N; i++)
	      B[i] = 0;

	  #pragma omp critical (crit2)
	    for (i = 0; i < N; i++)
	      B[i]++;
	}

      res2 = 0;
      for (i = 0; i < N; i++)
	if (B[i] != nthreads)
	  res2 = 1;
    }

  if (res1 || res2)
    abort ();

  return 0;
}
