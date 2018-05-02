/* { dg-do run } */

#ifndef NONMONOTONIC_TYPE
#include <omp.h>
#include <stdlib.h>
#define NONMONOTONIC_TYPE int
#define NONMONOTONIC_END(n) n
#endif

int a[73];

int
main ()
{
  NONMONOTONIC_TYPE i;
  #pragma omp parallel for schedule(nonmonotonic: dynamic)
  for (i = 0; i < NONMONOTONIC_END (73); i++)
    a[i]++;
  #pragma omp parallel for schedule(nonmonotonic: dynamic, 5)
  for (i = 0; i < NONMONOTONIC_END (73); i++)
    a[i]++;
  #pragma omp parallel for schedule(nonmonotonic: guided)
  for (i = 0; i < NONMONOTONIC_END (73); i++)
    a[i]++;
  #pragma omp parallel for schedule(nonmonotonic: guided, 7)
  for (i = 0; i < NONMONOTONIC_END (73); i++)
    a[i]++;
  #pragma omp parallel
  {
    int cnt = omp_get_num_threads ();
    int thr = omp_get_thread_num ();
    if (thr < 73)
      a[thr]++;
    #pragma omp barrier
    #pragma omp for schedule(nonmonotonic: dynamic)
    for (i = 0; i < NONMONOTONIC_END (73); i++)
      a[i]++;
    #pragma omp for schedule(nonmonotonic: dynamic, 7)
    for (i = 0; i < NONMONOTONIC_END (73); i++)
      a[i]++;
    #pragma omp for schedule(nonmonotonic: guided)
    for (i = 0; i < NONMONOTONIC_END (73); i++)
      a[i]++;
    #pragma omp for schedule(nonmonotonic: guided, 5)
    for (i = 0; i < NONMONOTONIC_END (73); i++)
      a[i]++;
    #pragma omp single private (i)
    for (i = 0; i < 73; i++)
      if (a[i] != 8 + (i < cnt))
	abort ();
  }
  return 0;
}
