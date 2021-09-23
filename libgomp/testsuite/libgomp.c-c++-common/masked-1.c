#include <omp.h>
#include <stdlib.h>

void
foo (int x, int *a)
{
  #pragma omp masked
  {
    if (omp_get_thread_num () != 0)
      abort ();
    a[128]++;
  }
  #pragma omp masked filter (0)
  {
    if (omp_get_thread_num () != 0)
      abort ();
    a[129]++;
  }
  #pragma omp masked filter (7)
  {
    if (omp_get_thread_num () != 7)
      abort ();
    a[130]++;
  }
  #pragma omp masked filter (x)
  {
    if (omp_get_thread_num () != x)
      abort ();
    a[131]++;
  }
  #pragma omp masked taskloop simd filter (x) grainsize (12) simdlen (4)
  for (int i = 0; i < 128; i++)
    a[i] += i;
}

int
main ()
{
  int a[136] = {};
  #pragma omp parallel num_threads (4)
  foo (4, a);
  for (int i = 0; i < 128; i++)
    if (a[i])
      abort ();
  if (a[128] != 1 || a[129] != 1 || a[130] || a[131])
    abort ();
  #pragma omp parallel num_threads (4)
  foo (3, a);
  for (int i = 0; i < 128; i++)
    if (a[i] != i)
      abort ();
  if (a[128] != 2 || a[129] != 2 || a[130] || a[131] != 1)
    abort ();
  #pragma omp parallel num_threads (8)
  foo (8, a);
  for (int i = 0; i < 128; i++)
    if (a[i] != i)
      abort ();
  if (a[128] != 3 || a[129] != 3 || a[130] != 1 || a[131] != 1)
    abort ();
  #pragma omp parallel num_threads (8)
  foo (6, a);
  for (int i = 0; i < 128; i++)
    if (a[i] != 2 * i)
      abort ();
  if (a[128] != 4 || a[129] != 4 || a[130] != 2 || a[131] != 2)
    abort ();
  for (int i = 0; i < 8; i++)
    a[i] = 0;
  /* The filter expression can evaluate to different values in different threads.  */
  #pragma omp parallel masked num_threads (8) filter (omp_get_thread_num () + 1)
  a[omp_get_thread_num ()]++;
  for (int i = 0; i < 8; i++)
    if (a[i])
      abort ();
  /* And multiple threads can be filtered.  */
  #pragma omp parallel masked num_threads (8) filter (omp_get_thread_num () & ~1)
  a[omp_get_thread_num ()]++;
  for (int i = 0; i < 8; i++)
    if (a[i] != !(i & 1))
      abort ();
  return 0;
}
