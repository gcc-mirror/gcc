#include <stdlib.h>

#define N 10

struct S
{
  int a, b;
  int *ptr;
  int c, d;
};

int
main (void)
{
  struct S a;
  a.ptr = (int *) malloc (sizeof (int) * N);

  for (int i = 0; i < N; i++)
    a.ptr[i] = 0;

  #pragma omp target enter data map(to: a.ptr[:N])

  #pragma omp target map(a, a.ptr[:0])
  for (int i = 0; i < N; i++)
    a.ptr[i] += 1;

  #pragma omp target exit data map(from: a.ptr[:N])

  for (int i = 0; i < N; i++)
    if (a.ptr[i] != 1)
      abort ();

  free (a.ptr);

  return 0;
}
