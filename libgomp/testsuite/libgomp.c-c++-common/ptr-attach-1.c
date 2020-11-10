#include <stdlib.h>

struct S
{
  int a, b;
  int *ptr;
  int c, d;
};
typedef struct S S;

#pragma omp declare target
int *gp;
#pragma omp end declare target

#define N 10
int main (void)
{
  /* Test to see if pointer attachment works, for scalar pointers,
     and pointer fields in structures.  */

  int *ptr = (int *) malloc (sizeof (int) * N);
  int *orig_ptr = ptr;

  #pragma omp target map (ptr, ptr[:N])
  {
    for (int i = 0; i < N; i++)
      ptr[i] = N - i;
  }

  if (ptr != orig_ptr)
    abort ();

  for (int i = 0; i < N; i++)
    if (ptr[i] != N - i)
      abort ();

  S s = { 0 };
  s.ptr = ptr;
  #pragma omp target map (s, s.ptr[:N])
  {
    for (int i = 0; i < N; i++)
      s.ptr[i] = i;

    s.a = 1;
    s.b = 2;
  }

  if (s.ptr != ptr)
    abort ();

  for (int i = 0; i < N; i++)
    if (s.ptr[i] != i)
      abort ();

  if (s.a != 1 || s.b != 2 || s.c != 0 || s.d != 0)
    abort ();

  gp = (int *) malloc (sizeof (int) * N);
  orig_ptr = gp;

  for (int i = 0; i < N; i++)
    gp[i] = i - 1;

  #pragma omp target map (gp[:N])
  {
    for (int i = 0; i < N; i++)
      gp[i] += 1;
  }

  if (gp != orig_ptr)
    abort ();

  for (int i = 0; i < N; i++)
    if (gp[i] != i)
      abort ();

  free (ptr);
  free (gp);

  return 0;
}

