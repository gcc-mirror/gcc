/* { dg-do run } */
/* { dg-options "-O1 -ftree-parallelize-loops=0" } */
/* { dg-additional-options "-flto" { target lto } } */

#include <stdlib.h>

#define N 123456

#pragma omp declare target
int X, Y;
#pragma omp end declare target

void
foo ()
{
  #pragma omp target map(alloc: X)
    X = N;
}

int
main ()
{
  int res;

  foo ();

  #pragma omp target map(alloc: X, Y) map(from: res)
    {
      Y = N;
      res = X + Y;
    }

  if (res != N + N)
    abort ();

  return 0;
}
