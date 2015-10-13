/* { dg-do run } */

#include <stdlib.h>

#define THRESHOLD 20

#pragma omp declare target
int fib (int n)
{
  if (n <= 0)
    return 0;
  else if (n == 1)
    return 1;
  else
    return fib (n - 1) + fib (n - 2);
}
#pragma omp end declare target

int fib_wrapper (int n)
{
  int x = 0;

  #pragma omp target if(n > THRESHOLD) map(from:x)
    x = fib (n);

  return x;
}

int main ()
{
  if (fib (15) != fib_wrapper (15))
    abort ();
  if (fib (25) != fib_wrapper (25))
    abort ();
  return 0;
}
