/* { dg-do run } */

#include <stdlib.h>

void foo ()
{
  int A[30], *p;
  #pragma omp target data map(A[0:4])
    {
      p = &A[0];
      #pragma omp target map(p[7:20]) map(A[0:4])
	{
	  A[2] = 777;
	  p[8] = 777;
	}
    }

  if (A[2] != 777 || A[8] != 777)
    abort ();
}

int main ()
{
  foo ();
  return 0;
}
