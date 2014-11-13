/* { dg-do run } */

#include <stdlib.h>

void foo ()
{
  int A[30], *p;
  #pragma omp target data map(A[0:10])
    {
      p = &A[0];
      #pragma omp target map(p[3:7]) map(A[0:10])
	{
	  A[2] = 777;
	  A[8] = 777;
	  p[8] = 999;
	}
    }

  if (A[2] != 777 || A[8] != 999)
    abort ();
}

int main ()
{
  foo ();
  return 0;
}
