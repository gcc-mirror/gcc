/* { dg-do run } */

#include <stdlib.h>

#define N 1024

unsigned int a[N];
unsigned int b[N];
unsigned int c[N];
unsigned int n = N;

int
main (void)
{
  for (unsigned int i = 0; i < n; ++i)
    {
      a[i] = i % 3;
      b[i] = i % 5;
    }

  unsigned int res = 1;
  unsigned long long res2 = 1;
#pragma acc parallel vector_length (128) copyin (a,b) reduction (+:res, res2) copy (res, res2)
  {
#pragma acc loop vector reduction (+:res, res2)
    for (unsigned int i = 0; i < n; i++)
      {
	res += ((a[i] + b[i]) % 2);
	res2 += ((a[i] + b[i]) % 2);
      }
  }

  if (res != 478)
    abort ();
  if (res2 != 478)
    abort ();

  return 0;
}
