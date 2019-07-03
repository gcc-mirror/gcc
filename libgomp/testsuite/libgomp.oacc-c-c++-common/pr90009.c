/* { dg-do run } */

#include <stdlib.h>

#define N 100

int data[N];

int
main (void)
{
  int n = N, b = 3;
#pragma acc parallel num_workers(2)
  {
    int c;
    if (n)
      c = 0;
    else
      c = b;

#pragma acc loop worker
    for (int i = 0; i < n; i++)
      data[i] = 1;

    if (c)
      data[0] = 2;
  }

  for (int i = 0; i < n; i++)
    if (data[i] != 1)
      abort ();

  return 0;
}
