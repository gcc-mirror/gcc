/* Test a combined acc parallel loop reduction.  */

/* { dg-do run } */

#include <assert.h>

int
main ()
{
  int i, v1 = 0, v2 = 0, n = 100;

#pragma acc parallel loop reduction(+:v1, v2)
  for (i = 0; i < n; i++)
    {
      v1++;
      v2++;
    }

  assert (v1 == n);
  assert (v2 == n);

  return 0;
}
