/* Subarray declared on enclosing data construct.  */

#include <assert.h>

int
main ()
{
  int a[100], i;

  for (i = 0; i < 100; i++)
    a[i] = 0;

#pragma acc data copy(a[10:80])
  {
    #pragma acc parallel loop
    for (i = 10; i < 90; i++)
      a[i] = i;
  }

  for (i = 0; i < 100; i++)
    if (i >= 10 && i < 90)
      assert (a[i] == i);
    else
      assert (a[i] == 0);

  return 0;
}

/* { dg-xfail-run-if "PR70828" { ! openacc_host_selected } } */
