/* Verify that reduction variables can appear in data clause.  */

#include <assert.h>

const int n = 100;

int
main ()
{
  int s = 0;
  int array[n];

  for (int i = 0; i < n; i++)
    array[i] = i+1;

#pragma acc parallel loop num_gangs (10) copy (s) reduction (+:s)
  for (int i = 0; i < n; i++)
    s += array[i];

#pragma acc parallel loop num_gangs (10) reduction (+:s) copy (s)
  for (int i = 0; i < n; i++)
    s += array[i];

  assert (s == n * (n + 1));

  return 0;
}
