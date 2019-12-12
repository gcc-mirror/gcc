/* Test present data clauses in acc offloaded regions when the
   subarray inside the present clause does not have the same base
   offset value as the subarray in the enclosing acc data or acc enter
   data variable.  */

#include <assert.h>

void
offset (int *data, int n)
{
  int i;

#pragma acc parallel loop present (data[0:n])
  for (i = 0; i < n; i++)
    data[i] = n;
}

int
main ()
{
  const int n = 30;
  int data[n], i;

  for (i = 0; i < n; i++)
    data[i] = -1;

#pragma acc data copy(data[0:n])
  {
    offset (data + 10, 10);
  }

  for (i = 0; i < n; i++)
    {
      if (i < 10 || i >= 20)
	assert (data[i] == -1);
      else
	assert (data[i] == 10);
    }

  return 0;
}
