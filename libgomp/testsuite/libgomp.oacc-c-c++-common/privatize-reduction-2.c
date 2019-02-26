#include <assert.h>

int
main ()
{
  const int n = 1000;
  int i, j, temp, a[n];

#pragma acc parallel loop
  for (i = 0; i < n; i++)
    {
      temp = i;
#pragma acc loop reduction (+:temp)
      for (j = 0; j < n; j++)
	temp ++;
      a[i] = temp;
    }

  for (i = 0; i < n; i++)
    assert (a[i] == i+n);

  return 0;
}
