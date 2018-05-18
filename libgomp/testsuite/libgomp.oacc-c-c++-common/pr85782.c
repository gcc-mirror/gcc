/* PR c++/85782 */

#include <assert.h>

#define N 100

int
main ()
{
  int i, a[N];

  for (i = 0; i < N; i++)
    a[i] = i+1;

  #pragma acc parallel loop copy(a)
  for (i = 0; i < N; i++)
    {
      if (i % 2)
	continue;
      a[i] = 0;
    }

  for (i = 0; i < N; i++)
    {
      if (i % 2)
	assert (a[i] == i+1);
      else
	assert (a[i] == 0);
    }

    return 0;
}
