/* Exercise the auto, independent, seq and tile loop clauses inside
   kernels regions.  */

#include <assert.h>

#define N 100

void
check (int *a, int *b)
{
  int i;

  for (i = 0; i < N; i++)
    assert (a[i] == b[i]);
}

int
main ()
{
  int i, a[N], b[N];

#pragma acc kernels copy(a)
  {
#pragma acc loop auto
    for (i = 0; i < N; i++)
      a[i] = i;
  }

  for (i = 0; i < N; i++)
    b[i] = i;

  check (a, b);

#pragma acc kernels copyout(a)
  {
#pragma acc loop independent
    for (i = 0; i < N; i++)
      a[i] = i;
  }

  check (a, b);

#pragma acc kernels present_or_copy(a)
  {
#pragma acc loop seq
    for (i = 0; i < N; i++)
      a[i] = i;
  }

  check (a, b);

#pragma acc kernels pcopyout(a) present_or_copyin(b)
  {
#pragma acc loop seq
    for (i = 0; i < N; i++)
      a[i] = b[i];
  }

  check (a, b);

  return 0;
}
