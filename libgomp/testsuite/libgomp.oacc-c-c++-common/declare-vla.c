/* Verify that acc declare accept VLA variables.  */

#include <assert.h>

int
main ()
{
  int N = 1000;
  int i, A[N];
#pragma acc declare copy(A)

  for (i = 0; i < N; i++)
    A[i] = -i;

#pragma acc kernels
  for (i = 0; i < N; i++)
    A[i] = i;

#pragma acc update host(A)

  for (i = 0; i < N; i++)
    assert (A[i] == i);

  return 0;
}
