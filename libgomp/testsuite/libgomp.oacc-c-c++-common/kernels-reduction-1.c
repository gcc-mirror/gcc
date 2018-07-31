/* Verify that a simple, explicit acc loop reduction works inside
 a kernels region.  */

#include <stdlib.h>

#define N 100

int
main ()
{
  int i, red = 0;

#pragma acc kernels
  {
#pragma acc loop reduction (+:red)
  for (i = 0; i < N; i++)
    red++;
  }

  if (red != N)
    abort ();

  return 0;
}
