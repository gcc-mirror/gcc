/* Check a parallel reduction which is are explicitly initialized by
   the user.  */

#include <assert.h>

int
main ()
{
  int n = 10;
  float accel = 1.0, host = 1.0;
  int i;

#pragma acc parallel copyin(n) reduction(*:accel)
  {
    accel = 1.0;
#pragma acc loop gang reduction(*:accel)
    for( i = 1; i <= n; i++)
      {
	accel *= 2.0;
      }
  }

  for (i = 1; i <= n; i++)
    host *= 2.0;

  assert (accel == host);

  return 0;
}
