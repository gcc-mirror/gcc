#include <float.h>  /* For FLT_EPSILON. */
#include <math.h>  /* For fabs.  */
#include <stdlib.h>  /* For abort.  */


int main()
{
#define N 100
  float b[N];
  float c[N];

#pragma acc enter data create(b)

#pragma acc parallel loop no_create(b) no_create(c)
  for (int i = 0; i < N; ++i)
    b[i] = i;

#pragma acc exit data copyout(b)

  for (int i = 0; i < N; ++i)
    if (fabs (b[i] - i) > 10.0*FLT_EPSILON)
      abort ();

  return 0;
}
