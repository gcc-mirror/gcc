/* Test no_create clause with attach/detach when data is not present on the
   device.  */

#include <stdlib.h>
#include <stdio.h>
#include <openacc.h>

#define N 128

typedef struct {
  int x;
  int *y;
} mystruct;

int
main (int argc, char *argv[])
{
  int *devptr;
  mystruct s;

  s.x = 5;
  s.y = (int *) malloc (N * sizeof (int));

  #pragma acc data copyin(s)
  {
    #pragma acc parallel no_create(s.y[0:N]) copyout(devptr)
    {
      devptr = &s.y[2];
    }
  }

  if (devptr != &s.y[2])
    __builtin_abort ();

  free (s.y);

  return 0;
}
