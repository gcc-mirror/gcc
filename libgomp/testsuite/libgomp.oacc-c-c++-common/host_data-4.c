/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <openacc.h>
#include <stdlib.h>

#define N 1024

int main (int argc, char* argv[])
{
  int x[N], *xp2;

#pragma acc data copyin (x[0:N])
  {
    int *xp;
#pragma acc host_data use_device (x)
    {
#pragma acc data
      {
        xp = x;
      }
      xp2 = x;
    }

    if (xp != acc_deviceptr (x) || xp2 != xp)
      abort ();
  }

  return 0;
}
