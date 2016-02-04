/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <openacc.h>
#include <stdlib.h>

#define N 1024

int main (int argc, char* argv[])
{
  int x[N], y[N], *yp;

  yp = y + 1;

#pragma acc data copyin (x[0:N])
  {
    int *xp, *yp2;
#pragma acc host_data use_device (x)
    {
#pragma acc data copyin (y)
      {
#pragma acc host_data use_device (yp)
	{
	  xp = x;
	  yp2 = yp;
	}

        if (yp2 != acc_deviceptr (yp))
	  abort ();
      }
    }

    if (xp != acc_deviceptr (x))
      abort ();

  }

  return 0;
}
