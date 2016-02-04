/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdlib.h>
#include <openacc.h>

char *global_in_host;

void foo (char *in)
{
  if (!acc_is_present (global_in_host, sizeof (*global_in_host))
      || in != acc_deviceptr (global_in_host))
    abort ();
}

int
main (int argc, char **argv)
{
  char mydata[1024];

  global_in_host = mydata;

#pragma acc data copyin(mydata)
  {
#pragma acc host_data use_device (mydata)
    {
      foo (mydata);
    }
  }

  return 0;
}
