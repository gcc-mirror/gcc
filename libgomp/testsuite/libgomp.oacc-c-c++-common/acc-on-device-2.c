/* Test the acc_on_device library function. */
/* { dg-additional-options "-fno-builtin-acc_on_device" } */

#include <openacc.h>

int main ()
{
  int dev;
  
#pragma acc parallel copyout (dev)
  {
    dev = acc_on_device (acc_device_not_host);
  }

  int expect = 1;
  
#if  ACC_DEVICE_TYPE_host
  expect = 0;
#endif
  
  return dev != expect;
}
