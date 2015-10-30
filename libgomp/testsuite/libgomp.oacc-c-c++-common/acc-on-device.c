/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#include <openacc.h>

int Foo (acc_device_t x)
{
  return acc_on_device (x);
}

/* { dg-final { scan-assembler-not "acc_on_device" } } */
