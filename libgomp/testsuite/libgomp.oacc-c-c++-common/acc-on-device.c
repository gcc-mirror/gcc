/* { dg-do compile } */
/* We don't expect this to work with optimizations disabled.
   { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <openacc.h>

int Foo (acc_device_t x)
{
  return acc_on_device (x);
}

/* { dg-final { scan-assembler-not "acc_on_device" } } */
