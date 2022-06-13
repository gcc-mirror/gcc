/* { dg-shouldfail "omp_invalid_device" } */

#include <omp.h>

void
foo (void)
{
}

volatile int dev = omp_invalid_device;

int
main ()
{
  #pragma omp target device (dev)
  foo ();
  return 0;
}

/* { dg-output "omp_invalid_device" } */
