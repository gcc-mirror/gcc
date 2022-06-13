/* { dg-shouldfail "omp_invalid_device" } */

#include <omp.h>

void
foo (void)
{
}
#pragma omp declare target enter (foo)

int
main ()
{
  #pragma omp target device (omp_invalid_device)
  foo ();
  return 0;
}

/* { dg-output "omp_invalid_device" } */
