/* { dg-shouldfail "omp_invalid_device" } */

#include <omp.h>

void
foo (void)
{
}

int
main ()
{
  omp_set_default_device (omp_invalid_device);
  #pragma omp target
  foo ();
  return 0;
}

/* { dg-output "omp_invalid_device" } */
