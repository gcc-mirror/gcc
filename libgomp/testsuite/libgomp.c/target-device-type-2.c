/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include <omp.h>

int
f (int *x)
{
  #pragma omp target map(tofrom:x[0:1]) device_type(nohost)
    (*x)++;
  return *x;
}

int
main ()
{
  omp_set_default_device (omp_initial_device);
  int i = 3;
  if (f(&i) != 4)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "= __builtin_omp_is_initial_device \\(\\);" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_error \\(&\"Executing device-type ..nohost.. target region on the host\"\\\[0\\\]," "gimple" } } */

/* { dg-output "libgomp: fatal error: Executing device-type 'nohost' target region on the host" } */
/* { dg-shouldfail "'nohost' target region on the host" } */
