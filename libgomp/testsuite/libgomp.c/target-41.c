/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

#include <omp.h>
#include <stdlib.h>

int v;

void
foo (void)
{
  v++;
}

#pragma omp declare target enter (v, foo)

int
main ()
{
  /* OMP_TARGET_OFFLOAD=mandatory shouldn't fail for host fallback
     if it is because the program explicitly asked for the host
     fallback through if(false) or omp_get_initial_device () or
     omp_initial_device as the device.  */
  #pragma omp target if (v)
  foo ();
  #pragma omp target device (omp_initial_device)
  foo ();
  #pragma omp target device (omp_get_initial_device ())
  foo ();
  omp_set_default_device (omp_get_initial_device ());
  #pragma omp target
  foo ();
  if (v != 4)
    abort ();
  return 0;
}
