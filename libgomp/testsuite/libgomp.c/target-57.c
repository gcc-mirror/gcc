/* Only run this with available non-host devices; in that case, GCC sets
   the default-device-var to 0.  */

/* { dg-do run { target { offload_device } } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

/* Was failing before because of the created data environment ICV,
   the resolution of the ICV value was not visible when the target region
   was called.  Due to mandatory, it then wrongly claims that no offload
   device was available.  */

#include <omp.h>

int
main ()
{
  /* Due 'target offload_device' a device is available, hence,
     it will return a valid device number - in GCC it is always
     0 unless otherwise overridden.
     The following uses a nested ICV due to 'task'.  */
  #pragma omp task
    if (omp_get_default_device() != 0)
    {
      __builtin_printf ("default-device: %d\n", omp_get_default_device());
      __builtin_abort ();
    }
  #pragma omp taskwait

  return 0;
}
