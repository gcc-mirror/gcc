/* Only run this with when there is only the host; in that case, GCC shall
   set the default-device-var to omp_invalid_device.  */

/* { dg-do run { target { ! offload_device } } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

/* Was failing before because of the created data environment ICV,
   the resolution of the ICV value was not visible when the target region
   was called.  Due to mandatory, it then wrongly claims that no offload
   device was available.  */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  /* No device available. This means 'omp_invalid_device' if the
     env var was successfullly set - and otherwise '0' as default.  */
  const char *env = getenv ("OMP_TARGET_OFFLOAD");
  #pragma omp task
    if ((env != NULL && omp_get_default_device() != omp_invalid_device)
	|| (env == NULL && omp_get_default_device() != 0))
      {
        __builtin_printf ("default-device: %d, env: %s\n", omp_get_default_device(), env);
        __builtin_abort ();
      }
  #pragma omp taskwait

  return 0;
}
