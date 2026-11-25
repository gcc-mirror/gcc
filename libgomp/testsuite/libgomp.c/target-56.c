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
  bool host_flag;
  #pragma omp task shared(host_flag)
    #pragma omp target map(from: host_flag)
      host_flag = omp_is_initial_device();
  #pragma omp taskwait

  /* Due target offload_device, this must be the case;
     the actual check is that this line is reached at
     all without aborting due to not resolved 'mandatory'.  */
  if (host_flag == true)
    __builtin_abort ();
  return 0;
}
