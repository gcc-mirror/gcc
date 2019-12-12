/* OpenACC parallelism dimensions clauses: num_gangs, num_workers,
   vector_length.  */

/* Copied from '../libgomp.oacc-c-c++-common/parallel-dims.c'.  */

/* Used by 'parallel-dims.f90'.  */

#include <limits.h>
#include <openacc.h>
#include <gomp-constants.h>

/* TODO: "(int) acc_device_*" casts because of the C++ acc_on_device wrapper
   not behaving as expected for -O0.  */
#pragma acc routine seq
/* static */ unsigned int __attribute__ ((optimize ("O2"))) acc_gang ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
  else
    __builtin_abort ();
}

#pragma acc routine seq
/* static */ unsigned int __attribute__ ((optimize ("O2"))) acc_worker ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
  else
    __builtin_abort ();
}

#pragma acc routine seq
/* static */ unsigned int __attribute__ ((optimize ("O2"))) acc_vector ()
{
  if (acc_on_device ((int) acc_device_host))
    return 0;
  else if (acc_on_device ((int) acc_device_nvidia))
    return __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);
  else
    __builtin_abort ();
}
