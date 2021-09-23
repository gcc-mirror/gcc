/* Disable the acc_on_device builtin; we want to test the libgomp library
   function.  */
/* { dg-additional-options "-fno-builtin-acc_on_device" } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char *argv[])
{
  /* Host.  */

  {
    if (!acc_on_device (acc_device_none))
      abort ();
    if (!acc_on_device (acc_device_host))
      abort ();
    if (acc_on_device (acc_device_not_host))
      abort ();
    if (acc_on_device (acc_device_nvidia))
      abort ();
    if (acc_on_device (acc_device_radeon))
      abort ();
  }


  /* Host via offloading fallback mode.  */

#pragma acc parallel if(0)
  {
    if (!acc_on_device (acc_device_none))
      abort ();
    if (!acc_on_device (acc_device_host))
      abort ();
    if (acc_on_device (acc_device_not_host))
      abort ();
    if (acc_on_device (acc_device_nvidia))
      abort ();
    if (acc_on_device (acc_device_radeon))
      abort ();
  }


#if !ACC_DEVICE_TYPE_host

  /* Offloaded.  */

#pragma acc parallel
  {
    if (acc_on_device (acc_device_none))
      abort ();
    if (acc_on_device (acc_device_host))
      abort ();
    if (!acc_on_device (acc_device_not_host))
      abort ();
#if ACC_DEVICE_TYPE_nvidia
    if (!acc_on_device (acc_device_nvidia))
      abort ();
#else
    if (acc_on_device (acc_device_nvidia))
      abort ();
#endif
#if ACC_DEVICE_TYPE_radeon
    if (!acc_on_device (acc_device_radeon))
      abort ();
#else
    if (acc_on_device (acc_device_radeon))
      abort ();
#endif
  }

#endif

  return 0;
}
