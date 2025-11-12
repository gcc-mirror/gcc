#include <omp.h>

#if __cplusplus
static_assert (omp_default_device < -1
	       && omp_default_device != omp_invalid_device, "");
#else
_Static_assert (omp_default_device < -1
		&& omp_default_device != omp_invalid_device, "");
#endif

static int
is_same_dev (int d1, int d2)
{
  int num_dev = omp_get_num_devices ();
  if (d1 == omp_initial_device)
    d1 = num_dev;
  if (d2 == omp_initial_device)
    d2 = num_dev;
  return (d1 == d2);
}

int
main()
{
  int dev = -99;
  int def_dev = omp_get_default_device ();
  #pragma omp target map(from: dev) device(omp_default_device)
    dev = omp_get_device_num ();

  if (!is_same_dev (def_dev, dev))
    __builtin_abort ();

  for (def_dev = omp_initial_device; def_dev <= omp_get_num_devices ();
       def_dev++)
    {
      const char* uid = omp_get_uid_from_device(def_dev);
      omp_set_default_device (def_dev);
      dev = -99;
      #pragma omp target map(from: dev) device(omp_default_device)
        dev = omp_get_device_num ();
      if (!is_same_dev (def_dev, dev))
        __builtin_abort ();

      /* Shall not modify the ICV.  */
      omp_set_default_device (omp_default_device);
      if (def_dev != omp_get_default_device ())
        __builtin_abort ();

      /* Assume the ptr and no only the string is the same.  */
      if (uid != omp_get_uid_from_device (omp_default_device))
        __builtin_abort ();
    }

  omp_set_default_device (omp_invalid_device);
  /* Shall not modify the ICV.  */
  omp_set_default_device (omp_default_device);
  if (omp_invalid_device != omp_get_default_device ())
    __builtin_abort ();
}
