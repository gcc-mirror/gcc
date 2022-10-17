#include <omp.h>
#include <stdlib.h>

int main (void)
{

  int host_device_num = omp_get_device_num ();

  if (host_device_num != omp_get_initial_device ())
    abort ();

  int device_num;
  int initial_device;

  for (int i = 0; i <= omp_get_num_devices (); i++)
    {
      #pragma omp target map(from: device_num, initial_device) device(i)
	{
	  initial_device = omp_is_initial_device ();
	  device_num = omp_get_device_num ();
	}

      if (i != device_num)
	abort ();

      if (initial_device && host_device_num != device_num)
	abort ();

      if (!initial_device && host_device_num == device_num)
	abort ();
    }

  return 0;
}
