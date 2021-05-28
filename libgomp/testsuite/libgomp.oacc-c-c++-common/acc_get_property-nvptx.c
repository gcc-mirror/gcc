/* Test the `acc_get_property' and '`acc_get_property_string' library
   functions on Nvidia devices by comparing property values with
   those obtained through the CUDA API. */
/* { dg-additional-sources acc_get_property-aux.c } */
/* { dg-additional-options "-lcuda -lcudart" } */
/* { dg-do run { target openacc_nvidia_accel_selected } } */
/* { dg-require-effective-target openacc_cudart } */

#include <openacc.h>
#include <cuda.h>
#include <cuda_runtime_api.h>
#include <string.h>
#include <stdio.h>

void expect_device_properties (acc_device_t dev_type, int dev_num,
			       size_t expected_memory,
			       const char* expected_vendor,
			       const char* expected_name,
			       const char* expected_driver);

int
main ()
{
  int dev_count;
  cudaGetDeviceCount (&dev_count);

  for (int dev_num = 0; dev_num < dev_count; ++dev_num)
    {
      if (cudaSetDevice (dev_num) != cudaSuccess)
	{
	  fprintf (stderr, "cudaSetDevice failed.\n");
	  abort ();
	}

      printf ("Checking device %d\n", dev_num);

      const char *vendor = "Nvidia";
      size_t free_mem;
      size_t total_mem;
      if (cudaMemGetInfo (&free_mem, &total_mem) != cudaSuccess)
	{
	  fprintf (stderr, "cudaMemGetInfo failed.\n");
	  abort ();
	}

      struct cudaDeviceProp p;
      if (cudaGetDeviceProperties (&p, dev_num) != cudaSuccess)
	{
	  fprintf (stderr, "cudaGetDeviceProperties failed.\n");
	  abort ();
	}

      int driver_version;
      if (cudaDriverGetVersion (&driver_version) != cudaSuccess)
	{
	  fprintf (stderr, "cudaDriverGetVersion failed.\n");
	  abort ();
	}
      /* The version string should contain the version of the CUDA Toolkit
	 in the same MAJOR.MINOR format that is used by Nvidia.
	 The format string below is the same that is used by the deviceQuery
	 program, which belongs to Nvidia's CUDA samples, to print the version. */
      char driver[30];
      snprintf (driver, sizeof driver, "CUDA Driver %u.%u",
		driver_version / 1000, driver_version % 1000 / 10);

      /* Note that this check relies on the fact that the device numbering
	 used by the nvptx plugin agrees with the CUDA device ordering. */
      expect_device_properties (acc_device_nvidia, dev_num,
				total_mem, vendor, p.name, driver);
    }
}
