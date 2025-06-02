// PR libgomp/120444

#include <omp.h>

int main()
{
  for (int dev = omp_initial_device; dev < omp_get_num_devices (); dev++)
    {
      char *ptr = (char *) omp_target_alloc (sizeof(int) * 1024, dev);

      /* Play also around with the alignment - as hsa_amd_memory_fill operates
	 on multiples of 4 bytes (uint32_t).  */

      for (int start = 0; start < 32; start++)
	for (int tail = 0; tail < 32; tail++)
	  {
	    unsigned char val = '0' + start + tail;
	    void *ptr2 = omp_target_memset (ptr + start, val,
					    1024 - start - tail, dev);
	    if (ptr + start != ptr2)
	      __builtin_abort ();

	    #pragma omp target device(dev) is_device_ptr(ptr)
	      for (int i = start; i < 1024 - start - tail; i++)
		if (ptr[i] != val)
		  __builtin_abort ();

	  }

      /* Check 'small' values for correctness.  */

      for (int start = 0; start < 32; start++)
	for (int size = 0; size <= 64 + 32; size++)
	  {
	    omp_target_memset (ptr, 'a' - 2, 1024, dev);

	    unsigned char val = '0' + start + size % 32;
	    void *ptr2 = omp_target_memset (ptr + start, val, size, dev);

	    if (ptr + start != ptr2)
	      __builtin_abort ();

	    if (size == 0)
	      continue;

	    #pragma omp target device(dev) is_device_ptr(ptr)
	    {
	      for (int i = 0; i < start; i++)
		if (ptr[i] != 'a' - 2)
		  __builtin_abort ();
	      for (int i = start; i < start + size; i++)
		if (ptr[i] != val)
		  __builtin_abort ();
	      for (int i = start + size + 1; i < 1024; i++)
		if (ptr[i] != 'a' - 2)
		  __builtin_abort ();
	    }
	  }

      omp_target_free (ptr, dev);
    }
}
