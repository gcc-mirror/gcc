// PR libgomp/120444
// Async version

#include <omp.h>

int main()
{
  #pragma omp parallel for
  for (int dev = omp_initial_device; dev <= omp_get_num_devices (); dev++)
    {
      char *ptr = (char *) omp_target_alloc (sizeof(int) * 1024, dev);

      omp_depend_t dep;
      #pragma omp depobj(dep) depend(inout: ptr)

      /* Play also around with the alignment - as hsa_amd_memory_fill operates
	 on multiples of 4 bytes (uint32_t).  */

      for (int start = 0; start < 32; start++)
	for (int tail = 0; tail < 32; tail++)
	  {
	    unsigned char val = '0' + start + tail;
#if __cplusplus
	    void *ptr2 = omp_target_memset_async (ptr + start, val,
					    1024 - start - tail, dev, 0);
#else
	    void *ptr2 = omp_target_memset_async (ptr + start, val,
					    1024 - start - tail, dev, 0, nullptr);
#endif
	    if (ptr + start != ptr2)
	      __builtin_abort ();

	    #pragma omp taskwait

	    #pragma omp target device(dev) is_device_ptr(ptr) depend(depobj: dep) nowait
	      for (int i = start; i < 1024 - start - tail; i++)
		{
		  if (ptr[i] != val)
		    __builtin_abort ();
		  ptr[i] += 2;
		}

	    omp_target_memset_async (ptr + start, val + 3,
				     1024 - start - tail, dev, 1, &dep);

	    #pragma omp target device(dev) is_device_ptr(ptr) depend(depobj: dep) nowait
	      for (int i = start; i < 1024 - start - tail; i++)
		{
		  if (ptr[i] != val + 3)
		    __builtin_abort ();
		  ptr[i] += 1;
		}

	    omp_target_memset_async (ptr + start, val - 3,
				     1024 - start - tail, dev, 1, &dep);

	    #pragma omp taskwait depend (depobj: dep)
	  }
      #pragma omp depobj(dep) destroy
      omp_target_free (ptr, dev);
    }
}
