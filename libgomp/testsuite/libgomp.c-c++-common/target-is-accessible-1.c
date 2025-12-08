#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

//#define __builtin_abort() __builtin_printf ("fail at line %d\n", __LINE__)

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int n = omp_get_num_devices ();
  int *p = (int*)malloc (sizeof (int));

  if (d < 0 || d >= n)
    d = id;

  if (!omp_target_is_accessible (p, sizeof (int), n))
    __builtin_abort ();

  if (!omp_target_is_accessible (p, sizeof (int), id))
    __builtin_abort ();

  if (!omp_target_is_accessible (p, sizeof (int), omp_initial_device))
    __builtin_abort ();

  if (omp_target_is_accessible (p, sizeof (int), -6 /* omp_default_device - 1 */))
    __builtin_abort ();

  if (omp_target_is_accessible (p, sizeof (int), n + 1))
    __builtin_abort ();

  int a[128];
  for (int d = 0; d <= omp_get_num_devices (); d++)
    {
      if (omp_target_is_accessible (NULL, 1, d))
	__builtin_abort ();

      if (omp_target_is_accessible (p, 0, d))
	__builtin_abort ();

      /* Check if libgomp is treating the device as a shared memory device.  */
      int shared_mem = 0;
      #pragma omp target map (alloc: shared_mem) device (d)
	shared_mem = 1;

      int heap_accessible = shared_mem;
      if (omp_target_is_accessible (p, sizeof (int), d) != shared_mem)
	{
	  if (shared_mem)
	    __builtin_abort ();

	  /* shared_mem is false, but the memory is reading as accessible,
	     so let's check that by reading it.  We should not do so
	     unconditionally because if it's wrong then we'll probably get
	     a memory fault.  */
	  *p = 123;
	  uintptr_t addr = (uintptr_t)p;

	  #pragma omp target is_device_ptr(p) map(from:heap_accessible) \
			     device(d)
	    {
	      if ((uintptr_t)p == addr && *p == 123)
		heap_accessible = 1;
	    }

	  if (!heap_accessible)
	    __builtin_abort ();
	}

      int stack_accessible = shared_mem;
      if (omp_target_is_accessible (a, 128 * sizeof (int), d) != shared_mem)
	{
	  if (shared_mem)
	    __builtin_abort ();

	  /* shared_mem is false, but the memory is reading as accessible,
	     so let's check that by reading it.  We should not do so
	     unconditionally because if it's wrong then we'll probably get
	     a memory fault.  */
	  int test_accessible = 123;
	  uintptr_t addr = (uintptr_t)&test_accessible;

	  #pragma omp target has_device_addr(test_accessible) \
	                     map(from:stack_accessible) device(d)
	    {
	      if ((uintptr_t)&test_accessible == addr
		  && test_accessible == 123)
		stack_accessible = 1;
	    }

	  if (!stack_accessible)
	    __builtin_abort ();
	}
      __builtin_printf ("device #%d: shared_mem=%d heap_accessible=%d "
			"stack_accessible=%d\n",
			d, shared_mem, heap_accessible, stack_accessible);

      /* omp_target_is_accessible returns false if *any* of the array is
	 inaccessible, so we only check the aggregate result.
	 (Varying access observed on amdgcn without xnack.)  */
      bool accessible = true;
      for (int i = 0; i < 128; i++)
	if (!omp_target_is_accessible (&a[i], sizeof (int), d))
	  accessible = false;
      if (accessible != (shared_mem || stack_accessible))
	__builtin_abort ();
    }

  return 0;
}
