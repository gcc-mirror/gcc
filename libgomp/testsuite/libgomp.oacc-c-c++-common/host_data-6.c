/* Test if, if_present clauses on host_data construct.  */

#include <assert.h>
#include <stdint.h>

void
foo (float *p, intptr_t host_p, int shared_mem_p, int cond)
{
  assert (p == (float *) host_p);

#pragma acc data copyin(host_p)
  {
#pragma acc host_data use_device(p) if_present
    /* p not mapped yet, so it will be equal to the host pointer.  */
    assert (p == (float *) host_p);

#pragma acc data copy(p[0:100])
    {
      /* Not inside a host_data construct, so p is still the host pointer.  */
      assert (p == (float *) host_p);

      if (!shared_mem_p)
      {
#pragma acc host_data use_device(p)
        /* The device address is different from the host address.  */
        assert (p != (float *) host_p);

#pragma acc host_data use_device(p) if_present
        /* p is present now, so this is the same as above.  */
        assert (p != (float *) host_p);
      }

#pragma acc host_data use_device(p) if(cond)
      /* p is the device pointer iff cond is true and device memory is
         separate from host memory.  */
      assert ((p != (float *) host_p) == (cond && !shared_mem_p));
    }
  }
}

int
main (void)
{
  float arr[100];
  int shared_mem_p = 0;
#if ACC_MEM_SHARED
  shared_mem_p = 1;
#endif
  foo (arr, (intptr_t) arr, shared_mem_p, 0);
  foo (arr, (intptr_t) arr, shared_mem_p, 1);

  return 0;
}
