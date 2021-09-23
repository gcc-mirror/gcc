/* Test if, if_present clauses on host_data construct.  */

/* { dg-additional-options "-fopt-info-all-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-all-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

/* C/C++ variant of 'libgomp.oacc-fortran/host_data-5.F90' */

#include <assert.h>
#include <stdint.h>

void
foo (float *p, intptr_t host_p, int cond)
{
  assert (p == (float *) host_p);

#pragma acc data copyin(host_p)
  {
#pragma acc host_data use_device(p) if_present
    /* { dg-note {variable 'host_p\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* p not mapped yet, so it will be equal to the host pointer.  */
    assert (p == (float *) host_p);

#pragma acc data copy(p[0:100])
    /* { dg-note {variable 'host_p\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    {
      /* Not inside a host_data construct, so p is still the host pointer.  */
      assert (p == (float *) host_p);

#pragma acc host_data use_device(p)
      /* { dg-note {variable 'host_p\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
      {
#if ACC_MEM_SHARED
	assert (p == (float *) host_p);
#else
	/* The device address is different from host address.  */
	assert (p != (float *) host_p);
#endif
      }

#pragma acc host_data use_device(p) if_present
      /* { dg-note {variable 'host_p\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
      {
#if ACC_MEM_SHARED
	assert (p == (float *) host_p);
#else
	/* p is present now, so this is the same as above.  */
	assert (p != (float *) host_p);
#endif
      }

#pragma acc host_data use_device(p) if(cond)
      /* { dg-note {variable 'host_p\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
      /* { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target { ! openacc_host_selected } } .-2 } */
      {
#if ACC_MEM_SHARED
	assert (p == (float *) host_p);
#else
	/* p is the device pointer iff cond is true.  */
	assert ((p != (float *) host_p) == cond);
#endif
      }
    }
  }
}

int
main (void)
{
  float arr[100];
  foo (arr, (intptr_t) arr, 0);
  foo (arr, (intptr_t) arr, 1);

  return 0;
}
