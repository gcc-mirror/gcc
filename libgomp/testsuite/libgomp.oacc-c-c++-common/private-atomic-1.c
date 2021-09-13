// 'atomic' access of thread-private variable

/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <assert.h>

int main (void)
{
  int res;

  res = 0;
#pragma acc parallel reduction(+: res)
  /* { dg-note {variable 'i' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  {
#pragma acc loop vector reduction(+: res)
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'v' declared in block is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 }
       { dg-note {variable 'v' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } .-3 }
       { dg-note {variable 'v' adjusted for OpenACC privatization level: 'vector'} "" { target { ! openacc_host_selected } } .-4 } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-5 } */
    for (int i = 0; i < 2322; i++)
    {
      int v = -222;

#pragma acc loop seq
      /* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
      for (int j = 0; j < 121; ++j)
	{
#pragma acc atomic update
	  ++v;
	  /* nvptx offloading: PR83812 "operation not supported on global/shared address space".
	     { dg-output "(\n|\r\n|\r)libgomp: cuStreamSynchronize error: operation not supported on global/shared address space(\n|\r\n|\r)$" { target openacc_nvidia_accel_selected } }
	       Scan for what we expect in the "XFAILed" case (without actually XFAILing).
	     { dg-shouldfail "XFAILed" { openacc_nvidia_accel_selected } }
	       ... instead of 'dg-xfail-run-if' so that 'dg-output' is evaluated at all.
	     { dg-final { if { [dg-process-target { xfail openacc_nvidia_accel_selected }] == "F" } { xfail "[testname-for-summary] really is XFAILed" } } }
	       ... so that we still get an XFAIL visible in the log.  */
	}

      res += (v == -222 + 121);
    }
  }
  assert (res == 2322);

  return 0;
}
