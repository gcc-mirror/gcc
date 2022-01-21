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
	}

      res += (v == -222 + 121);
    }
  }
  assert (res == 2322);

  return 0;
}
