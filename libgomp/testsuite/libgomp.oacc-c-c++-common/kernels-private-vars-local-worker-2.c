/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <assert.h>

/* Test of worker-private variables declared in a local scope, broadcasting
   to vector-partitioned mode.  Successive vector loops.  */

int
main (int argc, char* argv[])
{
  int x = 5, i, arr[32 * 32 * 32];

  for (i = 0; i < 32 * 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
  {
    int j;

    #pragma acc loop gang(num:32)
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    for (i = 0; i < 32; i++)
      {
        #pragma acc loop worker(num:32)
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
	/* { dg-note {variable 'k' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
	/* { dg-note {variable 'x' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-3 } */
	for (j = 0; j < 32; j++)
	  {
	    int k;
	    int x = i ^ j * 3;

	    #pragma acc loop vector(length:32)
	    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	    
	    x = i | j * 5;
	    
	    #pragma acc loop vector(length:32)
	    /* { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
	    for (k = 0; k < 32; k++)
	      arr[i * 1024 + j * 32 + k] += x * k;
	  }
      }
  }

  for (i = 0; i < 32; i++)
    for (int j = 0; j < 32; j++)
      for (int k = 0; k < 32; k++)
        {
	  int idx = i * 1024 + j * 32 + k;
          assert (arr[idx] == idx + (i ^ j * 3) * k + (i | j * 5) * k);
	}

  return 0;
}
