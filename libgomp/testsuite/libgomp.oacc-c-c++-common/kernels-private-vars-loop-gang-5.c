/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <assert.h>

/* Test of gang-private array variable declared on loop directive, with
   broadcasting to partitioned workers.  */

int
main (int argc, char* argv[])
{
  int x[8], i, arr[32 * 32];

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    #pragma acc loop gang(num:32) private(x)
    /* { dg-note {variable 'x' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-3 } */
    for (i = 0; i < 32; i++)
      {
        for (int j = 0; j < 8; j++)
	  x[j] = j * 2;

	#pragma acc loop worker(num:32)
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += x[j % 8];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i % 8) * 2);

  return 0;
}
