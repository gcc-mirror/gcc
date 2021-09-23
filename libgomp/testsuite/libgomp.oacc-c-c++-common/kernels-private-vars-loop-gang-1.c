/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <assert.h>

/* Test of gang-private variables declared on loop directive.  */

int
main (int argc, char* argv[])
{
  int x = 5, i, arr[32];

  for (i = 0; i < 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    #pragma acc loop gang(num:32) private(x)
    /* { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    for (i = 0; i < 32; i++)
      {
	x = i * 2;
	arr[i] += x;
      }
  }

  for (i = 0; i < 32; i++)
    assert (arr[i] == i * 3);

  return 0;
}
