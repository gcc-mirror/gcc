/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <assert.h>

/* Test of gang-private aggregate variable declared on loop directive, with
   broadcasting to partitioned workers.  */

typedef struct {
  int x, y, z;
  int attr[13];
} vec3;

int
main (int argc, char* argv[])
{
  int i, arr[32 * 32];
  vec3 pt;

  for (i = 0; i < 32 * 32; i++)
    arr[i] = i;

  #pragma acc kernels copy(arr)
  {
    #pragma acc loop gang private(pt)
    /* { dg-note {variable 'pt' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    /* { dg-note {variable 'j' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-3 } */
    for (i = 0; i < 32; i++)
      {
        pt.x = i;
	pt.y = i * 2;
	pt.z = i * 4;
	pt.attr[5] = i * 6;

	#pragma acc loop worker
	/* { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
	for (int j = 0; j < 32; j++)
	  arr[i * 32 + j] += pt.x + pt.y + pt.z + pt.attr[5];
      }
  }

  for (i = 0; i < 32 * 32; i++)
    assert (arr[i] == i + (i / 32) * 13);

  return 0;
}
