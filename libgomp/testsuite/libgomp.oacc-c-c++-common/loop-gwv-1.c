/* { dg-additional-options "-fopt-info-note-omp" }
   { dg-additional-options "--param=openacc-privatization=noisy" }
   { dg-additional-options "-foffload=-fopt-info-note-omp" }
   { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
   for testing/documenting aspects of that functionality.  */

#include <stdio.h>
#include <openacc.h>
#include <gomp-constants.h>

#define N (32*32*32+17)
int main ()
{
  int ary[N];
  int ix;
  int exit = 0;
  int ondev = 0;
  int gangsize, workersize, vectorsize;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#pragma acc parallel num_gangs(32) num_workers(32) vector_length(32) \
	    copy(ary) copy(ondev) copyout(gangsize, workersize, vectorsize)
  /* { dg-note {variable 'ix' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
  {
#pragma acc loop gang worker vector
    /* { dg-note {variable 'ix' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 } */
    /* { dg-note {variable 'g' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 } */
    /* { dg-note {variable 'w' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-3 } */
    /* { dg-note {variable 'v' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-4 } */
    for (unsigned ix = 0; ix < N; ix++)
      {
	if (acc_on_device (acc_device_not_host))
	  {
	    int g, w, v;

	    g = __builtin_goacc_parlevel_id (GOMP_DIM_GANG);
	    w = __builtin_goacc_parlevel_id (GOMP_DIM_WORKER);
	    v = __builtin_goacc_parlevel_id (GOMP_DIM_VECTOR);

	    ary[ix] = (g << 16) | (w << 8) | v;
	    ondev = 1;
	  }
	else
	  ary[ix] = ix;
      }

    gangsize = __builtin_goacc_parlevel_size (GOMP_DIM_GANG);
    workersize = __builtin_goacc_parlevel_size (GOMP_DIM_WORKER);
    vectorsize = __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int chunk_size = (N + gangsize * workersize * vectorsize - 1)
			   / (gangsize * workersize * vectorsize);
	  
	  int g = ix / (chunk_size * workersize * vectorsize);
	  int w = (ix / vectorsize) % workersize;
	  int v = ix % vectorsize;

	  expected = (g << 16) | (w << 8) | v;
	}
      
      if (ary[ix] != expected)
	{
	  exit = 1;
	  printf ("ary[%d]=%x expected %x\n", ix, ary[ix], expected);
	}
    }
  
  return exit;
}
