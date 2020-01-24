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
  int vectorsize;

  for (ix = 0; ix < N;ix++)
    ary[ix] = -1;
  
#pragma acc parallel vector_length(32) copy(ary) copy(ondev) \
	    copyout(vectorsize)
  {
#pragma acc loop vector
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
    vectorsize = __builtin_goacc_parlevel_size (GOMP_DIM_VECTOR);
  }

  for (ix = 0; ix < N; ix++)
    {
      int expected = ix;
      if(ondev)
	{
	  int g = 0;
	  int w = 0;
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
